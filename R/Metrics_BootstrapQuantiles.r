#' @title Bootstrap Quantile Lookup for Daily Time Series
#'
#' @description Computes quantiles across years for each day‑of‑year using a rolling bootstrap window to be used in `Metrics_ETTCDI` calculation of ETCCDI. Internally the function uses \code{Helper_ETCCDIDailyBootstraps()} to determine which dates belong in the window centred on each calendar day (assuming a leap year for consistency).
#'
#' This is primarily used by the ETCCDI metric code to obtain base period thresholds but can be called directly by users. Dates in the input `CFVariable` must present individual days.
#'
#' @param CFVariable `CFVariable` object for which quantiles for each bootstrap day interval ought to be calculated.
#' @param probs Numeric vector of probabilities (between 0 and 1) for which quantiles will be returned.  Default is \code{c(0.1,0.9)}.
#' @param bootstrapWindow Odd integer giving the width of the window in days used for the bootstrap sample around each target date. The window is centred on the day of year of interest.  Defaults to 5.
#'
#' @importFrom ncdfCF as_CF
#' @importFrom ncdfCF create_ncdf
#'
#' @return A numeric array with dimensions \code{lon × lat × dayOfYear × probs}.  The third dimension has 366 levels ("01‑01" through "12‑31", including Feb 29) and the fourth dimension is named by the supplied \code{probs}.  Values are the requested quantiles computed over the set of years that fall inside the bootstrap window.  If no valid observations exist for a given cell/day the result is NA.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' TX <- NC_Read("inst/extdata/KiN_tx.nc")[["tasmax"]]
#' TX_Quant <- Metrics_BootstrapQuantiles(CFVariable = TX, probs = c(0.1, 0.9), bootstrapWindow = 5)
#' TX_Quant
#' }
#' @export
Metrics_BootstrapQuantiles <- function(
    CFVariable,
    probs = c(0.1, 0.9),
    bootstrapWindow = 5) {
    ## transform input for internal use
    data_array <- CFVariable$raw()
    dates <- as.Date(dimnames(data_array)[[3]])

    ## input checks
    if (!is.numeric(data_array) || length(dim(data_array)) != 3) {
        stop("data_array must be a numeric array with three dimensions")
    }
    if (any(probs < 0 | probs > 1)) {
        stop("probs must be between 0 and 1")
    }

    ## extract years from the dates for later use (leap-year detection)
    years <- sort(unique(as.integer(format(dates, "%Y"))))

    ## create grouping information
    group_ls <- Helper_ETCCDIDailyBootstraps(dates, bootstrapWindow = bootstrapWindow)
    nday <- length(group_ls) # 366
    nl <- dim(data_array)[1]
    nt <- dim(data_array)[2]
    nprob <- length(probs)

    ## construct a time dimension that reflects a leap year; use the first leap year found in the input dates if present, otherwise fall back to 2000
    leap_years <- years[years %% 4 == 0 & (years %% 100 != 0 | years %% 400 == 0)]
    if (length(leap_years) > 0) {
        base_year <- leap_years[1]
    } else {
        base_year <- 2000
    }
    time_dim <- paste0(base_year, "-", names(group_ls), "T00:00:00")

    ## create empty array which is to be filled (third dim now actual timestamps)
    out <- array(NA_real_,
        dim = c(nl, nt, nday, nprob),
        dimnames = list(
            dimnames(data_array)[[1]],
            dimnames(data_array)[[2]],
            time_dim,
            as.character(probs)
        )
    )

    ## progress bar
    pb <- Helper_Progress(iterLength = nday, text = "Calculating Quantiles")

    ## iterate over the 366 days
    for (doy in seq_len(nday)) {
        ## select data for the relevant days in the base period
        idx <- match(group_ls[[doy]], dates)
        idx <- idx[!is.na(idx)]
        if (length(idx) == 0) next
        slice <- data_array[, , idx, drop = FALSE] # lon × lat × nobs

        ## iterate over locations and calculate quantiles
        qmat <- apply(slice, 1:2, function(v) {
            if (all(is.na(v))) {
                rep(NA_real_, nprob)
            } else {
                as.numeric(stats::quantile(v,
                    probs = probs,
                    na.rm = TRUE, type = 7
                ))
            }
        })
        dim(qmat) <- c(nprob, nl, nt)

        ## assign quantiles to pre-defined array
        for (ip in seq_len(nprob)) {
            out[, , doy, ip] <- qmat[ip, , ]
        }

        ## update progress bar
        pb$tick(tokens = list(layer = doy))
    }

    ## convert each probability slice into its own CFVariable and collect in a CFDataset
    # create CFDataset for reporting back
    ds <- ncdfCF::create_ncdf()

    # attributes to copy (exclude actual_range since values have changed)
    atts <- CFVariable$attributes[CFVariable$attributes$name != "actual_range", ]

    for (ip in seq_len(nprob)) {
        prob <- probs[ip]
        slice <- out[, , , ip, drop = TRUE] # lon × lat × time
        names(dimnames(slice)) <- names(dimnames(data_array))

        # variable name uses probability so user can see it later
        varname <- paste0("quantile_", gsub("\\.", "p", as.character(prob)))
        new_var <- ncdfCF::as_CF(varname, slice)

        # copy attributes
        apply(atts, 1, function(a) new_var$set_attribute(a$name, a$type, a$value))
        
        ## add data
        ds$add_variable(new_var)
    }

    ## return CFDataset
    ds
}
