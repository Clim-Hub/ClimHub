### TEMPORAL DECUMULATION =======================================================
#' Carry out decumulation of cumulatively stored data
#'
#' Takes a SpatRaster and user-specifications of temporal cumulation and reverses the cumulative measurements into indvidual ones. Currently conceptualised to decumulate hourly CDS records.
#'
#' @param Raster A SpatRaster within which coverage should be identified
#' @param Interval Number of layers in the raster belonging to the same interval of cumulation.
#' @param Mode Character. Mode of cumulative storage of values. Currently supported: CDS (first layer per day is the cumulative sum of the previous day).
#' @param Cores Integer. Number of cores for parallelisation if desired.
#'
#' @importFrom terra as.data.frame
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom snow stopCluster
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom progress progress_bar
#' @importFrom terra nlyr
#'
#' @return A SpatRaster
#' @examples
#' @export
Temporal.Decumulation <- function(Raster, Interval, Mode, Cores = 1) {
    ## Make Raster into data.frame
    message("Turning Spatial Data into DataFrame")
    df <- as.data.frame(Raster)

    ## Limit data according to cumulation mode
    if (Mode == "CDS") {
        df <- df[, c(-1, -(ncol(df) - 22):-ncol(df))] # remove first and last 23 layers
        Out <- Raster[[-(nlyr(Raster) - 23):-nlyr(Raster)]]
    }
    if (Mode != "CDS") {
        stop("Only CDS cumulation is currently supported as the Mode argument.")
    }

    ## progress bar
    pb <- progress_bar$new(
        format = "Decumulation (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
        total = nrow(df), # 100
        width = getOption("width"),
        clear = FALSE
    )
    progressIter <- 1:nrow(df) # token reported in progress bar

    ## cluster opening
    if (Cores > 1) {
        cl <- makeCluster(Cores)
        on.exit(snow::stopCluster(cl))
        doSNOW::registerDoSNOW(cl)
        progress <- function(n) {
            pb$tick(tokens = list(layer = progressIter[n]))
        }
        ForeachObjects <- c("df", "Interval")
    }

    ## iteration code
    looptext <- "
    row <- as.numeric(df[CumulIter, ])

    # Split the row into groups of size `Interval`
    groups <- base::split(
        x = row,
        f = as.factor(ceiling(seq_along(row) / Interval))
    )
    # Decumulate each group
    decumulated_groups <- lapply(groups, function(group) {
        c(group[1], diff(group)) # First value stays as is; subsequent values are differences
    })
    # Combine all groups back into a single row
    list(Iter = unlist(decumulated_groups))
    "

    ## Make downloads
    message("Decumulation")
    if (Cores > 1) {
        Decumulls <- foreach(
            CumulIter = 1:nrow(df),
            # .packages = c("httr"),
            .export = ForeachObjects,
            .options.snow = list(progress = progress)
        ) %dopar% { # parallel loop'
            eval(parse(text = looptext))
        } # end of parallel loop
    } else {
        Decumulls <- list("NA" = NA)
        for (CumulIter in 1:nrow(df)) {
            Fret <- eval(parse(text = looptext)) # evaluate the kriging specification per layer
            Decumulls <- c(Decumulls, Fret)
            pb$tick(tokens = list(layer = progressIter[CumulIter]))
        }
        Decumulls <- Decumulls[[-1]]
    }

    ## Reassing values to output and return it
    message("Reassigning data to Raster")
    decumulated_df <- do.call(rbind, lapply(Decumulls, unlist))
    values(Out) <- decumulated_df
    return(Out)
}
