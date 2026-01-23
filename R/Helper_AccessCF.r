#' @title Execute direct download calls with ncdfCF
#'
#' @description Loops over all supplied URLs assuming these query NetCDF file from open hosts. These are then registered and subsetted for variable and extent using ncdfCF before being downloaded and appended by the time axis.
#'
#' @param URLS Character. Vector of URLs for download.
#' @param variable Character. Variable name to extract from the NetCDF files.
#' @param extent Optional, A named list containg the elements `names` (indexing axes in the CFVariable to use for subsetting) and `value` (a SpatExtent to match against the axes in `names`). A spatial extent to subset the data to. Defaults to NULL returning full spatial range of data.
#' @param time Optional, POSIXct vector of length 2. A time extent to subset the data to. Defaults to NULL returning full temporal range of data.
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom ncdfCF open_ncdf
#'
#' @return A CFVariable object containing the requested data.
#'
#' @author Erik Kusch
#'
#' @examples
#' Helper_AccessCF(
#'     URLS = c("https://thredds.met.no/thredds/dodsC/nora3/1961/08/01/00/fc1961080100_003_sfx.nc", "https://thredds.met.no/thredds/dodsC/nora3/1961/08/01/06/fc1961080106_003_sfx.nc", "https://thredds.met.no/thredds/dodsC/nora3/1961/08/01/12/fc1961080112_003_sfx.nc"),
#'     variable = "T2M",
#'     extent <- list(names = c("longitude", "latitude"), value = terra::ext(c(0, 20, 60, 70)))
#' )
Helper_AccessCF <- function(URLS, variable, extent = NULL, time = NULL, verbose = TRUE) {
    ## make progress bar
    pb <- Helper_Progress(iterLength = length(URLS), text = "Downloading Data")

    ## loading data
    MetNo_cf <- as.list(rep(NA, length(URLS)))
    for (LoadIter in 1:length(URLS)) {
        # print(URLS[LoadIter])
        tryCatch(
            {
                iter_cf <- ncdfCF::open_ncdf(URLS[LoadIter])
            },
            error = function(e) {
                if (grepl("Error opening netCDF resource", e$message)) {
                    stop(paste0("Failed to open netCDF resource for URL ", URL, ". This is likely due to the query URL being misspecified or an internet connection issue."))
                } else {
                    stop(paste0("ncdfCF error accessing URL ", URL, " : ", e$message))
                }
            }
        )

        ## extracting relevant variable
        # print(iter_cf)
        iter_cf <- iter_cf[[variable]]
        if (!is.null(extent)) {
            iter_cf <- eval(parse(text = paste0(
                "iter_cf$subset(",
                extent$names[1], " = c(", paste(extent$value[1:2], collapse = ","), "), ",
                extent$names[2], " = c(", paste(extent$value[3:4], collapse = ","), ")",
                ")"
            )))
        }

        if (!is.null(time)) {
            iter_cf <- iter_cf$subset(
                T = time
            )
        }

        if (verbose) {
            pb$tick(tokens = list(layer = LoadIter))
        }
        iter_cf$detach() # Make sure we grab the data
        MetNo_cf[[LoadIter]] <- iter_cf
    }

    ## appending along time
    Return_cf <- MetNo_cf[[1]]
    for (i in 2:length(MetNo_cf)) {
        Return_cf$append(MetNo_cf[[i]], "time")
    }

    ## returning data
    return(Return_cf)
}
