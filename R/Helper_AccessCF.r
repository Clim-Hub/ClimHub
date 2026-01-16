#' @title Execute direct download calls with ncdfCF
#'
#' @description Loops over all supplied URLs assuming these query NetCDF file from open hosts. These are then registered and subsetted for variable and extent using ncdfCF before being downloaded and appended by the time axis.
#'
#' @param URLS Character. Vector of URLs for download.
#' @param variable Character. Variable name to extract from the NetCDF files.
#' @param extent Optional, SpatExtent. A spatial extent to subset the data to. Defaults to NULL returning full spatial range of data.
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
#'     extent <- terra::ext(c(0, 40, 50, 60))
#' )
Helper_AccessCF <- function(URLS, variable, extent = NULL, verbose = TRUE) {
    ## make progress bar
    pb <- Helper_Progress(iterLength = length(URLS), text = "Downloading Data")

    ## loading data
    MetNo_cf <- as.list(rep(NA, length(URLS)))
    for (LoadIter in 1:length(URLS)) {
        iter_cf <- ncdfCF::open_ncdf(URLS[LoadIter])
        ## extracting relevant variable
        # print(iter_cf)
        iter_cf <- iter_cf[[variable]]
        if (!is.null(extent)) {
            iter_cf <- iter_cf$subset(
                longitude = c(extent[1], extent[2]),
                latitude = c(extent[3], extent[4])
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
