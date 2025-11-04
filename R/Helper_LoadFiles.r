#' @title Load list of NetCDF files and stack them into a SpatRaster
#'
#' @description Iterates of a number of filenames, pointing to NetCDFs, and loads them into one SpatRaster.
#'
#' @param fileName Character. Vector of absolute filenames to be loaded.
#' @param dates POSIXct. Optional vector of times to apply to list-loaded data
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra nlyr
#'
#' @return A spatraster object.
#' 
#' @author Erik Kusch
#' 
#' @examples
#' Helper_LoadFiles(
#'    fileName = c(
#'        system.file("extdata", "KiN_rast.nc", package = "ClimHub"), 
#'        system.file("extdata", "KiN_rast.nc", package = "ClimHub")
#'        )
#'    )
Helper_LoadFiles <- function(fileName, dates = NULL, verbose = TRUE) {
    ## make progress bar
    pb <- Helper_Progress(iterLength = length(fileName), text = "Loading Files")
    # pb <- progress_bar$new(
    #     format = "Loading from disk (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
    #     total = length(FNames), # 100
    #     width = getOption("width"),
    #     clear = FALSE
    # )
    # progressIter <- 1:length(FNames) # token reported in progress bar

    ## loading data
    MetNo_rast <- as.list(rep(NA, length(fileName)))
    for (LoadIter in 1:length(fileName)) {
        MetNo_rast[[LoadIter]] <- terra::rast(file.path(fileName[LoadIter]))
        if (!is.null(dates)) {
            terra::time(MetNo_rast[[LoadIter]]) <- rep(dates[LoadIter], terra::nlyr(MetNo_rast[[LoadIter]]))
        }
        if(verbose){pb$tick(tokens = list(layer = LoadIter))}
    }

    ## returning data
    MetNo_rast <- do.call(c, MetNo_rast)
    return(MetNo_rast)
}