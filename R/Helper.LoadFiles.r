### LOADING MULTIPLE FILES FROM DRIVE AND MAKE SPATRASTER ========================================================
#' Load list of NetCDF files and stack them into a SpatRaster
#'
#' Iterates of a number of filenames, pointing to NetCDFs, and loads them into one SpatRaster.
#'
#' @param FNames Character. Vector of absolute filenames to be loaded.
#' @param TimeAssign POSIXct. Optional vector of times to apply to list-loaded data
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra nlyr
#'
#' @return A spatraster object.
#' 
#' @examples
#' Helper.LoadFiles(
#'    FNames = c(
#'        system.file("extdata", "KiN_AT.nc", package = "ClimHub"), 
#'        system.file("extdata", "KiN_PR.nc", package = "ClimHub")
#'        )
#'    )
#' @export
Helper.LoadFiles <- function(FNames, TimeAssign = NULL, verbose = TRUE) {
    ## make progress bar
    pb <- Helper.Progress(IterLength = length(FNames), Text = "Loading Files")
    # pb <- progress_bar$new(
    #     format = "Loading from disk (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
    #     total = length(FNames), # 100
    #     width = getOption("width"),
    #     clear = FALSE
    # )
    # progressIter <- 1:length(FNames) # token reported in progress bar

    ## loading data
    MetNo_rast <- as.list(rep(NA, length(FNames)))
    for (LoadIter in 1:length(FNames)) {
        MetNo_rast[[LoadIter]] <- terra::rast(file.path(FNames[LoadIter]))
        if (!is.null(TimeAssign)) {
            terra::time(MetNo_rast[[LoadIter]]) <- rep(TimeAssign[LoadIter], terra::nlyr(MetNo_rast[[LoadIter]]))
        }
        if(verbose){pb$tick(tokens = list(layer = LoadIter))}
    }

    ## retunring data
    MetNo_rast <- do.call(c, MetNo_rast)
    return(MetNo_rast)
}