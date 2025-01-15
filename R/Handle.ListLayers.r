### LAYER-WISE OPERATION ACROSS SPATRASTERS ========================================================
#' Layer-wise application on SpatRasters in a list
#'
#' Loops over layers of SpatRasters within supplied list object and calculates one resulting layer per layer-group across SpatRasters in accordance with supplied FUN argument.
#'
#' @param Rast_ls List. List of SpatRasters whose layers to compute with each other. All SpatRasters need to have the same layer count.
#' @param FUN Function. Function by which to summarise/compute SpatRaster layers with each other.
#'
#' @importFrom terra nlyr
#' @importFrom terra time
#' @importFrom progress progress_bar
#'
#' @return A SpatRaster.
#'
#' @examples
#' Rast1 <- Download.KlimaiNorge2100(
#'     Variable = "Mean Air Temperature",
#'     DateStart = "1990-01-01",
#'     DateStop = "1990-12-31",
#'     Model = "CNRM_CCLM",
#'     Scenario = "rcp85",
#'     Cores = 1,
#'     Dir = getwd(),
#'     FileName = "CNRM_CCLM",
#'     Compression = NA,
#'     RemoveTemporary = TRUE
#' )
#'
#' Rast2 <- Download.KlimaiNorge2100(
#'     Variable = "Mean Air Temperature",
#'     DateStart = "1990-01-01",
#'     DateStop = "1990-12-31",
#'     Model = "CNRM_RCA",
#'     Scenario = "rcp85",
#'     Cores = 1,
#'     Dir = getwd(),
#'     FileName = "CNRM_RCA",
#'     Compression = NA,
#'     RemoveTemporary = TRUE
#' )
#'
#' Handle.ListLayers(Rast_ls = list(Rast1, Rast2), FUN = mean)
#'
#' @export
Handle.ListLayers <- function(Rast_ls, FUN = mean) {
    ## Checks
    LayerCount <- unique(unlist(lapply(Rast_ls, terra::nlyr)))
    if (length(LayerCount) != 1) {
        stop("The rasters contained in your supplied list contain different numbers of layers.")
    }
    # if (is.function(FUN)) {
    #     stop("You have not specified a function for the FUN argument.")
    # }

    ## progress bar
    pb <- progress_bar$new(
        format = "List-Layer Handling (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
        total = LayerCount,
        width = getOption("width"),
        clear = FALSE
    )
    progressIter <- 1:LayerCount # token reported in progress bar

    ## Make layer-wise application; does not support multi-core execution as described here: https://github.com/rspatial/terra/issues/36
    NewLayers <- list() # Use a list for better performance than concatenation
    for (AppIter in 1:LayerCount) {
        # Perform the terra::app operation directly without constructing strings
        Fret <- terra::app(do.call(c, lapply(Rast_ls, "[[", AppIter)), fun = FUN)
        NewLayers[[AppIter]] <- Fret # Append result to list
        pb$tick(tokens = list(layer = progressIter[AppIter]))
    }
    NewLayers <- do.call(c, NewLayers)

    ## Assign time component of first raster in list to final raster
    terra::time(NewLayers) <- terra::time(Rast_ls[[1]])

    ## return file names
    return(NewLayers)
}
