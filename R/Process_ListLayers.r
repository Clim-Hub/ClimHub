#' @title Layer-wise application on SpatRasters in a list
#'
#' @description Loops over layers of SpatRasters within supplied list object and calculates one resulting layer per layer-group across SpatRasters in accordance with supplied fun argument.
#'
#' @param ls List. List of SpatRasters whose layers to compute with each other. All SpatRasters need to have the same layer count.
#' @param fun Function. Function by which to summarise/compute SpatRaster layers with each other.
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra nlyr
#' @importFrom terra time
#'
#' @return A SpatRaster.
#'
#' @author Erik Kusch
#'
#' @examples
#' ## load the first 2 days of the KiN temperature data provided with ClimHub to two separate list slots and articifially increase values by 1 for the second list item
#' ls <- list(
#'     Base = terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1:2]],
#'     Base1 = terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1:2]] + 1
#' )
#' ## calculate difference between each layer in the two list slots
#' Diff_rast <- Process_ListLayers(ls = ls, fun = diff)
#' Diff_rast # all values in this 2-layer raster, ought to be 1 or NA
#'
#' @export
Process_ListLayers <- function(ls, fun = mean, verbose = TRUE) {
    ## Checks
    LayerCount <- unique(unlist(lapply(ls, terra::nlyr)))
    if (length(LayerCount) != 1) {
        stop("The rasters contained in your supplied list contain different numbers of layers.")
    }
    # if (is.function(fun)) {
    #     stop("You have not specified a function for the fun argument.")
    # }

    ## progress bar
    pb <- Helper_Progress(iterLength = LayerCount, text = "List-Layer Handling")

    ## Make layer-wise application; does not support multi-core execution as described here: https://github.com/rspatial/terra/issues/36
    NewLayers <- list() # Use a list for better performance than concatenation
    for (AppIter in 1:LayerCount) {
        # Perform the terra::app operation directly without constructing strings
        Fret <- terra::app(rast(lapply(ls, "[[", AppIter)), fun = fun)
        NewLayers[[AppIter]] <- Fret # Append result to list
        if (verbose) {
            pb$tick(tokens = list(layer = AppIter))
        }
    }
    NewLayers <- rast(NewLayers)

    ## Assign time component of first raster in list to final raster
    terra::time(NewLayers) <- terra::time(ls[[1]])

    ## return file names
    return(NewLayers)
}
