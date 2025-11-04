#' @title Cropping & Range Masking with Edge Support
#'
#' @description Cropped and masking the original SpatRaster (`spatRaster`) using supplied SpatExtent or shapefile (`shape`) and retaining all pixels which are even just partially covered.
#'
#' @param spatRaster A SpatRaster within which coverage should be identified
#' @param shape Either a SpatExtent or an sf polygon(-collection) whose coverage of the raster object is to be found.
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra crop
#' @importFrom terra ext
#' @importFrom terra mask
#' @importFrom terra nlyr
#'
#' @return A SpatRaster.
#'
#' @author Erik Kusch
#'
#' @examples
#' Data_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' data(Jotunheimen_sf)
#' Data_rast <- Spatial_Reproject(Data_rast, Jotunheimen_sf)
#' Spatial_Limit(Data_rast, Jotunheimen_sf)
#' @export
Spatial_Limit <- function(spatRaster, shape, verbose = TRUE) {
    ## progress bar
    pb <- Helper_Progress(iterLength = nlyr(spatRaster), text = "Cropping/Masking Layers")

    ## going layer by layer to avoid error reported in https://github.com/rspatial/terra/issues/1556
    CroppedLayers_ls <- lapply(1:nlyr(spatRaster), FUN = function(Iter) {
        ret_rast <- crop(spatRaster[[Iter]], ext(shape))
        if (class(shape)[1] == "sf") {
            ret_rast <- mask(ret_rast, shape, touches = TRUE)
        }
        if (verbose) {
            pb$tick(tokens = list(layer = Iter))
        }
        ret_rast
    })

    ## return fused layers as SpatRaster
    return(do.call(c, CroppedLayers_ls))
}
