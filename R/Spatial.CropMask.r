### CROPPING & MASKING =========================================================
#' Cropping & Range Masking with Edge Support
#'
#' Cropped and masking the original SpatRaster (`Raster`) using supplied SpatExtent or shapefile (`Shape`) and retaining all pixels which are even just partially covered.
#'
#' @param Raster A SpatRaster within which coverage should be identified
#' @param Shape Either a SPatExtent or an sf polygon(-collection) whose coverage of the raster object is to be found.
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra crop
#' @importFrom terra ext
#' @importFrom terra mask
#' @importFrom terra nlyr
#'
#' @return A SpatRaster.
#'
#' @examples
#' Data_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1:31]]
#' data(Jotunheimen_sf)
#' Data_rast <- Spatial.Reproject(Data_rast, Jotunheimen_sf)
#' Spatial.CropMask(Data_rast, Jotunheimen_sf)
#' @export
Spatial.CropMask <- function(Raster, Shape, verbose = TRUE) {
    ## progress bar
    pb <- Helper.Progress(IterLength = nlyr(Raster), Text = "Cropping/Masking Layers")

    ## going layer by layer to avoid error reported in https://github.com/rspatial/terra/issues/1556
    CroppedLayers_ls <- lapply(1:nlyr(Raster), FUN = function(Iter){
        ret_rast <- crop(Raster[[Iter]], ext(Shape))
        if (class(Shape)[1] == "sf") {
            ret_rast <- mask(ret_rast, Shape, touches = TRUE)
        }
        if(verbose){pb$tick(tokens = list(layer = Iter))}
        ret_rast
    })

    ## return fused layers as SpatRaster
    return(do.call(c, CroppedLayers_ls))
}
