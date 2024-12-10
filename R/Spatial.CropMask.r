### CROPPING & MASKING =========================================================
#' Cropping & Range Masking with Edge Support
#'
#' Cropped and masking the original SpatRaster (`Raster`) using supplied SpatExtent or shapefile (`Shape`) and retaining all pixels which are even just partially covered.
#'
#' @param Raster A SpatRaster within which coverage should be identified
#' @param Shape Either a SPatExtent or an sf polygon(-collection) whose coverage of the raster object is to be found.
#'
#' @importFrom terra crop
#' @importFrom terra ext
#' @importFrom terra mask
#' @importFrom terra nlyr
#' @importFrom pbapply pblapply
#'
#' @return A SpatRaster.
#'
#' @examples
#' @export
Spatial.CropMask <- function(Raster, Shape) {
    ## splitting by rasterlayers if necessary to avoid error reported in https://github.com/rspatial/terra/issues/1556
    if (terra::nlyr(Raster) > 65535) {
        Indices <- ceiling((1:terra::nlyr(Raster)) / 2e4)
        r_ls <- terra::split(x = Raster, f = Indices)
        ret_ls <- pblapply(r_ls, FUN = function(Raster_iter) {
            ret_rast <- crop(Raster_iter, ext(Shape))
            if (class(Shape)[1] == "sf") {
                ret_rast <- mask(ret_rast, Shape, touches = TRUE)
            }
            ret_rast
        })
        ret_rast <- do.call(c, ret_ls)
        return(ret_rast)
    }

    ## regular cropping and masking for SPatRasters not exceeding layer limit
    ret_rast <- crop(Raster, ext(Shape))
    if (class(Shape)[1] == "sf") {
        ret_rast <- mask(ret_rast, Shape, touches = TRUE)
    }
    return(ret_rast)
}
