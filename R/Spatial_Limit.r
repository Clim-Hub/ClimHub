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
#' @seealso \code{\link{Spatial_Reproject}}.
#'
#' @examples
#' # single-layer exaples
#' ## limit by sf object
#' KiN_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1]]
#' data(Jotunheimen_sf)
#' # Kin_Limited <- Spatial_Limit(KiN_rast, Jotunheimen_sf) # this would throw an error due to different projections of the spatRaster and the sf object
#' KiN_rast <- Spatial_Reproject(KiN_rast, Jotunheimen_sf)
#' KiN_Limited <- Spatial_Limit(spatRaster = KiN_rast, shape = Jotunheimen_sf)
#' # Viz_SpatRast(KiN_Limited)
#'
#' ## limit by SpatExtent
#' KiN_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1]]
#' KiN_Limited <- Spatial_Limit(spatRaster = KiN_rast, shape = terra::ext(-7e4, 3e5, 6.5e6, 7e6))
#' # Viz_SpatRast(KiN_Limited)
#'
#' # multi-layer exaple
#' KiN_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' KiN_Limited <- Spatial_Limit(spatRaster = KiN_rast, shape = terra::ext(-7e4, 3e5, 6.5e6, 7e6))
#' # Viz_SpatRast(KiN_Limited, panelColumns = 5)
#' @export
Spatial_Limit <- function(spatRaster, shape, verbose = TRUE) {
    ## check that shape is specified correctly
    class_name <- class(shape)[1]
    if (!(class_name %in% c("sf", "SpatExtent"))) {
        stop("You have misspecified your shape argument. It must be an sf object or SpatExtent.")
    }

    ## check that cropping/masking can even happen
    if (class_name == "sf") {
        if (!identical(as.numeric(terra::crs(spatRaster, describe = TRUE)$code), as.numeric(st_crs(shape)$epsg))) {
            stop("The CRS EPSG Code of the spatRast and shape are not identical. You need to reproject one of these objects first. You can do so with the Spatial_Reproject function.")
        }
    }
    if (class_name == "SpatExtent") {
        if (
            any(
                c(
                    terra::ext(spatRaster)[c(1, 3)] > shape[c(1, 3)],
                    terra::ext(spatRaster)[c(2, 4)] < shape[c(2, 4)]
                )
            )
        ) {
            stop(
                "The SpatExtent you have specified in the shape argument extends beyond the limits of the SpatRaster provided via the spatRaster argument. The extent of this object (and maximum extent for Spatial_limit operations on it is:\n",
                paste0("xmin: ", terra::ext(spatRaster)[1], ", xmax: ", terra::ext(spatRaster)[2], ", ymin: ", terra::ext(spatRaster)[3], ", ymax: ", terra::ext(spatRaster)[4]),
                "\nPlease provide a SpatExtent that is within these limits."
            )
        }
    }

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
