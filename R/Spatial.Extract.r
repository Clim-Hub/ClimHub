### EXTRACTING DATA IN SPACE =========================================================
#' Extracting data from SpatRaster with a SpatialFeaturesObject
#'
#' Extract data from SpatRaster corresponding to spatial features in SF object.
#'
#' @param Raster A SpatRaster within which coverage should be identified
#' @param SF Either an sf polygon(-collection) or an sf point(-collection).
#' @param FUN User-defined function by which to aggregate values of cells within a polygon. Supported functions are mean, sum, min, max and table.
#'
#' @importFrom terra extract
#' @importFrom terra crs
#' @importFrom sf st_crs
#' @importFrom terra time
#'
#' @return A data.frame.
#'
#' @examples
#' Data_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' data(Jotunheimen_sf)
#' Spatial.Extract(
#'     Raster = Data_rast,
#'     SF = Jotunheimen_sf,
#'     FUN = mean
#' )
#' data(Nor2K_sf)
#' Spatial.Extract(
#'     Raster = Data_rast,
#'     SF = Nor2K_sf
#' )
#' @export
Spatial.Extract <- function(Raster, SF, FUN = mean) {
    ## preparing extraction
    if (terra::crs(Raster) != st_crs(SF)$wkt) {
        SF <- Spatial.Reproject(SF, Raster)
        warning("Had to reproject your SF object to align with the Raster object CRS. This was done automatically, but I recommend doing so yourself and investigating the reprojected objects.")
    }

    ## actual extraction
    Extracted_df <- terra::extract(x = Raster, y = SF, fun = FUN, exact = TRUE, na.rm = TRUE)

    ## column names as dates/times
    colnames(Extracted_df)[-1] <- as.character(terra::time(Raster))

    ## return
    return(Extracted_df)
}
