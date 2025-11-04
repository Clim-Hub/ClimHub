#' @title Extracting data from SpatRaster with a SpatialFeaturesObject
#'
#' @description Extract data from SpatRaster corresponding to spatial features in sf object.
#'
#' @param spatRaster A SpatRaster within which coverage should be identified
#' @param sf Either an sf polygon(-collection) or an sf point(-collection).
#' @param fun User-defined function by which to aggregate values of cells within a polygon. Supported functions are mean, sum, min, max and table.
#'
#' @importFrom terra extract
#' @importFrom terra crs
#' @importFrom sf st_crs
#' @importFrom terra time
#'
#' @return A data.frame.
#'
#' @author Erik Kusch
#'
#' @examples
#' Data_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' data(Jotunheimen_sf)
#' Spatial_Extract(
#'     spatRaster = Data_rast,
#'     sf = Jotunheimen_sf,
#'     fun = mean
#' )
#' data(Nor2K_sf)
#' Spatial_Extract(
#'     spatRaster = Data_rast,
#'     sf = Nor2K_sf
#' )
#' @export
Spatial_Extract <- function(spatRaster, sf, fun = mean) {
    ## preparing extraction
    if (terra::crs(spatRaster) != st_crs(sf)$wkt) {
        sf <- Spatial_Reproject(sf, spatRaster)
        warning("Had to reproject your sf object to align with the spatRaster object CRS. This was done automatically, but I recommend doing so yourself and investigating the reprojected objects.")
    }

    ## actual extraction
    Extracted_df <- terra::extract(x = spatRaster, y = sf, fun = fun, exact = TRUE, na.rm = TRUE)

    ## column names as dates/times
    colnames(Extracted_df)[-1] <- as.character(terra::time(spatRaster))

    ## return
    return(Extracted_df)
}
