#' @title Transform data frame-type inputs into sf
#'
#' @description Transform data frame with ID for querying functionality around point-loactions to SpatialPoints
#'
#' @param dataFrame A data.frame containing geo-referenced points with Lat and Lon columns
#' @param crs Numeric. EPSG CRS number. See [here](https://epsg.io/) for more information
#'
#' @importFrom sf st_as_sf
#'
#' @author Erik Kusch
#'
#' @return An sf POINT object.
#'
#' @examples
#' data(Nor2K_sf)
#' df <- cbind(sf::st_drop_geometry(Nor2K_sf), sf::st_coordinates(Nor2K_sf))
#' colnames(df)[5:6] <- c("Lon", "Lat")
#' Spatial_MakePoints(df, sf::st_crs(Nor2K_sf))
#' @export
Spatial_MakePoints <- function(dataFrame, crs = 4326) {
    dataFrame <- data.frame(dataFrame) ## attempt to catch tibbles or data.tables
    if (sum(c("Lat", "Lon") %in% colnames(dataFrame)) != 2) {
        stop("Please provide your geo-locations with a Lat and a Lon column (named exactly like such).")
    }
    st_as_sf(dataFrame, coords = c("Lon", "Lat"), crs = crs, remove = FALSE)
}
