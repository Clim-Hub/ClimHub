### POINT LOCATIONS ============================================================
#' Transform data frame-type inputs into sf
#'
#' Transform data frame with ID for querying functionality around point-loactions to SpatialPoints
#'
#' @param DataFrame A data.frame containing geo-referenced points with Lat and Lon columns
#' @param CRS Numeric. EPSG CRS number. See [here](https://epsg.io/) for more information
#'
#' @importFrom sf st_as_sf
#'
#' @return An sf POINT object.
#'
#' @examples
#' @export
Spatial.MakePoints <- function(DataFrame, CRS = 4326) {
    DataFrame <- data.frame(DataFrame) ## attempt to catch tibbles or data.tables
    if (sum(c("Lat", "Lon") %in% colnames(DataFrame)) != 2) {
        stop("Please provide your geo-locations with a Lat and a Lon column (named exactly like such).")
    }
    st_as_sf(DataFrame, coords = c("Lon", "Lat"), crs = CRS, remove = FALSE)
}
