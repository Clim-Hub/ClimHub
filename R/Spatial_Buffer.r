#' @title Square Buffers Around Point Data
#'
#' @description Allow for drawing of buffer zones around point-location data for downloading and kriging of spatial data around point-locations. Overlapping individual buffers are merged.
#'
#' @param sf An sf POINT object
#' @param buffer Size of buffer (unit dependant on projection of input SF).
#'
#' @importFrom sf st_buffer
#' @importFrom sf st_union
#' @importFrom sf st_as_sf
#'
#' @return An sf polygon made up of individual square buffers around point-location input.
#'
#' @author Erik Kusch
#'
#' @examples
#' data(Nor2K_sf)
#' Spatial.Buffer(Nor2K_sf, Buffer = 1e3) # 1km buffer
#' @export
Spatial_Buffer <- function(sf, buffer = 1e3) {
    st_as_sf(st_union(st_buffer(sf, buffer, endCapStyle = "SQUARE")))
}
