### POINT BUFFERING ============================================================
#' Square Buffers Around Point Data
#'
#' Allow for drawing of buffer zones around point-location data for downloading and kriging of spatial data around point-locations. Overlapping individual buffers are merged.
#'
#' @param SF An sf POINT object
#' @param Buffer Size of buffer in km.
#'
#' @importFrom sf st_buffer
#' @importFrom sf st_union
#' @importFrom sf st_as_sf
#'
#' @return An sf polygon made up of individual square buffers around point-location input.
#'
#' @examples
#' @export
Spatial.Buffer <- function(SF, Buffer = 1e3) {
    st_as_sf(st_union(st_buffer(SF, Buffer, endCapStyle = "SQUARE")))
}
