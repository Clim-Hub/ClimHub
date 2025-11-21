#' @title Read a netCDF file
#'
#' @description Open and read a netCDF file including its metadata. Data is
#'   returned as a `ncdfCF` object.
#'
#' @param resource Character. Fully qualified file name for a netCDF file on a
#'   local file system or the URL to a netCDF resource on a THREDDS server.
#'
#' @return A `ncdfCF` data set instance.
#' @author Patrick Van Laake
#' @examples
#' ds <- NC_Read(resource = system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' @export
NC_Read <- function(resource) {
  ncdfCF::open_ncdf(resource)
}
