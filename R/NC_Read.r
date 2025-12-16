#' @title Read a netCDF file
#'
#' @description Open and read a netCDF file including its metadata.
#'
#' @param fileName Character. Fully qualified file name for a netCDF file on a local file system or the URL to a netCDF resource on a THREDDS server.
#' @param asTerra Logical. If `TRUE`, the contents of the netCDF file is returned as a `SpatRaster` object from the `terra` package. Note that metadata support in `terra` is limited compared to the `ncdfCF` package. Default is `FALSE`, which returns an `ncdfCF` data set instance.
#'
#' @return Either a `SpatRaster` object or an `ncdfCF` data set instance.
#'
#' @author Patrick Van Laake, Erik Kusch
#'
#' @importFrom ncdfCF open_ncdf
#' @importFrom terra rast
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncatt_get
#' @importFrom ncdf4 nc_close
#' @importFrom terra metags
#'
#' @examples
#' ## loading as ncdfCF object
#' NC_Read(fileName = system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' ## loading as terra SpatRaster object
#' NC_Read(fileName = system.file("extdata", "KiN_rast.nc", package = "ClimHub"), asTerra = TRUE)
#' @export
NC_Read <- function(fileName, asTerra = FALSE) {
  if (!asTerra) {
    nc_obj <- ncdfCF::open_ncdf(fileName)
  } else {
    ## we need to load the file now (read path from fileName)
    nc_obj <- terra::rast(fileName)
    ## Reading metadata and assigning it to returned raster
    nc_handle <- ncdf4::nc_open(fileName)
    Meta <- ncdf4::ncatt_get(nc_handle, 0)
    ncdf4::nc_close(nc_handle)
    Meta_vec <- unlist(Meta)
    terra::metags(nc_obj) <- Meta_vec
    return(nc_obj)
  }
  return(nc_obj)
}
