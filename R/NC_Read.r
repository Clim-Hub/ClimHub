#' @title Read NetCDF files and their metadata
#'
#' @description Read NetCDF files and their metadata attributes. Metadata from the NetCDF file is read and attached to the returned SpatRaster via `terra::metags()`.
#'
#' @param fileName Character. Filename including directory for reading/writing.
#'
#' @importFrom terra rast
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncatt_get
#' @importFrom ncdf4 nc_close
#' @importFrom terra metags
#'
#' @return A SpatRaster with metadata
#'
#' @author Erik Kusch
#'
#' @examples
#' Read_ras <- NC_Read(fileName = system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' terra::metags(Read_ras)[terra::metags(Read_ras)$name == "Citation", ]
#' @export
NC_Read <- function(fileName) {
  ## we need to load the file now (read path from fileName)
  nc_obj <- rast(fileName)
  ## Reading metadata and assigning it to returned raster
  nc_handle <- nc_open(fileName)
  Meta <- ncdf4::ncatt_get(nc_handle, 0)
  nc_close(nc_handle)
  Meta_vec <- unlist(Meta)
  terra::metags(nc_obj) <- Meta_vec

  ## return object
  return(nc_obj)
}
