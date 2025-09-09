### NETCDF WRITING AND READING SUPPORTING METADATA ========================================
#' Read or write NetCDF files and their metadata
#'
#' Read or write netcdf files and their metadata attributes.
#'
#' @param NC SpatRaster
#' @param FName Filename including directory
#' @param Write Logical. Whether to write metadata
#' @param Variable Character. Needed for writing. Variable name to be saved as varname in NC output.
#' @param LongVar Character. Needed for writing. Long variable name.
#' @param Unit Character. Needed for writing. Unit in which Variable is recorded.
#' @param Attrs Named vector of metadata attributes to be written. Needed for writing.  When reading, all attributes in the .nc file are read.
#' @param Compression Integer between 1 to 9. Needed for writing. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
#'
#' @importFrom terra writeCDF
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncatt_get
#' @importFrom terra metags
#' @importFrom terra longnames
#'
#' @return A SpatRaster with metadata
#' @export
WriteRead.NC <- function(NC, FName, Variable, LongVar, Unit = "NA", Attrs = NULL, Write = FALSE, Compression = 1) {
  ## remove = signs in variable vector, these break metags assignment
  Attrs <- gsub(pattern = "=", replacement = "...", Attrs)

  ## Writing metadata
  if (Write) {
    WriteNC(
      spatraster = NC,
      output_file = FName,
      variable = Variable,
      unit = Unit, longname = LongVar,
      compression = Compression,
      attrs = Attrs
    )
  }
  ## we need to load NC now
  NC <- rast(FName)
  ## Reading metadata and assigning it to returned raster
  nc <- nc_open(FName)
  Meta <- ncdf4::ncatt_get(nc, 0)
  # # Retrieve custom metadata
  # Meta <- lapply(names(Attrs), FUN = function(name) {
  #   ncatt_get(nc, 0, name)$value
  # })
  # # Close the NetCDF file
  nc_close(nc)
  Meta_vec <- unlist(Meta)
  # names(Meta_vec) <- names(Attrs)
  terra::metags(NC) <- Meta_vec

  ## return object
  return(NC)
}
