### NETCDF WRITING AND READING SUPPORTING METADATA ========================================
#' Read or write NetCDF files and their metadata
#'
#' Read or write netcdf files and their metadata attributes.
#'
#' @param NC SpatRaster
#' @param FName Filename including directory
#' @param Variable Character. Variable name to be saved as varname in NC output.
#' @param LongVar Character. Character. Long variable name.
#' @param Unit Character. Unit in which Variable is recorded.
#' @param Attrs Named vector of metadata attributes
#' @param Write Logical. Whether to write metadata
#' @param Compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
#'
#' @importFrom terra writeCDF
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncatt_get
#' @importFrom terra metags
#' @importFrom terra longnames
#'
#' @return A SpatRaster with metadata
#'
WriteRead.NC <- function(NC, FName, Variable, LongVar, Unit = "NA", Attrs, Write = FALSE, Compression = 1) {
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
    NC <- rast(FName)
    # NC <- writeCDF(x = NC, filename = FName, varname = Variable, unit = Unit, longname = longnames(NC), compression = Compression)
    # nc <- nc_open(FName, write = TRUE)
    # for (name in names(Attrs)) {
    #   ncatt_put(nc, varid = 0, attname = name, attval = Attrs[[name]])
    # }
    # nc_close(nc)
  }
  ## Reading metadata
  nc <- nc_open(FName)
  # Retrieve custom metadata
  Meta <- lapply(names(Attrs), FUN = function(name) {
    ncatt_get(nc, 0, name)$value
  })
  # Close the NetCDF file
  nc_close(nc)
  Meta_vec <- unlist(Meta)
  names(Meta_vec) <- names(Attrs)
  terra::metags(NC) <- Meta_vec

  ## return object
  return(NC)
}
