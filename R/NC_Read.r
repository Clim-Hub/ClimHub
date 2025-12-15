#' @title Read a netCDF file
#'
#' @description Open and read a netCDF file including its metadata.
#'
#' @param fileName Character. Fully qualified file name for a netCDF file on a local file system or the URL to a netCDF resource on a THREDDS server.
#' @param asTerra Logical. If `TRUE` (default), the contents of the netCDF file is returned as a `SpatRaster` object from the `terra` package.
#'
#' @return Either a `SpatRaster` object or an `ncdfCF` data set instance.
#'
#' @author Patrick Van Laake, Erik Kusch
#'
#' @importFrom ncdfCF open_ncdf
#'
#' @examples
#' ## loading as ncdfCF object
#' NC_Read(fileName = system.file("extdata", "KiN_rast.nc", package = "ClimHub"), asTerra = FALSE)
#' ## loading as terra SpatRaster object
#' NC_Read(fileName = system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' @export
NC_Read <- function(fileName, asTerra = TRUE) {
  ds <- ncdfCF::open_ncdf(fileName)
  if (asTerra) {
    NCVars <- names(ds$variables()) # get variable names in netCDF
    ds_ls <- lapply(NCVars, FUN = function(var) { # loop over variables and turn each into terra SpatRaster
      ## ! "need to ensure that axis names are X or Y" otherwise, this fails (see example code above)
      ds[[var]]$terra()
    })
    ds <- do.call(c, ds_ls) # combine all SpatRasters into a single SpatRaster
    ## ! "need to assign correct variable name, long name, units, etc. to SpatRaster layers" - they seem lost in conversion
  }
  return(ds)
}
