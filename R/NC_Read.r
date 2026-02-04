#' @title Read netCDF files
#'
#' @description Read netCDF files and their metadata. Specific data variables may be extracted and optionally subsetted over the dimensions of the data variable.
#'
#' In the returned `CFDataset`, the data array of individual data variables need not have been loaded into memory.
#'
#' @param fileName Character. Fully qualified name of the netCDF resource. This may be on a local file system or on a THREDDS server.
#' @param vars Character vector, optional. If supplied, only the indicated data variables are read from the netCDF resource.
#' @param ... Optional. Arguments for subsetting the data variables. When supplied, these must be of the form `axis_name = c(0, 90)`, naming an axis of the data variables and the extreme values of the range to extract. There may be multiple such entries, or they can alternatively be supplied in a named list.
#' @param forWriting Logical, default is `FALSE`. If `TRUE`, the netCDF resource will be opened for writing. This is typically only allowable on netCDF files on a local file system.
#' @return A `CFDataset` with the data variables, optionally subsetted.
#'
#' @author Patrick Van Laake, Erik Kusch
#'
#' @examples
#' FNAME <- system.file("extdata", "NORA3.nc", package = "ClimHub")
#' # Reading entire CFDataset
#' NC_Read(fileName = FNAME)
#' # Reading just specific variable
#' NC_Read(fileName = FNAME, vars = "T2M")
#' # Read while subsetting
#' NC_Read(
#'   fileName = FNAME,
#'   vars = "T2M",
#'   list(
#'     longitude = c(10, 20),
#'     latitude = c(55, 65),
#'     time = "2001-08-01T06:00:00"
#'   )
#' )
#' @export
NC_Read <- function(fileName, vars, ..., forWriting = FALSE) {
  ds <- ncdfCF::open_ncdf(resource = fileName, write = forWriting)
  if (inherits(ds, "CFDataset")) {
    subs <- list(...)
    if (is.list(subs) && length(subs)) subs <- subs[[1L]]
    if (length(subs)) {
      # Subset the variables
      if (missing(vars)) {
        vars <- ds$var_names
      }
      ds_new <- ncdfCF::create_ncdf()
      lapply(vars, function(v) {
        var <- ds[[v]]$subset(subs)
        if (inherits(var, "CFVariable")) {
          ds_new$add_variable(var)
        }
      })
      return(ds_new)
    } else {
      # Get the full extent of the data variables
      if (missing(vars)) {
        return(ds)
      } else {
        ds_new <- ncdfCF::create_ncdf()
        lapply(vars, function(v) {
          var <- ds[[v]]
          if (inherits(var, "CFVariable")) {
            ds_new$add_variable(var)
          }
        })
        return(ds_new)
      }
    }
  } else {
    stop(paste("Could not open the netCDF resource:", fileName))
  }
}
