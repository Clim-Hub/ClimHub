#' @title Checking if a file already exists
#'
#' @description If a file already exists in a given place, load that file.
#'
#' @param fileName Filename including directory
#' @param loadFun function with which to load filetype of fileName
#' @param load Logical. Whether to load the data or not
#' @param verbose Logical. Whether to print/message function progress in console or not.
#'
#' @return Either a data object or NULL
#' 
#' @author Erik Kusch 
#'
#' @examples
#' ## returning file contents as R object if file present and prompted to load
#' Helper_FileCheck(fileName = system.file("extdata", "KiN_rast.nc", package = "ClimHub"), loadFun = NC_Read, load = TRUE, verbose = TRUE)
#' ## returning NULL if file does not exist
#' Helper_FileCheck(fileName = "", loadFun = NC_Read, load = TRUE, verbose = TRUE)
Helper_FileCheck <- function(fileName, loadFun, load = TRUE, verbose = TRUE) {
  # fileName <- file.path(Dir, fileName)
  file <- NULL
  if (file.exists(fileName)) {
    if (verbose) {
      print(paste0(
        "The file with ", fileName, " already exists in."
      ))
    }
    if (load) {
      if (verbose) {
        print("Loading this file for you from the disk.")
      }
      file <- sapply(fileName, loadFun)[[1]]
    } else {
      file <- "Present. Not Loaded."
    }
  }
  return(file)
}
