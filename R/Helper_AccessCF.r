#' @title Execute direct download calls with ncdfCF
#'
#' @description Loops over all supplied URLs assuming these query netCDF files from open hosts. These are then registered and subsetted for variable and extent using ncdfCF before being downloaded and appended by the "time" axis.
#'
#' @param URLs Character. Vector of URLs for download.
#' @param variable Character. Variable name to extract from the netCDF files.
#' @param subset List, optional. A list with the ranges along the axes of the data variable to subset.
#' @param verbose Logical. If progress should be displayed in the console.
#' @return A CFVariable object containing the requested data.
#'
#' @author Erik Kusch, Patrick Van Laake
Helper_AccessCF <- function(URLs, variable, subset = list(), verbose = TRUE) {
  ## make progress bar
  if (verbose) {
    pb <- Helper_Progress(iterLength = length(URLs), text = "Downloading Data")
  }

  ## loading data
  out <- NULL
  for (LoadIter in seq_along(URLs)) {
    iter_dataset <- NC_Read(fileName = URLs[LoadIter], vars = variable, subset)
    iter_var <- iter_dataset[[variable]]
    if (verbose) {
      pb$tick(tokens = list(layer = LoadIter))
    }
    if (is.null(out)) {
      out <- iter_var
    } else {
      out$append(iter_var, "time")
    } # "time" should be an argument to this function
  }
  out
}
