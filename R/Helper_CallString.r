#' @title Prepare metadata string for storing in NetCDF
#' @description Takes function call arguments and prepares a named vector suitable for storing as metadata attributes in NetCDF files. Optionally, also preprends a citation string.
#'
#' @param callargs Vector. A named vector of function call arguments to be stored as metadata.
#' @param functionName Character. The name of the function being called and whose arguments are stored in callargs.
#' @param citation Optional, Character. A citation string to be prepended to the metadata.
#'
#' @return A named vector suitable for storing as metadata attributes in NetCDF files.
#'
#' @author Erik Kusch
#'
#' @examples
#' callargs <- c(variable = "TS", dateStart = "1961-08-01 00", dateStop = "1961-08-02 18")
#' functionName <- "Access_NORA3"
#' citation <- "Please cite NORA3 data as: ..."
#' Helper_CallString(callargs, functionName, citation)
Helper_CallString <- function(callargs, functionName, citation = NULL) {
    callargs[sapply(callargs, is.null)] <- "NULL"
    callargs[sapply(callargs, class) == "name"] <- ""
    names(callargs) <- paste(functionName, names(callargs), sep = "+")
    Meta_vec <- c(c(Citation = citation), unlist(callargs))
    return(Meta_vec)
}
