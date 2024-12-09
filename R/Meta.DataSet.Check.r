### CHECK METADATA STRING ========================================================
#' Check dataset string validity
#'
#' Check if dataset string is resolvable in metadata library.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Meta.Library()}}.
#'
#' @importFrom jsonlite fromJSON
#'
#' @return Throws an error message if user-provided dataset string is not recognised in metadata library.
#'
#' @seealso \code{\link{Meta.Library}}, \code{\link{Meta.Variables}}, \code{\link{Meta.DOI}}, \code{\link{Meta.QuickFacts}}.
#'
#' @examples
#' Meta.DataSet.Check(dataset = "NORA3")
#' Meta.DataSet.Check(dataset = "NULL")
#'
Meta.DataSet.Check <- function(dataset) {
    if (!(dataset %in% Meta.Library())) {
        stop(
            paste(
                "Please specify one of the following supported datasets:",
                paste(Meta.Library(), collapse = "\n"),
                sep = "\n"
            )
        )
    }
}
