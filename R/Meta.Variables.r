### DATASET VARIABLES ==========================================================
#' Variables available within data set
#'
#' Read and return overview of variables available for specific data set.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Meta.Library()}}.
#'
#' @return Data frame. Contains at least 3 columns:
#' \itemize{
#' \item{name}{Variable clear name and what is used to query a download}
#' \item{unit}{Unit of measurement}
#' \item{issues}{potential known issues with this variable}
#' }
#' May contain additional columns mostly used for backend operations.
#'
#' @seealso \code{\link{Meta.Library}}, \code{\link{Meta.Read}}, \code{\link{Meta.DOI}}, \code{\link{Meta.QuickFacts}}, \code{\link{Meta.Citation}}.
#'
#' @examples
#' Meta.Variables(dataset = "NORA3")
#'
#' @export
Meta.Variables <- function(dataset = "NULL") {
    Meta.DataSet.Check(dataset)
    Meta.Read(dataset = dataset)$variables
}
