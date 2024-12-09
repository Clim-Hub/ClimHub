### DATASET CITATION DOI ===========================================================
#' DOI of data set
#'
#' Read and return DOI of data set for easy citation.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Meta\.Library()}}.
#'
#' @return Character. DOI string for data set.
#'
#' @seealso \code{\link{Meta\.Library}}, \code{\link{Meta.Read}}, \code{\link{Meta.Variables}}, \code{\link{Meta.QuickFacts}}, \code{\link{Meta.Citation}}.
#'
#' @examples
#' Meta.DOI(dataset = "NORA3")
#'
#' @export
Meta.DOI <- function(dataset = "NULL") {
    Meta.DataSet.Check(dataset)
    Meta.Read(dataset = dataset)$doi
}

### DATASET CITATION ===========================================================
#' DOI of data set
#'
#' Read and return DOI of data set for easy citation.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Meta\.Library()}}.
#'
#' @return Character. Citation string for data set.
#'
#' @seealso \code{\link{Meta\.Library}}, \code{\link{Meta.Read}}, \code{\link{Meta.Variables}}, \code{\link{Meta.QuickFacts}}, \code{\link{Meta.DOI}.
#'
#' @examples
#' Meta.Citation(dataset = "NORA3")
#'
#' @export
Meta.Citation <- function(dataset = "NULL") {
    Meta.DataSet.Check(dataset)
    Meta.Read(dataset = dataset)$citation
}
