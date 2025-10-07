### READ METADATA FACTS ========================================================
#' Data set overview
#'
#' Read and return metadata for specific data set.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Meta.Library()}}.
#'
#' @importFrom jsonlite fromJSON
#'
#' @return List. Contains information of data set, type, variables, resolution, citation, etc.
#'
#' @seealso \code{\link{Meta.Library}}, \code{\link{Meta.Variables}}, \code{\link{Meta.DOI}}, \code{\link{Meta.QuickFacts}}, \code{\link{Meta.Citation}}.
#'
#' @examples
#' Meta.Read(dataset = "NORA3")
#'
#' @export
Meta.Read <- function(dataset = "NULL") {
    owner <- "Clim-Hub"
    repo <- "ClimHub"
    path <- "product-metadata"

    Meta.DataSet.Check(dataset)

    # URL of the raw JSON file
    URL <- paste0("https://raw.githubusercontent.com/", owner, "/", repo, "/master/", path, "/", paste0(dataset, ".json"))

    # Read JSON content
    json_data <- jsonlite::fromJSON(URL)

    # View the data
    json_data
}
