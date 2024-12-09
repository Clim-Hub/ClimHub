### READ METADATA LIBRARY =========================================================
#' List out all supported data sets
#'
#' Provide an overview of all data sets for which metadata files are present.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#'
#' @return A vector of supported datasets.
#'
#' @seealso \code{\link{Meta.Read}}, \code{\link{Meta.Variables}}, \code{\link{Meta.DOI}}, \code{\link{Meta.QuickFacts}}, \code{\link{Meta.Citation}}.
#'
#' @examples
#' Meta.Library()
#'
#' @export
Meta.Library <- function() {
    owner <- "ErikKusch"
    repo <- "ClimHub"
    path <- "product-metadata"

    # Construct the API URL
    URL <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path)

    # Get the file list
    response <- httr::GET(URL)
    file_list <- httr::content(response, as = "text", encoding = "UTF-8")
    files <- jsonlite::fromJSON(file_list)

    # Extract file names
    file_names <- files$name
    tools::file_path_sans_ext(file_names)
}
