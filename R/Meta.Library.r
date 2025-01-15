### READ METADATA LIBRARY =========================================================
#' List out all supported data sets
#'
#' Provide an overview of all data sets for which metadata files are present.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom rvest read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>%
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

    # Construct the URL
    URL <- paste0("https://github.com/", owner, "/", repo, "/tree/master/", path)

    # Read the page
    page <- rvest::read_html(URL)

    # Extract the embedded JSON metadata
    raw_metadata <- page %>%
        rvest::html_nodes("script") %>% # Extract all <script> tags
        rvest::html_text() %>% # Get the text content
        grep("payload", ., value = TRUE) %>% # Locate the script containing "payload"
        jsonlite::fromJSON(simplifyVector = FALSE) # Parse JSON metadata

    # Navigate to the relevant "tree" section
    tree_items <- raw_metadata$payload$tree$items

    # make into vector of names of files
    tree_items <- unlist(lapply(tree_items, "[[", "name"))

    # subset for .json files
    tree_items <- tree_items[grep(pattern = "\\.json$", tree_items)]

    # Filter for JSON files without extensions
    tools::file_path_sans_ext(tree_items)
}



library(rvest)
