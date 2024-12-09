### READ METADATA LIBRARY =========================================================
#' List out all supported data sets
#'
#' Provide an overview of all data sets for which metadata files are present.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom supportR github_ls
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
    URL <- paste0("https://github.com/", owner, "/", repo)

    # list files
    file_names <- supportR::github_ls(URL, folder = path, quiet = TRUE)$name
    tools::file_path_sans_ext(file_names)
}
