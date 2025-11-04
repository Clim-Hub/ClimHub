#' @title Check dataSet string validity
#'
#' @description Check if dataSet string is resolvable in metadata library.
#'
#' @param dataSet Character. Name of data set. Usually a set of words separated by dashes. See possible dataSets by calling \code{\link{Discovery_Library()}}.
#'
#' @importFrom jsonlite fromJSON
#'
#' @return Throws an error message if user-provided dataSet string is not recognised in metadata library.
#' 
#' @author Erik Kusch
#'
#' @seealso \code{\link{Discovery_Library}}, \code{\link{Discovery_Variables}}, \code{\link{Discovery_DOI}}, \code{\link{Discovery_QuickFacts}}.
#'
#' @examples
#' Discovery_DataSet.Check(dataSet = "NORA3")
#' # Discovery_DataSet.Check(dataSet = "NULL")
#' @export
Discovery_DataSet.Check <- function(dataSet) {
    if (!(dataSet %in% Discovery_Library())) {
        stop(
            paste(
                "Please specify one of the following supported dataSets:",
                paste(Discovery_Library(), collapse = "\n"),
                sep = "\n"
            )
        )
    }
}


#' @title Data set overview
#'
#' @description Read and return metadata for specific data set.
#'
#' @param dataSet Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Discovery_Library()}}.
#'
#' @importFrom jsonlite fromJSON
#'
#' @return List. Contains information of data set, type, variables, resolution, citation, etc.
#' 
#' @author Erik Kusch
#'
#' @seealso \code{\link{Discovery_Library}}, \code{\link{Discovery_Variables}}, \code{\link{Discovery_DOI}}, \code{\link{Discovery_QuickFacts}}, \code{\link{Discovery_Citation}}.
#'
#' @examples
#' Discovery_Read(dataSet = "NORA3")
#'
#' @export
Discovery_Read <- function(dataSet = "NULL") {
    owner <- "Clim-Hub"
    repo <- "ClimHub"
    path <- "product-metadata"

    Discovery_DataSet.Check(dataSet)

    # URL of the raw JSON file
    URL <- paste0("https://raw.githubusercontent.com/", owner, "/", repo, "/master/", path, "/", paste0(dataSet, ".json"))

    # Read JSON content
    json_data <- jsonlite::fromJSON(URL)

    # View the data
    json_data
}

#' @title Get DOI of dataset
#'
#' @description Retrieve and return the DOI (Digital Object Identifier) for a specific dataset for easy citation.
#'
#' @param dataSet Character. Name of dataset. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Discovery_Library()}}.
#'
#' @return Character. DOI string for dataset.
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Discovery_Library}}, \code{\link{Discovery_Read}}, \code{\link{Discovery_Variables}}, \code{\link{Discovery_QuickFacts}}, \code{\link{Discovery_Citation}}.
#'
#' @examples
#' Discovery_DOI(dataSet = "NORA3")
#'
#' @export
Discovery_DOI <- function(dataSet = "NULL") {
    Discovery_DataSet.Check(dataSet)
    Discovery_Read(dataSet = dataSet)$doi
}

#' @title Get citation for dataset
#'
#' @description Retrieve and return the proper citation for a specific dataset.
#'
#' @param dataSet Character. Name of dataset. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Discovery_Library()}}.
#'
#' @return Character. Citation string for dataset.
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Discovery_Library}}, \code{\link{Discovery_Read}}, \code{\link{Discovery_Variables}}, \code{\link{Discovery_QuickFacts}}, \code{\link{Discovery_DOI}}.
#'
#' @examples
#' Discovery_Citation(dataSet = "NORA3")
#'
#' @export
Discovery_Citation <- function(dataSet = "NULL") {
    Discovery_DataSet.Check(dataSet)
    Discovery_Read(dataSet = dataSet)$citation
}

#' @title List supported datasets
#'
#' @description Provides an overview of all datasets for which metadata files are present in the package.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom rvest read_html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr %>%
#'
#' @return Character vector. Names of supported datasets.
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Discovery_Read}}, \code{\link{Discovery_Variables}}, \code{\link{Discovery_DOI}}, \code{\link{Discovery_QuickFacts}}, \code{\link{Discovery_Citation}}.
#'
#' @examples
#' Discovery_Library()
#'
#' @export
Discovery_Library <- function() {
    owner <- "Clim-Hub"
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
    tools::file_path_sans_ext(tree_items[-grep("SKELETON.json", tree_items)])
}


#' @title Quick facts about dataset
#'
#' @description Retrieves and returns a short overview of dataset characteristics, including supported types, extent, time frames, and required arguments.
#'
#' @param dataSet Character. Name of dataset. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Discovery_Library()}}.
#'
#' @return List. Contains information on queried dataset in standardised way:
#' \itemize{
#' \item{DataSet}{data set string}.
#' \item{Type}{character, supported types of the data set}.
#' \item{URL}{character, url of CDS webpage corresponding to data set}.
#' \item{Description}{character, plain text description of data set scraped from CDS}.
#' \item{TResolution}{character, base temporal resolution of each layer in data set}.
#' \item{TStep}{numeric, vector of time step between layers in data set corresponding to Type}.
#' \item{TStart}{POSIXct, date and time at which first layer is available}.
#' \item{TEnd}{POSIXct or character, date and time at which first layer is available}.
#' \item{Projection}{crs of data set}.
#' \item{SpatialResolution}{numeric, resolution of data set in space in degrees}.
#' \item{CDSArguments}{list, required arguments for CDS call beyond standard arguments and also reporting default/options for common CDS query arguments}.
#' }
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Discovery_Library}}, \code{\link{Discovery_Read}}, \code{\link{Discovery_Variables}}, \code{\link{Discovery_DOI}}, \code{\link{Discovery_Citation}}.
#'
#' @examples
#' Discovery_QuickFacts("NORA3")
#'
#' @export
Discovery_QuickFacts <- function(dataSet = "NULL") {
    Discovery_DataSet.Check(dataSet)
    metadata_ls <- Discovery_Read(dataSet = dataSet)
    metadata_ls[which(names(metadata_ls) != "variables")]
}


#' @title List dataset variables
#'
#' @description Reads and returns an overview of variables available for a specific dataset.
#'
#' @param dataSet Character. Name of dataset. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Discovery_Library()}}.
#'
#' @return Data frame. Contains at least 3 columns:
#' \itemize{
#' \item{name}{Variable clear name and what is used to query a download}
#' \item{unit}{Unit of measurement}
#' \item{issues}{Potential known issues with this variable}
#' }
#' May contain additional columns mostly used for backend operations.
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Discovery_Library}}, \code{\link{Discovery_Read}}, \code{\link{Discovery_DOI}}, \code{\link{Discovery_QuickFacts}}, \code{\link{Discovery_Citation}}.
#'
#' @examples
#' Discovery_Variables(dataSet = "NORA3")
#'
#' @export
Discovery_Variables <- function(dataSet = "NULL") {
    Discovery_DataSet.Check(dataSet)
    Discovery_Read(dataSet = dataSet)$variables
}
