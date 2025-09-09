### DATASET QUICK FACTS ========================================================
#' Fact sheet overview of data set
#'
#' Read and return short overview of data set characteristics, supported types, extent, time frames and required arguments.
#'
#' @param dataset Character. Name of data set. Usually a set of words separated by dashes. See possible datasets by calling \code{\link{Meta\.Library()}}.
#'
#' @return A list object reporting information on queried dataset in standardised way:
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
#' @seealso \code{\link{Meta\.Library}}, \code{\link{Meta.Read}}, \code{\link{Meta.Variables}}, \code{\link{Meta.DOI}, \code{\link{Meta.Citation}}.
#'
#' @examples
#' Meta.QuickFacts("NORA3")
#'
#' @export
Meta.QuickFacts <- function(dataset = "NULL") {
    Meta.DataSet.Check(dataset)
    metadata_ls <- Meta.Read(dataset)
    metadata_ls[which(names(metadata_ls) != "variables")]
}
