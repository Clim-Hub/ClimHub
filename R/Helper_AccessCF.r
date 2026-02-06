#' @title Execute direct download calls with ncdfCF
#'
#' @description Loops over all supplied URLs assuming these query netCDF files from open hosts. These are then registered and subsetted for variable and extent using ncdfCF before being downloaded and appended by the "time" axis.
#'
#' @param URLs Character. Vector of URLs for download.
#' @param variable Character. Variable name to extract from the netCDF files.
#' @param subset List, optional. A list with the ranges along the axes of the data variable to subset.
#' @param verbose Logical. If progress should be displayed in the console.
#' @return A CFVariable object containing the requested data.
#'
#' @author Erik Kusch, Patrick Van Laake
#'
#' @examples
#' Helper_AccessCF(
#'   URLs = "https://thredds.met.no/thredds/dodsC/nora3/2011/05/28/12/fc2011052812_009_sfx.nc",
#'   variable = "T2M",
#'   subset = list(
#'     longitude = c(0, 40),
#'     latitude = c(50, 60)
#'   )
#' )
#'
#' # subsetting with time and extent
#' Helper_AccessCF(
#'   URLs = c(
#'     "https://thredds.met.no/thredds/dodsC/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries/tas/eqm/hist/noresm-r1i1p1f1-hclim/noresm-r1i1p1f1-hclim_hist_eqm-sn2018v2005_rawbc_norway_1km_tas_daily_2019.nc4",
#'     "https://thredds.met.no/thredds/dodsC/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries/tas/eqm/hist/noresm-r1i1p1f1-hclim/noresm-r1i1p1f1-hclim_hist_eqm-sn2018v2005_rawbc_norway_1km_tas_daily_2020.nc4",
#'     "https://thredds.met.no/thredds/dodsC/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries/tas/eqm/ssp370/noresm-r1i1p1f1-hclim/noresm-r1i1p1f1-hclim_ssp370_eqm-sn2018v2005_rawbc_norway_1km_tas_daily_2021.nc4",
#'     "https://thredds.met.no/thredds/dodsC/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries/tas/eqm/ssp370/noresm-r1i1p1f1-hclim/noresm-r1i1p1f1-hclim_ssp370_eqm-sn2018v2005_rawbc_norway_1km_tas_daily_2022.nc4"
#'   ),
#'   variable = "tas",
#'   subset = list(
#'     lon = c(6, 8),
#'     lat = c(62, 64),
#'     time = c("2019-08-01T00:00:00", "2022-09-18T00:00:00")
#'   )
#' )
Helper_AccessCF <- function(URLs, variable, subset = list(), verbose = TRUE) {
  ## make progress bar
  if (verbose) {
    pb <- Helper_Progress(iterLength = length(URLs), text = "Downloading Data")
  }

  ## loading data
  out <- NULL
  for (LoadIter in seq_along(URLs)) {
    iter_dataset <- NC_Read(fileName = URLs[LoadIter], vars = variable, subset)
    iter_var <- iter_dataset[[variable]]
    if (verbose) {
      pb$tick(tokens = list(layer = LoadIter))
    }
    if (is.null(out)) {
      out <- iter_var
    } else {
      out$append(iter_var, "time")
    } # "time" should be an argument to this function
  }
  out
}
