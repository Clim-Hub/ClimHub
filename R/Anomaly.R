#' @title Compute anomalies from climate projection data
#' @description Compute the anomalies of a data variable for a chosen era (range
#'   of years) relative to a baseline, as an absolute anomaly or relative to the
#'   baseline. The anomaly is calculated over all axes other than the temporal
#'   axis.
#' @details The anomaly is calculated by differencing values from the `era` by
#'   those of the `baseline`. Values are first averaged over the `period` and
#'   then the anomaly is computed for each `period` separately from the "era"
#'   and "baseline" values. In the output the temporal axis will be replaced
#'   with a climatological axis for each of the periods.
#'
#'   With relative anomalies there is an asymptotic behaviour close to 0 values
#'   for the baseline. The user is advised to determine that the resulting
#'   anomalies make sense in the context of the data.
#' @param var A `CFVariable` instance to calculate the anomalies for. The `var`
#'   must have a temporal axis.
#' @param baseline The baseline against which to calculate the anomalies. A
#'   range of years (`integer`) present in `var`.
#' @param era The range of years over which to calculate the anomaly, or a list
#'   thereof. If a list, any names will be used in the output.
#' @param period Optionally, the period over which each anomaly is to be
#'   calculated, either of "day", "dekad" (10-day period), "month" (default),
#'   "season", "quarter" or "year". Data in `var` is averaged to this period
#'   before the anomaly is calculated so the data in `var` has to have the same
#'   or higher resolution than this `period` specifies.
#' @param absolute Optional logical to indicate if the anomaly is to be an
#'   absolute value in units of argument `var` or relative to the baseline.
#'   Default is `TRUE`.
#' @param progress Optional progress bar. One call to this function makes 4
#'   ticks.
#' @returns A `CFVariable` with the anomaly data, or a (named) list according to
#'   the `era` argument. If either or both of the `baseline` or `era` range of
#'   years are outside of the temporal range of `var`, `NULL` is returned.
#' @author Patrick Van Laake
#' @export
#' @examples
#' \dontrun{
#' # pr is a CFVariable instance with a long time series of precipitation values
#' anom <- anomaly(pr, 1991:2020, 2041:2060)
#' }
anomaly <- function(var, baseline, era, period = "month", absolute = TRUE, progress = NULL) {
  if (!is.null(progress))
    progress$tick(tokens = list(info = var$name))

  # Baseline data
  if (is.numeric(baseline)) {
    baseline <- suppressWarnings(range(as.integer(baseline), na.rm = TRUE))
    if (is.infinite(baseline[1L]))
      stop("Bad `baseline` argument.")
    base <- var$summarise("baseline", mean, period, baseline)
    if (is.null(base))
      return(NULL)
  } else
    stop("Bad `baseline` argument.")
  baseline_years <- if (baseline[1L] == baseline[2L])
    paste("year", baseline[1L])
  else
    paste0("years ", baseline[1L], "-", baseline[2L])
  baseline_data <- base$raw()

  if (!is.null(progress))
    progress$tick(tokens = list(info = var$name))

  # Era
  era <- if (is.list(era)) {
    lapply(era, function(e) {
      e <- suppressWarnings(range(as.integer(e), na.rm = TRUE))
      if (is.infinite(e[1L]))
        stop("Bad `era` argument.")
      e
    })
  } else {
    era <- suppressWarnings(range(as.integer(era), na.rm = TRUE))
    if (is.infinite(era[1L]))
      stop("Bad `era` argument.")
    list(future = era)
  }
  era_years <- lapply(era, function(e){
    if (e[1L] == e[2L])
      paste("year", e[1L])
    else
      paste0("years ", e[1L], "-", e[2L])
  })

  if (!is.null(progress))
    progress$tick(tokens = list(info = var$name))

  future <- var$summarise("future", mean, period, era)
  if (!is.list(future))
    future <- list(future)

  # Output names
  name <- var$name
  long <- var$attribute("long_name")
  if (is.na(long))
    long <- name

  # Loop over the eras
  out <- lapply(future, function(fut) {
    # Data to calculate the anomaly over
    if (is.null(fut))
      return(NULL)

    # Calculate the anomaly
    anom <- fut$raw() - baseline_data
    if (!absolute)
      anom <- anom / baseline_data

    # Create the output CFVariable
    result <- ncdfCF::CFVariable$new(var = paste0(name, "_anomaly"),
                                     group = ncdfCF::makeGroup(),
                                     axes = fut$axes,
                                     values = anom,
                                     attributes = fut$attributes)
    result$set_attribute("long_name", "NC_CHAR",
                         sprintf("%s %s anomaly per %s averaged over the %s over the baseline %s",
                                 long, if (absolute) "absolute" else "relative", period,
                                 era_years, baseline_years))

    if (absolute) {
      units <- var$attribute("units")
      if (!is.na(units))
        result$set_attribute("units", "NC_CHAR", units)
    } else
      result$set_attribute("units", "NC_CHAR", "1")

    result$set_attribute("ncdfCF_anomaly_baseline", "NC_INT", baseline)
    result$set_attribute("ncdfCF_anomaly_period", "NC_CHAR", period)
    result$set_attribute("ncdfCF_anomaly_absolute", "NC_INT", as.integer(absolute))
  })

  if (!is.null(progress))
    progress$tick(tokens = list(info = var$name))

  if (length(out) == 1L)
    out[[1L]]
  else {
    names(out) <- names(era)
    out
  }
}
