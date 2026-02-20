#' @title Prepare Bootstrap Intervals of Daily Time Series across Years
#'
#' @description Create bootstrap intervals of daily time series across years for a given time sequence. This function is needed for baseperiod quantile calculations for `Metrics_ETCCDI`.
#'
#' @param dates A character vector of dates in the format "YYYY-MM-DDTHH:MM:SS" (e.g., "1961-01-01T00:00:00") representing the time series for which bootstrap intervals should be created. Same as is obtained with the `dimnames(ds[["time"]])` of a `CFDataset` object called `ds`.
#' @param bootstrapWindow Numeric, uneven integer. Length of bootstrapping interval around each target date.
#'
#' @return A list of character vectors, each containing the dates that fall within the bootstrap window around each target date. The names of the list correspond to the day of year (in "MM-DD" format) for which the bootstrap intervals are created. Interval creation assumes a leap year (i.e., 366 days) to ensure that the same day of year is always included in the bootstrap intervals, even for February 29th.
#'
#' @author Erik Kusch
#'
#' @examples
#' time_series <- seq(
#'     as.POSIXct("1961-01-01 00:00:00", tz = "UTC"),
#'     as.POSIXct("1999-12-31 23:00:00", tz = "UTC"),
#'     by = "1 day"
#' )
#' Time_seq <- format(time_series, "%Y-%m-%dT%H:%M:%S")
#' Helper_DailyBootstrap(dates = Time_seq)
#'
Helper_DailyBootstrap <- function(dates, bootstrapWindow = 5) {
    ## input checking
    if (bootstrapWindow %% 2 != 1) {
        stop("Bootstrap window must be an odd number.")
    }

    ## transforming time sequence into dates
    Date_seq <- format(as.POSIXct(dates, tryFormats = c("%Y-%m-%dT%H:%M:%OS"), tz = "UTC"), "%Y-%m-%d")
    dates <- as.Date(Date_seq) # Convert Date_seq to Date objects
    years <- sort(unique(as.integer(format(dates, "%Y")))) # Get unique years

    ## actual grouping creation
    half_window <- (bootstrapWindow - 1) / 2
    offsets <- c(-half_window:-1, 0, 1:half_window)
    leap_year_dates <- as.Date("2000-01-01") + (0:365) # Compute MM-DD names for each day of year using a leap year

    # For each day of year
    grouping_ls <- lapply(1:366, function(doy) {
        centre_dates <- as.Date(paste0(years, "-01-01")) + (doy - 1)
        target_dates <- do.call(c, lapply(centre_dates, function(centre_date) {
            neigh_dates <- centre_date + offsets
            neigh_str <- format(neigh_dates, "%Y-%m-%d")
            neigh_str
        }))
        target_dates[target_dates %in% Date_seq] # Keep only dates that exist in the data
    })
    names(grouping_ls) <- format(leap_year_dates, "%m-%d")
    grouping_ls
}
