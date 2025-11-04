#' @title Resolve time zones as requested by user and UTC format with which to query from CDS
#'
#' @description Create UTC counterparts of user-input dates for CDS queries
#'
#' @param dates A vector of POSIXct objects
#'
#' @return A data frame on input dates respective to user-queried timezone and their UTC counterparts.
#' 
#' @author Erik Kusch
#'
#' @examples
#' IN_DateStart <- as.POSIXct("1995-01-01 00:00", tz = "CET")
#' IN_DateStop <- as.POSIXct("2005-01-01 23:00", tz = "CET")
#' Dates_df <- Helper_MakeUTC(dates = c(IN_DateStart, IN_DateStop))
#' Dates_df
Helper_MakeUTC <- function(dates = NULL) {
    data.frame(
        IN = dates,
        UTC = do.call(c, lapply(dates, FUN = function(x) {
            as.POSIXct(x, tz = "UTC")
        }))
    )
}
