### DATE REFORMATTING ==========================================================
#' Resolve time zones as requested by user and UTC format with which to query from CDS
#'
#' Create UTC counterparts of user-input dates for CDS queries
#'
#' @param DatesVec A vector of POSIXct objects
#'
#' @return A data frame on input dates respective to user-queried timezone and their UTC counterparts.
#'
#' @examples
#' IN_DateStart <- as.POSIXct("1995-01-01 00:00", tz = "CET")
#' IN_DateStop <- as.POSIXct("2005-01-01 23:00", tz = "CET")
#' Dates_df <- Helper.MakeUTC(DatesVec = c(IN_DateStart, IN_DateStop))
#' Dates_df
#'
#' @export
Helper.MakeUTC <- function(DatesVec = NULL) {
    data.frame(
        IN = DatesVec,
        UTC = do.call(c, lapply(DatesVec, FUN = function(x) {
            as.POSIXct(x, tz = "UTC")
        }))
    )
}
