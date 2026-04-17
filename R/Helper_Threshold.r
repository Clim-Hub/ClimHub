#' @title Apply tresholding to CFVariable contents
#'
#' @description Evaluates threshold criteria defined by user against a CFVariable and reports summaries through the `$summarise` method of `ncdfCF`.
#'
#' @param CFVariable A CFVariable within which thresholding should be applied
#' @param operator Character. One of ">", "<", ">=", "<=", "==", "!=" to evaluate CFVariable cells against threshold value.
#' @param threshold Numeric or Character. threshold to evaluate CFVariable cells against using operator value.
#' @param threshMode Character, optional. If supplying a CFVariable containing baseline quantiles for ETCCDI calculation, setting `threshMode = "ETCCDIQuantiles"` will internally apply location and day-of-year specific thresholds to `CFVariable` input. Default is `NULL` avoiding this behaviour.
#' @param returnValues Logical. Whether to return CFVariable of values matching thresholding (TRUE) or only logical raster indicating where thresholding is matched (FALSE).
#' @param returnSummary Function. Summary function for tresholded CFVariable.
#' @param returnTResolution Character. Temporal resolution of summary of tresholded CFVariable.
#'
#' @importFrom ncdfCF as_CF
#'
#' @return A CFVariable.
#'
#' @author Erik Kusch
#'
#' @examples
#' Data_CF <- NC_Read("inst/extdata/KiN_tas.nc")[["tas"]]
#' Helper_Threshold(
#'     CFVariable = Data_CF,
#'     operator = "<",
#'     threshold = 273.15
#' )
#' Helper_Threshold(
#'     CFVariable = Data_CF,
#'     operator = "<",
#'     threshold = 273.15,
#'     returnValues = TRUE,
#'     returnSummary = mean
#' )
Helper_Threshold <- function(CFVariable, operator, threshold, threshMode = NULL, returnValues = FALSE, returnSummary, returnTResolution = "year") {
    ## input check, needs rwriting to Helper.InputChecker
    if (!operator %in% c(">", "<", ">=", "<=", "==", "!=")) {
        stop("operator must be one of: '>', '<', '>=', '<=', '==', '!='")
    }

    ## Get the raw data array
    raw_array <- CFVariable$raw()
    array_dims <- dimnames(raw_array)

    ## reformatting treshold to array if necessary
    if (class(threshold)[1] == "CFVariable") {
        threshold <- threshold$raw()
    }

    ## reformatting of threshold if ETCCDI Baseline is specified
    if (identical(threshMode, "ETCCDIQuantiles")) {
        # Extract the time labels from dimnames
        raw_dates <- as.Date(sub("T.*$", "", dimnames(raw_array)$time)) # drops the "T12:00:00"
        thresh_dates <- as.Date(dimnames(threshold)$time)
        # Create a month-day key for matching
        raw_key <- format(raw_dates, "%m-%d")
        thresh_key <- format(thresh_dates, "%m-%d")
        # match dates
        idx <- match(raw_key, thresh_key)
        # Now expand `threshold` to a 3D array matching raw_array time length:
        threshold <- threshold[, , idx, drop = FALSE]
        dimnames(threshold) <- array_dims
    }

    ## apply thresholding
    if (class(threshold) == "array") { # this is comparing to arrays and setting every exceeding value in the former to NA
        expr <- paste("raw_array", operator, "threshold")
    } else { # this is setting every value that does NOT match the operator threshold to NA
        expr <- paste0("raw_array", operator, threshold)
    }
    mask <- eval(parse(text = expr))
    raw_array[!mask] <- NaN

    ## binarise if return of values is not desired
    if (!returnValues) {
        raw_array[!is.na(raw_array)] <- 1 # these are those not masked due to threshold
        raw_array[is.nan(raw_array)] <- 0 # these are those masked due to threshold
    }
    dimnames(raw_array) <- array_dims

    # Create a new CFVariable from the filtered data
    Thresh_CF <- ncdfCF::as_CF("Thresholded", raw_array)

    # The original attributes - drop "actual_range" because it is no longer accurate
    atts <- CFVariable$attributes[CFVariable$attributes$name != "actual_range", ]

    # Loop over the attributes and set them in the new CFVariable
    apply(atts, 1, function(a) Thresh_CF$set_attribute(a$name, a$type, a$value))

    ## return summary only when explicitly requested by caller
    ## (run-based ETCCDI logic needs unsummarised daily output)
    if (!missing(returnSummary)) {
        Thresh_CF <- Thresh_CF$summarise(
            "Tresholded",
            returnSummary,
            returnTResolution
        )
    }

    ## return object
    Thresh_CF
}
