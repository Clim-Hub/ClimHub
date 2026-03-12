#' @title Apply tresholding to CFVariable contents
#'
#' @description Evaluates threshold criteria defined by user against a CFVariable and reports summaries through the `$summarise` method of `ncdfCF`.
#'
#' @param CFVariable A CFVariable within which thresholding should be applied
#' @param operator Character. One of ">", "<", ">=", "<=", "==", "!=" to evaluate CFVariable cells against threshold value.
#' @param threshold Numeric or Character. threshold to evaluate CFVariable cells against using operator value.
#' @param returnValues Logical. Whether to return CFVariable of values matching thresholding (TRUE) or only logical raster indicating where thresholding is matched (FALSE).
#' @param returnSummary Function. Summary function for tresholded CFVariable.
#' @param returnTResolution Character. Temporal resolution of summary of tresholded CFVariable.
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
Helper_Threshold <- function(CFVariable, operator, threshold, returnValues = FALSE, returnSummary = sum, returnTResolution = "year") {
    ## input check, needs rwriting to Helper.InputChecker
    if (!operator %in% c(">", "<", ">=", "<=", "==", "!=")) {
        stop("operator must be one of: '>', '<', '>=', '<=', '==', '!='")
    }

    ## apply thresholding
    # Get the raw data array
    raw_array <- CFVariable$raw()
    # array_dims <- dimnames(raw_array)

    # Apply the logical filter over the entire array at once
    expr <- paste0("raw_array[raw_array", operator, threshold, "] <- NA")
    eval(parse(text = expr))

    if (!returnValues) {
        raw_array <- (!is.na(raw_array)) * 1
    }
    # dimnames(raw_array) <- array_dims

    # Create a new CFVariable from the filtered data
    Thresh_CF <- as_CF("Thresholded", raw_array)

    # The original attributes - drop "actual_range" because it is no longer accurate
    atts <- CFVariable$attributes[CFVariable$attributes$name != "actual_range", ]

    # Loop over the attributes and set them in the new CFVariable
    apply(atts, 1, function(a) Thresh_CF$set_attribute(a$name, a$type, a$value))

    ## return summary
    # when thresholding creates Inf values (e.g. dividing by the logical mask) we want to treat them as missing rather than drop them silently.
    Thresh_CF$summarise(
        "Tresholded",
        returnSummary,
        returnTResolution
    )
}
