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
    expr <- paste0("CFVariable ", operator, " ", threshold)
    Thresh_CF <- eval(parse(text = expr))

    if (returnValues) {
        Thresh_CF <- CFVariable / Thresh_CF
    }

    ## return summary
    # when thresholding creates Inf values (e.g. dividing by the logical mask) we want to treat them as missing rather than drop them silently.
    Thresh_CF$summarise(
        "Tresholded",
        function(x) {
            x[!is.finite(x)] <- NA_real_
            returnSummary(x)
        },
        returnTResolution
    )
}
