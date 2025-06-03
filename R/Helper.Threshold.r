### DOWNLOAD ARGUMENT INPUT CHECKING ========================================================
#' Check validity of user input
#'
#' Loops over contents of a named list of lists where each sublist contains the pointers Input, Allowed, and Operator.
#'
#' @param Raster A SpatRaster within which coverage should be identified
#' @param Operator Character. One of ">", "<", ">=", "<=", "==", "!=" to evaluate Raster cells against Threshold value.
#' @param Threshold Numeric or Character. Threshold to evaluate Raster cells against using Operator value.
#' @param ReturnValues Logical. Whether to return Raster of values matching thresholding (TRUE) or only logical raster indicating where thresholding is matched (FALSE).
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra nlyr
#'
#' @return A SpatRaster.
#'
#' @examples
#' Data_rast <- rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1:31]]
#' Helper.Threshold(
#'     Raster = Data_rast,
#'     Operator = "<",
#'     Threshold = 2650
#' )
#' Helper.Threshold(
#'     Raster = Data_rast,
#'     Operator = "<",
#'     Threshold = 2650,
#'     ReturnValues = TRUE
#' )
#'
Helper.Threshold <- function(Raster, Operator, Threshold, ReturnValues = FALSE, verbose = TRUE) {
    ## input check, needs rwriting to Helper.InputChecker
    if (!Operator %in% c(">", "<", ">=", "<=", "==", "!=")) {
        stop("Operator must be one of: '>', '<', '>=', '<=', '==', '!='")
    }

    ## progress bar
    pb <- Helper.Progress(IterLength = nlyr(Raster), Text = "Applying Thresholding")

    ## going layer by layer to avoid error reported in https://github.com/rspatial/terra/issues/1556
    ThresholdedLayers_ls <- lapply(1:nlyr(Raster), FUN = function(Iter) {
        expr <- paste0("Raster[[Iter]] ", Operator, " ", Threshold)
        ret_rast <- eval(parse(text = expr))
        if (verbose) {
            pb$tick(tokens = list(layer = Iter))
        }
        if (ReturnValues) {
            ret_rast2 <- Raster[[Iter]]
            ret_rast2[!ret_rast] <- NA
            ret_rast <- ret_rast2
        }
        ret_rast
    })

    ## return fused layers as SpatRaster
    do.call(c, ThresholdedLayers_ls)
}
