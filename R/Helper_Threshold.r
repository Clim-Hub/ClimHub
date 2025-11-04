#' @title Apply tresholding to raster layers
#'
#' @description Loops over layers in a SpatRaster object and evaluates threshold criteria defined by user.
#'
#' @param spatRaster A SpatRaster within which thresholding should be applied
#' @param operator Character. One of ">", "<", ">=", "<=", "==", "!=" to evaluate spatRaster cells against threshold value.
#' @param threshold Numeric or Character. threshold to evaluate spatRaster cells against using operator value.
#' @param returnValues Logical. Whether to return spatRaster of values matching thresholding (TRUE) or only logical raster indicating where thresholding is matched (FALSE).
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra nlyr
#'
#' @return A SpatRaster.
#' 
#' @author Erik Kusch
#'
#' @examples
#' Data_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1:31]]
#' Helper_Threshold(
#'     spatRaster = Data_rast,
#'     operator = "<",
#'     threshold = 2650
#' )
#' Helper_Threshold(
#'     spatRaster = Data_rast,
#'     operator = "<",
#'     threshold = 2650,
#'     returnValues = TRUE
#' )
Helper_Threshold <- function(spatRaster, operator, threshold, returnValues = FALSE, verbose = TRUE) {
    ## input check, needs rwriting to Helper.InputChecker
    if (!operator %in% c(">", "<", ">=", "<=", "==", "!=")) {
        stop("operator must be one of: '>', '<', '>=', '<=', '==', '!='")
    }

    ## progress bar
    pb <- Helper_Progress(iterLength = nlyr(spatRaster), text = "Applying Thresholding")

    ## going layer by layer to avoid error reported in https://github.com/rspatial/terra/issues/1556
    ThresholdedLayers_ls <- lapply(1:nlyr(spatRaster), FUN = function(Iter) {
        expr <- paste0("spatRaster[[Iter]] ", operator, " ", threshold)
        ret_rast <- eval(parse(text = expr))
        if (verbose) {
            pb$tick(tokens = list(layer = Iter))
        }
        if (returnValues) {
            ret_rast2 <- spatRaster[[Iter]]
            ret_rast2[!ret_rast] <- NA
            ret_rast <- ret_rast2
        }
        ret_rast
    })

    ## return fused layers as SpatRaster
    do.call(c, ThresholdedLayers_ls)
}
