#' CALCULATE EXPERT TEAM CLIMATE CHANGE DETECTION INDICES (ETCCDI) METRICS ======================================================
#' Calculate ETCCDI indices from list input
#'
#' This function calculates \href{https://etccdi.pacificclimate.org/list_27_indices.shtml}{ETCCDIs} from a named list of SpatRaster objects. Currently, only the first four ETCCDI are supported.
#'
#' @param Rasters List of SpatRasters. Names of elements bust be "TX" and "TN", holding maximum and minimum daily air temperature respectively.
#'
#' @importFrom terra units
#' @importFrom terra varnames
#' @importFrom terra longnames
#' @importFrom terra time
#'
#' @return A named list of SpatRasters with each element corresponding to an ETCCDI.
#'
#'
#' @examples
#' TX_rast <- Download.KlimaiNorge2100(
#'     Variable = "Maximum Air Temperature",
#'     DateStart = "1995-01-01",
#'     DateStop = "1996-12-31",
#'     Model = "CNRM_CCLM",
#'     FileName = "KiN_TX",
#' )
#'
#' TN_rast <- Download.KlimaiNorge2100(
#'     Variable = "Minimum Air Temperature",
#'     DateStart = "1995-01-01",
#'     DateStop = "1996-12-31",
#'     Model = "CNRM_CCLM",
#'     FileName = "KiN_TN",
#' )
#'
#' Metrics.ETCCDI(Rasters = list(TX = TX_rast, TN = TN_rast))
#'
#' @export
Metrics.ETCCDI <- function(Rasters) {
    ## validate specification
    InCheck_ls <- list(
        Unit_TX = list(
            Input = unique(units(Rasters$TX)),
            Allowed = c("K"),
            Operator = "in"
        ),
        Unit_TN = list(
            Input = unique(units(Rasters$TN)),
            Allowed = c("K"),
            Operator = "in"
        )
    )
    Helper.InputChecker(InCheck_ls)
    #' shoudl also check here for:
    #'  1. temporal resolution being days
    #'  2. time ranges being neat years
    #'  3. time across all inputs in Rasters being the same

    ## split into years
    AnnualRasters_ls <- lapply(Rasters, Helper.TimeSplit, TimeSplit = "Year")
    Dates <- as.POSIXct(paste0(unique(names(AnnualRasters_ls[[1]])), "-01-01"), tz = "UTC") ## merging lists into SpatRaster does not like numeric names of list elements

    ## ETCCDI
    ### Frost Days
    message("===== FD - Number of frost days =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$TN, FUN = function(RastIter) {
        sum(Helper.Threshold(Raster = RastIter, Threshold = 273.15, Operator = "<"))
    })
    names(Thresholded_ls) <- NULL
    FD_rast <- do.call(c, Thresholded_ls)

    ### Summer Days
    message("===== SU - Number of summer days =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$TX, FUN = function(RastIter) {
        sum(Helper.Threshold(Raster = RastIter, Threshold = 273.15 + 25, Operator = ">="))
    })
    names(Thresholded_ls) <- NULL
    SU_rast <- do.call(c, Thresholded_ls)

    ### Icing Days
    message("===== ID - Number of icing days =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$TX, FUN = function(RastIter) {
        sum(Helper.Threshold(Raster = RastIter, Threshold = 273.15, Operator = "<"))
    })
    names(Thresholded_ls) <- NULL
    ID_rast <- do.call(c, Thresholded_ls)

    ### Tropical Nights
    message("===== TR - Number of tropical nights =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$TN, FUN = function(RastIter) {
        sum(Helper.Threshold(Raster = RastIter, Threshold = 273.15 + 20, Operator = ">"))
    })
    names(Thresholded_ls) <- NULL
    TR_rast <- do.call(c, Thresholded_ls)

    ## return
    ## Fusing objects
    Return_ls <- list(
        FD = FD_rast,
        SU = SU_rast,
        ID = ID_rast,
        TR = TR_rast
    )

    ### Ascribing proper Names and Dates
    VarNames_ls <- list(
        FD = list("FD", "Number of frost days"),
        SU = list("FD", "Number of summer days"),
        ID = list("FD", "Number of iciing days"),
        TR = list("FD", "Number of tropical nights")
    )

    Return_ls <- lapply(1:length(Return_ls), FUN = function(Iter) {
        terra::varnames(Return_ls[[Iter]]) <- VarNames_ls[[Iter]][[1]]
        terra::longnames(Return_ls[[Iter]]) <- VarNames_ls[[Iter]][[2]]
        terra::time(Return_ls[[Iter]]) <- Dates
        Return_ls[[Iter]]
    })
    names(Return_ls) <- names(VarNames_ls)

    return(Return_ls)
}
