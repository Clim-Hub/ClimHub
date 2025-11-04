#' @title Calculate ETCCDI Indices from List Input
#' 
#' @description This function calculates \href{https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/jgrd.50203}{ETCCDIs} from a named list of SpatRaster objects. Currently, only the first four ETCCDI are supported.
#'
#' @param ls Optional, list. List of SpatRasters. Names of elements must be "TX" and "TN", holding maximum and minimum daily air temperature respectively.
#'
#' @importFrom terra units
#' @importFrom terra varnames
#' @importFrom terra longnames
#' @importFrom terra time
#'
#' @return A named list of SpatRasters with each element corresponding to an ETCCDI.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' TX_rast <- terra::rast("inst/extdata/Jotunheimen_TX.nc") # terra::rast(system.file("extdata", "Jotunheimen_TX.nc", package = "ClimHub"))
#' TN_rast <- terra::rast("inst/extdata/Jotunheimen_TN.nc")
#' RR_rast <- terra::rast("inst/extdata/Jotunheimen_RR.nc")
#' # BASEPeriod_rast <- terra::rast("inst/extdata/Jotunheimen_BASEPeriod.nc")
#'
#' Metrics_ETCCDI(ls = list(TX = TX_rast, TN = TN_rast, RR = RR_rast
#' # , BASEPeriod = BASEPeriod_rast)
#' )
#' }
#'
#' @export
Metrics_ETCCDI <- function(ls) {
    ## validate specification
    InCheck_ls <- list(
        Unit_TX = list(
            Input = unique(units(ls$TX)),
            Allowed = c("K"),
            Operator = "in"
        ),
        Unit_TN = list(
            Input = unique(units(ls$TN)),
            Allowed = c("K"),
            Operator = "in"
        ),
        Unit_RR = list(
            Input = unique(units(ls$RR)),
            Allowed = c("mm"),
            Operator = "in"
        )
    )
    Helper_InputChecker(inputCheck = InCheck_ls)
    # should also check here for:
    #  1. temporal resolution being days
    #  2. time ranges being neat years
    #  3. time across all inputs in Rasters being the same
    #  4. Check that CRS is EPS:4326

    stop("Add extra checks")
    stop("do quantile calculations for percentile based ETCCDI")

    ## split into years
    AnnualRasters_ls <- lapply(ls, Helper_TimeSplit, tResolution = "Year")
    Dates <- as.POSIXct(paste0(unique(names(AnnualRasters_ls[[1]])), "-01-01"), tz = "UTC") ## merging lists into SpatRaster does not like numeric names of list elements

    ## ETCCDI
    ### Frost Days; Number of frost days: Annual count of days when TN (daily minimum temperature) < 0°C.
    message("===== FD - Number of frost days =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$TN, FUN = function(RastIter) {
        sum(Helper_Threshold(spatRaster = RastIter, threshold = 273.15, operator = "<"))
    })
    names(Thresholded_ls) <- NULL
    FD_rast <- do.call(c, Thresholded_ls)

    ### Summer Days; Number of summer days: Annual count of days when TX (daily maximum temperature) > 25°C.
    message("===== SU - Number of summer days =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$TX, FUN = function(RastIter) {
        sum(Helper_Threshold(spatRaster = RastIter, threshold = 273.15 + 25, operator = ">"))
    })
    names(Thresholded_ls) <- NULL
    SU_rast <- do.call(c, Thresholded_ls)

    ### Icing Days; Number of icing days: Annual count of days when TX (daily maximum temperature) < 0°C.
    message("===== ID - Number of icing days =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$TX, FUN = function(RastIter) {
        sum(Helper_Threshold(spatRaster = RastIter, threshold = 273.15, operator = "<"))
    })
    names(Thresholded_ls) <- NULL
    ID_rast <- do.call(c, Thresholded_ls)

    ### Tropical Nights; Number of tropical nights: Annual count of days when TN (daily minimum temperature) > 20°C.
    message("===== TR - Number of tropical nights =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$TN, FUN = function(RastIter) {
        sum(Helper_Threshold(spatRaster = RastIter, threshold = 273.15 + 20, operator = ">="))
    })
    names(Thresholded_ls) <- NULL
    TR_rast <- do.call(c, Thresholded_ls)

    ### GSL - Growing Season Length: Count the number of days between the first occurrence of at least 6 consecutive days with (TN+TX)/2 > 5°C and the first occurrence after 1st July (Northern Hemisphere) or 1st January (Southern Hemisphere) of at least 6 consecutive days with (TN+TX)/2 < 5°C
    message("===== GSL - Growing Season Length =====")
    message("Not implemented yet")

    ### TXx - Monthly Max of Daily Max Temp: Maximum daily maximum temperature in each month.
    message("===== TXx - Monthly Max of Daily Max Temp =====")
    TXx_rast <- Temporal_Aggregation(spatRaster = ls$TX, tResolution = "month", tStep = 1, fun = max)

    ### TNx - Monthly Max of Daily Min Temp: Maximum daily minimum temperature in each month.
    message("===== TNx - Monthly Max of Daily Min Temp =====")
    TNx_rast <- Temporal_Aggregation(spatRaster = ls$TN, tResolution = "month", tStep = 1, fun = max)

    ### TXn - Monthly Min of Daily Max Temp: Minimum daily maximum temperature in each month.
    message("===== TXn - Monthly Min of Daily Max Temp =====")
    TXn_rast <- Temporal_Aggregation(spatRaster = ls$TX, tResolution = "month", tStep = 1, fun = min)

    ### TNn - Monthly Min of Daily Min Temp: Minimum daily minimum temperature in each month.
    message("===== TNn - Monthly Min of Daily Min Temp =====")
    TNn_rast <- Temporal_Aggregation(spatRaster = ls$TN, tResolution = "month", tStep = 1, fun = min)

    ### TN10p - Percent Days TN < 10th Percentile: Percent of days, per year, where TN < 10th percentile of base period.
    message("===== TN10p - Percent Days TN < 10th Percentile =====")
    message("Not implemented yet")

    ### TX10p - Percent Days TX < 10th Percentile: Percent of days, per year, where TX < 10th percentile of base period.
    message("===== TX10p - Percent Days TX < 10th Percentile =====")
    message("Not implemented yet")

    ### TN90p - Percent Days TN > 90th Percentile: Percent of days, per year, where TN > 90th percentile of base period.
    message("===== TN90p - Percent Days TN > 90th Percentile =====")
    message("Not implemented yet")

    ### TX90p - Percent Days TX > 90th Percentile: Percent of days, per year, where TX > 90th percentile of base period.
    message("===== TX90p - Percent Days TX > 90th Percentile =====")
    message("Not implemented yet")

    ### WSDI - Warm Spell Duration Index: Annual count of days with 6+ consecutive days when TX > 90th percentile of base period.
    message("===== WSDI - Warm Spell Duration Index =====")
    message("Not implemented yet")

    ### CSDI - Cold Spell Duration Index: Annual count of days with 6+ consecutive days when TN < 10th percentile of base period.
    message("===== CSDI - Cold Spell Duration Index =====")
    message("Not implemented yet")

    ### DTR - Daily Temperature Range: Monthly mean difference between daily max (TX) and min (TN) temperatures.
    message("===== DTR - Daily Temperature Range =====")
    DTR_rast <- Temporal_Aggregation(spatRaster = ls$TX - ls$TN, tResolution = "month", tStep = 1, fun = mean)

    ### Rx1day - Max 1-day Precipitation per Month: Maximum precipitation in a single day each month.
    message("===== Rx1day - Max 1-day Precipitation per Month =====")
    Rx1day_rast <- Temporal_Aggregation(spatRaster = ls$RR, tResolution = "month", tStep = 1, fun = max)

    ### Rx5day - Max 5-day Precipitation per Month: Maximum precipitation over any 5 consecutive days in each month.
    message("===== Rx5day - Max 5-day Precipitation per Month =====")
    message("Not implemented yet")

    ### SDII - Simple Precipitation Intensity Index: Mean precipitation amount on wet days (RR ≥ 1mm).
    message("===== SDII - Simple Precipitation Intensity Index =====")
    message("Not yet tested")
    SDII_rast <- ls$RR
    SDII_rast[SDII_rast < 1] <- NA
    SDII_rast <- Temporal_Aggregation(spatRaster = SDII_rast, tResolution = "month", tStep = 1, fun = mean)

    ### R10mm - Days with Precip ≥ 10mm: Annual count of days with precipitation ≥ 10mm.
    message("===== R10mm - Days with Precip ≥ 10mm =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$RR, FUN = function(RastIter) {
        sum(Helper_Threshold(spatRaster = RastIter, threshold = 10, operator = ">="))
    })
    names(Thresholded_ls) <- NULL
    R10mm_rast <- do.call(c, Thresholded_ls)

    ### R20mm - Days with Precip ≥ 20mm: Annual count of days with precipitation ≥ 20mm.
    message("===== R20mm - Days with Precip ≥ 20mm =====")
    Thresholded_ls <- lapply(AnnualRasters_ls$RR, FUN = function(RastIter) {
        sum(Helper_Threshold(spatRaster = RastIter, threshold = 20, operator = ">="))
    })
    names(Thresholded_ls) <- NULL
    R20mm_rast <- do.call(c, Thresholded_ls)

    ### Rnnmm - Days with Precip ≥ user-defined threshold: Annual count of days with precipitation ≥ nnmm.
    message("===== Rnnmm - Days with Precip ≥ user-defined threshold =====")
    message("Not implemented yet")

    ### CDD - Consecutive Dry Days: Maximum number of consecutive days with RR < 1mm.
    message("===== CDD - Consecutive Dry Days =====")
    message("Not implemented yet")

    ### CWD - Consecutive Wet Days: Maximum number of consecutive days with RR ≥ 1mm.
    message("===== CWD - Consecutive Wet Days =====")
    message("Not implemented yet")

    ### R95pTOT - Annual Precip from RR > 95th Percentile: Total precipitation from wet days (RR > 95th percentile of base period).
    message("===== R95pTOT - Annual Precip from RR > 95th Percentile =====")
    message("Not implemented yet")

    ### R99pTOT - Annual Precip from RR > 99th Percentile: Total precipitation from wet days (RR > 99th percentile of base period).
    message("===== R99pTOT - Annual Precip from RR > 99th Percentile =====")
    message("Not implemented yet")

    ### PRCPTOT - Annual Total Precipitation on Wet Days: Sum of precipitation on wet days (RR ≥ 1mm) over a year.
    message("===== PRCPTOT - Annual Total Precipitation on Wet Days =====")
    message("Not yet tested")
    PRCPTOT_rast <- ls$RR
    PRCPTOT_rast[PRCPTOT_rast < 1] <- 0
    PRCPTOT_rast <- Temporal_Aggregation(spatRaster = PRCPTOT_rast, tResolution = "year", tStep = 1, fun = sum)

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
        SU = list("SU", "Number of summer days"),
        ID = list("ID", "Number of icing days"),
        TR = list("TR", "Number of tropical nights")
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