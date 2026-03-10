#' @title Calculate ETCCDI Indices from List Input
#'
#' @description This function calculates \href{https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/jgrd.50203}{ETCCDIs} from a named list of CFVariable objects. Currently, only some ETCCDI are supported.
#'
#' @param projectionList List. List of `CFVariable` objects. Names of elements must be "TX", "TN", and "RR", holding maximum and minimum daily air temperature and daily total precipitation, respectively. Note that these data must be recorded in "K", "K", and "mm", respectively.
#' @param TResolution Character. Temporal resolution for calculation of ETCCDI. Supports "year" (default), "month" and "season".
#' @param RRThreshold Numeric. Custom threshold for daily precipiation in mm for calculation of Rnnmm. Defaults to 42.
#'
#' @return A named list of `CFVariable` objects with each element corresponding to an ETCCDI.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' TX_CF <- NC_Read("inst/extdata/KiN_tx_2050.nc")[["tasmax"]]
#' TN_CF <- NC_Read("inst/extdata/KiN_tn_2050.nc")[["tasmin"]]
#' RR_CF <- NC_Read("inst/extdata/KiN_prc_2050.nc")[["pr"]] * 86400 # to get from mm/day to kg m-2 s-1
#' RR_CF$set_attribute("units", "NC_CHAR", "mm")
#'
#' Metrics_ETCCDI(projectionList = list(TX = TX_CF, TN = TN_CF, RR = RR_CF), TResolution = "year")
#' Metrics_ETCCDI(projectionList = list(TX = TX_CF, TN = TN_CF, RR = RR_CF), TResolution = "month")
#' Metrics_ETCCDI(projectionList = list(TX = TX_CF, TN = TN_CF, RR = RR_CF), TResolution = "season")
#' }
#'
#' @export
Metrics_ETCCDI <- function(projectionList, TResolution = "year", RRThreshold = 42) {
    ## summary functions
    ### count maximum run of 1s (value 1 represents a value exceeding a given threshold supplied to and evaluated by Helper_Threshold)
    max_run_of_ones <- function(x) {
        x[is.na(x)] <- 0 # convert NAs to 0s as NAs break the computation
        if (length(x) == 0) {
            return(NA_real_)
        }
        r <- rle(x == 1) # run‑length encode logical test
        if (any(r$values)) { # any runs of 1
            max(r$lengths[r$values])
        } else {
            0
        } # length of longest TRUE run
    }

    ## regular functions ignoring NAs
    sum_non_na <- function(x) sum(x, na.rm = TRUE)
    mean_non_na <- function(x) mean(x, na.rm = TRUE)
    max_non_na <- function(x) max(x, na.rm = TRUE)
    min_non_na <- function(x) min(x, na.rm = TRUE)

    ## get list contents
    list2env(projectionList, env = environment())

    ## validate specification
    InCheck_ls <- list(
        Unit_Projection_TX = list(
            Input = unlist(TX$attributes$value[TX$attributes$name == "units"]),
            Allowed = c("K"),
            Operator = "in"
        ),
        Unit_Projection_TN = list(
            Input = unlist(TN$attributes$value[TN$attributes$name == "units"]),
            Allowed = c("K"),
            Operator = "in"
        ),
        Unit_Projection_RR = list(
            Input = unlist(RR$attributes$value[RR$attributes$name == "units"]),
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

    # stop("Add extra checks")
    # stop("do quantile calculations for percentile based ETCCDI")

    ## ETCCDI
    ### Frost Days; Number of frost days: Annual count of days when TN (daily minimum temperature) < 0°C.
    message("===== FD - Number of frost days =====")
    FD <- Helper_Threshold(
        TN,
        operator = "<",
        threshold = 273.15,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )

    ### Summer Days; Number of summer days: Annual count of days when TX (daily maximum temperature) > 25°C.
    message("===== SU - Number of summer days =====")
    SU <- Helper_Threshold(
        TX,
        operator = ">",
        threshold = 273.15 + 25,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )

    ### Icing Days; Number of icing days: Annual count of days when TX (daily maximum temperature) < 0°C.
    message("===== ID - Number of icing days =====")
    ID <- Helper_Threshold(
        TX,
        operator = "<",
        threshold = 273.15,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )

    ### Tropical Nights; Number of tropical nights: Annual count of days when TN (daily minimum temperature) > 20°C.
    message("===== TR - Number of tropical nights =====")
    TR <- Helper_Threshold(
        TN,
        operator = ">",
        threshold = 273.15 + 20,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )

    ### GSL - Growing Season Length: Count the number of days between the first occurrence of at least 6 consecutive days with (TN+TX)/2 > 5°C and the first occurrence after 1st July (Northern Hemisphere) or 1st January (Southern Hemisphere) of at least 6 consecutive days with (TN+TX)/2 < 5°C
    message("===== GSL - Growing Season Length =====")
    print("Not implemented yet")

    ### TXx - Monthly Max of Daily Max Temp: Maximum daily maximum temperature in each month.
    message("===== TXx - Monthly Max of Daily Max Temp =====")
    TXx <- TX$summarise(
        "Tresholded",
        max_non_na,
        TResolution
    )

    ### TNx - Monthly Max of Daily Min Temp: Maximum daily minimum temperature in each month.
    message("===== TNx - Monthly Max of Daily Min Temp =====")
    TNx <- TN$summarise(
        "Tresholded",
        max_non_na,
        TResolution
    )

    ### TXn - Monthly Min of Daily Max Temp: Minimum daily maximum temperature in each month.
    message("===== TXn - Monthly Min of Daily Max Temp =====")
    TXn <- TX$summarise(
        "Tresholded",
        min_non_na,
        TResolution
    )

    ### TNn - Monthly Min of Daily Min Temp: Minimum daily minimum temperature in each month.
    message("===== TNn - Monthly Min of Daily Min Temp =====")
    TNn <- TN$summarise(
        "Tresholded",
        min_non_na,
        TResolution
    )

    ### TN10p - Percent Days TN < 10th Percentile: Percent of days, per year, where TN < 10th percentile of base period.
    message("===== TN10p - Percent Days TN < 10th Percentile =====")
    print("Not implemented yet")

    ### TX10p - Percent Days TX < 10th Percentile: Percent of days, per year, where TX < 10th percentile of base period.
    message("===== TX10p - Percent Days TX < 10th Percentile =====")
    print("Not implemented yet")

    ### TN90p - Percent Days TN > 90th Percentile: Percent of days, per year, where TN > 90th percentile of base period.
    message("===== TN90p - Percent Days TN > 90th Percentile =====")
    print("Not implemented yet")

    ### TX90p - Percent Days TX > 90th Percentile: Percent of days, per year, where TX > 90th percentile of base period.
    message("===== TX90p - Percent Days TX > 90th Percentile =====")
    print("Not implemented yet")

    ### WSDI - Warm Spell Duration Index: Annual count of days with 6+ consecutive days when TX > 90th percentile of base period.
    message("===== WSDI - Warm Spell Duration Index =====")
    # TX_array <- TX$raw()
    print("Not implemented yet")

    ### CSDI - Cold Spell Duration Index: Annual count of days with 6+ consecutive days when TN < 10th percentile of base period.
    message("===== CSDI - Cold Spell Duration Index =====")
    print("Not implemented yet")

    ### DTR - Daily Temperature Range: Monthly mean difference between daily max (TX) and min (TN) temperatures.
    message("===== DTR - Daily Temperature Range =====")
    TR <- TX - TN # create daily range
    DTR <- TR$summarise(
        "Tresholded",
        mean_non_na,
        TResolution
    )

    ### Rx1day - Max 1-day Precipitation per Month: Maximum precipitation in a single day each month.
    message("===== Rx1day - Max 1-day Precipitation per Month =====")
    Rx1day <- RR$summarise(
        "Tresholded",
        max_non_na,
        TResolution
    )

    ### Rx5day - Max 5-day Precipitation per Month: Maximum precipitation over any 5 consecutive days in each month.
    message("===== Rx5day - Max 5-day Precipitation per Month =====")
    print("Not implemented yet")
    RR_array <- RR$raw()


    ### SDII - Simple Precipitation Intensity Index: Mean precipitation amount on wet days (RR ≥ 1mm).
    message("===== SDII - Simple Precipitation Intensity Index =====")
    SDII <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 1,
        returnValues = TRUE,
        returnSummary = mean_non_na,
        returnTResolution = TResolution
    )

    ### R10mm - Days with Precip ≥ 10mm: Annual count of days with precipitation ≥ 10mm.
    message("===== R10mm - Days with Precip ≥ 10mm =====")
    R10mm <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 10,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )

    ### R20mm - Days with Precip ≥ 20mm: Annual count of days with precipitation ≥ 20mm.
    message("===== R20mm - Days with Precip ≥ 20mm =====")
    R20mm <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 20,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )

    ### Rnnmm - Days with Precip ≥ user-defined threshold: Annual count of days with precipitation ≥ nnmm.
    message("===== Rnnmm - Days with Precip ≥ user-defined threshold =====")
    Rnnmm <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = RRThreshold,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )

    ### CDD - Consecutive Dry Days: Maximum number of consecutive days with RR < 1mm.
    message("===== CDD - Consecutive Dry Days =====")
    CDD <- Helper_Threshold(
        RR,
        operator = "<",
        threshold = 1,
        returnValues = FALSE,
        returnSummary = max_run_of_ones,
        returnTResolution = TResolution
    )

    ### CWD - Consecutive Wet Days: Maximum number of consecutive days with RR ≥ 1mm.
    message("===== CWD - Consecutive Wet Days =====")
    CWD <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 1,
        returnValues = FALSE,
        returnSummary = max_run_of_ones,
        returnTResolution = TResolution
    )

    ### R95pTOT - Annual Precip from RR > 95th Percentile: Total precipitation from wet days (RR > 95th percentile of base period).
    message("===== R95pTOT - Annual Precip from RR > 95th Percentile =====")
    print("Not implemented yet")

    ### R99pTOT - Annual Precip from RR > 99th Percentile: Total precipitation from wet days (RR > 99th percentile of base period).
    message("===== R99pTOT - Annual Precip from RR > 99th Percentile =====")
    print("Not implemented yet")

    ### PRCPTOT - Annual Total Precipitation on Wet Days: Sum of precipitation on wet days (RR ≥ 1mm) over a year.
    message("===== PRCPTOT - Annual Total Precipitation on Wet Days =====")
    RCPTOT <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 1,
        returnValues = TRUE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )

    ## return
    ## Fusing objects
    Return_ls <- list(
        FD = FD,
        SU = SU,
        ID = ID,
        TR = TR,
        # GSL = GSL,
        TXx = TXx,
        TNx = TNx,
        TXn = TXn,
        TNn = TNn,
        # TN10p = TN10p,
        # TX10p = TX10p,
        # TN90p = TN90p,
        # TX90p = TX90p,
        # WSDI = WSDI,
        # CSDI = CSDI,
        DTR = DTR,
        Rx1day = Rx1day,
        # Rx5day = Rx5day,
        SDII = SDII,
        R10 = R10mm,
        R20 = R20mm,
        Rnnmm = Rnnmm,
        CDD = CDD,
        CWD = CWD,
        # R95pTOT = R95pTOT,
        # R99pTOT = R99pTOT,
        RCPTOT = RCPTOT
    )

    return(Return_ls)
}
