#' @title Calculate ETCCDI Indices from List Input
#'
#' @description This function calculates \href{https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/jgrd.50203}{ETCCDIs} from a named list of CFVariable objects. Currently, only some ETCCDI are supported.
#'
#' @param projectionList List. List of `CFVariable` objects. Names of elements must be "TX", "TN", and "RR", holding maximum and minimum daily air temperature and daily total precipitation, respectively. Note that these data must be recorded in "K", "K", and "mm", respectively.
#' @param baseLineList List. List of `CFDataset` objects. Names of elements must be "TX_Base", "TN_Base", and "RR_Base", holding lower and upper quantiles of maximum and minimum daily air temperature and daily total precipitation, respectively. Note that these data must be recorded in "K", "K", and "mm", respectively.
#' @param TResolution Character. Temporal resolution for calculation of ETCCDI. Supports "year" (default), "month" and "season".
#' @param RRThreshold Numeric. Custom threshold for daily precipiation in mm for calculation of Rnnmm. Defaults to 42.
#' @param fileName Character, optional. Character. A file name for the produced file, including path and ".nc" file ending.
#'
#' @importFrom ncdfCF as_CF
#'
#' @return A `CFDataset` containing a CFVariable for each ETCCDI. Each variable is named by its ETCCDI acronym and has a `long_name` attribute describing the index.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' ## these are CFVariable objects
#' TX <- NC_Read("inst/extdata/KiN_tasmax_2050.nc")[["tasmax"]]
#' TN <- NC_Read("inst/extdata/KiN_tasmin_2050.nc")[["tasmin"]]
#' RR <- NC_Read("inst/extdata/KiN_pr_2050.nc")[["pr"]] * 86400 # to get from mm/day to kg m-2 s-1
#' RR$set_attribute("units", "NC_CHAR", "mm")
#'
#' ## these are CFDataset objects with two variables each (1st and 2nd level of quantiles)
#' TX_Base <- NC_Read("inst/extdata/KiN_tasmax_BaseLineQuantiles.nc") # variables are 0.1 and 0.9 quantile baselines, in that order
#' TN_Base <- NC_Read("inst/extdata/KiN_tasmin_BaseLineQuantiles.nc") # variables are 0.1 and 0.9 quantile baselines, in that order
#' RR_Base <- NC_Read("inst/extdata/KiN_pr_BaseLineQuantiles.nc") # variables are 0.95 and 0.99 quantile baselines, in that order
#' for (i in names(RR_Base)) {
#'     RR_Base[[i]] * 86400 # to get from mm/day to kg m-2 s-1
#'     RR_Base[[i]]$set_attribute("units", "NC_CHAR", "mm")
#' }
#'
#' Metrics_ETCCDI(
#'     projectionList = list(TX = TX, TN = TN, RR = RR),
#'     baseLineList = list(TX_Base = TX_Base, TN_Base = TN_Base, RR_Base = RR_Base),
#'     TResolution = "year"
#' )
#'
#' Metrics_ETCCDI(
#'     projectionList = list(TX = TX, TN = TN, RR = RR),
#'     baseLineList = list(TX_Base = TX_Base, TN_Base = TN_Base, RR_Base = RR_Base),
#'     TResolution = "month"
#' )
#'
#' Metrics_ETCCDI(
#'     projectionList = list(TX = TX, TN = TN, RR = RR),
#'     baseLineList = list(TX_Base = TX_Base, TN_Base = TN_Base, RR_Base = RR_Base),
#'     TResolution = "season",
#'     fileName = "ETCCDISeasons.nc"
#' )
#' }
#' @export
Metrics_ETCCDI <- function(projectionList, baseLineList, TResolution = "year", RRThreshold = 42, fileName = NULL) {
    ## Setting uo Progress Bar
    pb <- Helper_Progress(iterLength = 27, text = "ETCCDI Calculation")

    ## fileName handling
    if (missing(fileName)) {
        fileName <- NULL
    }
    if (!is.null(fileName)) {
        fileName <- normalizePath(fileName, mustWork = FALSE)
    }

    ## File Check =========
    if (!is.null(fileName)) {
        FCheck <- Helper_FileCheck(fileName = fileName, loadFun = NC_Read, load = TRUE, verbose = TRUE)
        if (!is.null(FCheck)) {
            return(FCheck)
        }
    }

    ## summary functions
    ### count maximum run of 1s (value 1 represents a value exceeding a given threshold supplied to and evaluated by Helper_Threshold)
    max_run_of_ones <- function(x) {
        if (length(na.omit(x)) == 0) {
            return(NA_real_)
        }
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

    ### sum number of days in runs exceeding certain threshold of length
    sum_run_of_ones <- function(x, thresh = 6) {
        if (length(na.omit(x)) == 0) {
            return(NA_real_)
        }
        x[is.na(x)] <- 0 # convert NAs to 0s as NAs break the computation
        if (length(x) == 0) {
            return(NA_real_)
        }
        r <- rle(x == 1) # run‑length encode logical test
        if (any(r$values)) { # any runs of 1
            sum(r$lengths[r$values][r$lengths[r$values] > 6])
        } else {
            0
        } # length of longest TRUE run
    }

    ### compute maximum of 5 day intervals
    max_sum_over_5 <- function(x) {
        if (length(na.omit(x)) == 0) {
            return(NA_real_)
        }
        x[is.na(x)] <- 0 # keep the NA‑to‑0 convention if you like
        n <- length(x)
        if (n < 5) {
            return(NA_real_)
        }
        best <- -Inf
        for (i in seq_len(n - 5 + 1)) {
            best <- max(best, sum(x[i:(i + 4)], na.rm = TRUE))
        }
        best
    }

    ### percentage of exceeding values
    percentage_non_na <- function(x) {
        x <- na.omit(x)
        if (length(x) == 0) {
            return(NA_real_)
        } else {
            return((sum(x) / length(x)) * 100)
        }
    }

    ### regular functions ignoring NAs
    sum_non_na <- function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) {
            NA_real_
        } else {
            sum(x, na.rm = TRUE)
        }
    }
    mean_non_na <- function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) {
            NA_real_
        } else {
            mean(x, na.rm = TRUE)
        }
    }
    max_non_na <- function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) {
            NA_real_
        } else {
            max(x, na.rm = TRUE)
        }
    }
    min_non_na <- function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) {
            NA_real_
        } else {
            min(x, na.rm = TRUE)
        }
    }

    ## get list contents
    list2env(projectionList, env = environment())
    list2env(baseLineList, env = environment())

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
    #  5. same as above for elements of baseLineList

    # stop("Add extra checks")
    # stop("do quantile calculations for percentile based ETCCDI")

    ## ETCCDI
    ### Frost Days; Number of frost days: Annual count of days when TN (daily minimum temperature) < 0°C.
    # message("===== FD - Number of frost days =====")
    FD <- Helper_Threshold(
        CFVariable = TN,
        operator = "<",
        threshold = 273.15,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 1))

    ### Summer Days; Number of summer days: Annual count of days when TX (daily maximum temperature) > 25°C.
    # message("===== SU - Number of summer days =====")
    SU <- Helper_Threshold(
        TX,
        operator = ">",
        threshold = 273.15 + 25,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 2))

    ### Icing Days; Number of icing days: Annual count of days when TX (daily maximum temperature) < 0°C.
    # message("===== ID - Number of icing days =====")
    ID <- Helper_Threshold(
        TX,
        operator = "<",
        threshold = 273.15,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 3))

    ### Tropical Nights; Number of tropical nights: Annual count of days when TN (daily minimum temperature) > 20°C.
    # message("===== TR - Number of tropical nights =====")
    TR <- Helper_Threshold(
        TN,
        operator = ">",
        threshold = 273.15 + 20,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 4))

    ### GSL - Growing Season Length: Count the number of days between the first occurrence of at least 6 consecutive days with (TN+TX)/2 > 5°C and the first occurrence after 1st July (Northern Hemisphere) or 1st January (Southern Hemisphere) of at least 6 consecutive days with (TN+TX)/2 < 5°C
    # message("===== GSL - Growing Season Length =====")
    TM <- (TN + TX) / 2
    GSL <- Helper_ETCCDIGSL(TM)
    pb$tick(tokens = list(layer = 5))

    ### TXx - Monthly Max of Daily Max Temp: Maximum daily maximum temperature in each month.
    # message("===== TXx - Monthly Max of Daily Max Temp =====")
    TXx <- TX$summarise(
        "Tresholded",
        max_non_na,
        TResolution
    )
    pb$tick(tokens = list(layer = 6))

    ### TNx - Monthly Max of Daily Min Temp: Maximum daily minimum temperature in each month.
    # message("===== TNx - Monthly Max of Daily Min Temp =====")
    TNx <- TN$summarise(
        "Tresholded",
        max_non_na,
        TResolution
    )
    pb$tick(tokens = list(layer = 7))

    ### TXn - Monthly Min of Daily Max Temp: Minimum daily maximum temperature in each month.
    # message("===== TXn - Monthly Min of Daily Max Temp =====")
    TXn <- TX$summarise(
        "Tresholded",
        min_non_na,
        TResolution
    )
    pb$tick(tokens = list(layer = 8))

    ### TNn - Monthly Min of Daily Min Temp: Minimum daily minimum temperature in each month.
    # message("===== TNn - Monthly Min of Daily Min Temp =====")
    TNn <- TN$summarise(
        "Tresholded",
        min_non_na,
        TResolution
    )
    pb$tick(tokens = list(layer = 9))

    ### TN10p - Percent Days TN < 10th Percentile: Percent of days, per year, where TN < 10th percentile of base period.
    # message("===== TN10p - Percent Days TN < 10th Percentile =====")
    TN10p <- Helper_Threshold(
        TN,
        operator = "<",
        threshold = TN_Base[[names(TN_Base)[1]]],
        threshMode = "ETCCDIQuantiles",
        returnValues = FALSE,
        returnSummary = percentage_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 10))

    ### TX10p - Percent Days TX < 10th Percentile: Percent of days, per year, where TX < 10th percentile of base period.
    # message("===== TX10p - Percent Days TX < 10th Percentile =====")
    TX10p <- Helper_Threshold(
        TX,
        operator = "<",
        threshold = TX_Base[[names(TX_Base)[1]]],
        threshMode = "ETCCDIQuantiles",
        returnValues = FALSE,
        returnSummary = percentage_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 11))

    ### TN90p - Percent Days TN > 90th Percentile: Percent of days, per year, where TN > 90th percentile of base period.
    # message("===== TN90p - Percent Days TN > 90th Percentile =====")
    TN90p <- Helper_Threshold(
        TN,
        operator = ">",
        threshold = TN_Base[[names(TN_Base)[2]]],
        threshMode = "ETCCDIQuantiles",
        returnValues = FALSE,
        returnSummary = percentage_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 12))

    ### TX90p - Percent Days TX > 90th Percentile: Percent of days, per year, where TX > 90th percentile of base period.
    # message("===== TX90p - Percent Days TX > 90th Percentile =====")
    TX90p <- Helper_Threshold(
        TX,
        operator = ">",
        threshold = TX_Base[[names(TX_Base)[2]]],
        threshMode = "ETCCDIQuantiles",
        returnValues = FALSE,
        returnSummary = percentage_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 13))

    ### WSDI - Warm Spell Duration Index: Annual count of days with 6+ consecutive days when TX > 90th percentile of base period.
    # message("===== WSDI - Warm Spell Duration Index =====")
    WSDI <- Helper_Threshold(
        TX,
        operator = ">",
        threshold = TX_Base[[names(TX_Base)[2]]],
        threshMode = "ETCCDIQuantiles",
        returnValues = FALSE,
        returnSummary = sum_run_of_ones,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 14))

    ### CSDI - Cold Spell Duration Index: Annual count of days with 6+ consecutive days when TN < 10th percentile of base period.
    # message("===== CSDI - Cold Spell Duration Index =====")
    CSDI <- Helper_Threshold(
        TN,
        operator = "<",
        threshold = TX_Base[[names(TX_Base)[1]]],
        threshMode = "ETCCDIQuantiles",
        returnValues = FALSE,
        returnSummary = sum_run_of_ones,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 15))

    ### DTR - Daily Temperature Range: Monthly mean difference between daily max (TX) and min (TN) temperatures.
    # message("===== DTR - Daily Temperature Range =====")
    DTR <- TX - TN # create daily range
    DTR <- DTR$summarise(
        "Tresholded",
        mean_non_na,
        TResolution
    )
    pb$tick(tokens = list(layer = 16))

    ### Rx1day - Max 1-day Precipitation per Month: Maximum precipitation in a single day each month.
    # message("===== Rx1day - Max 1-day Precipitation per Month =====")
    Rx1day <- RR$summarise(
        "Tresholded",
        max_non_na,
        TResolution
    )
    pb$tick(tokens = list(layer = 17))

    ### Rx5day - Max 5-day Precipitation per Month: Maximum precipitation over any 5 consecutive days in each month.
    # message("===== Rx5day - Max 5-day Precipitation per Month =====")
    Rx5day <- RR$summarise(
        "Tresholded",
        max_sum_over_5,
        TResolution
    )
    pb$tick(tokens = list(layer = 18))

    ### SDII - Simple Precipitation Intensity Index: Mean precipitation amount on wet days (RR ≥ 1mm).
    # message("===== SDII - Simple Precipitation Intensity Index =====")
    SDII <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 1,
        returnValues = TRUE,
        returnSummary = mean_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 19))

    ### R10mm - Days with Precip ≥ 10mm: Annual count of days with precipitation ≥ 10mm.
    # message("===== R10mm - Days with Precip ≥ 10mm =====")
    R10mm <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 10,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 20))

    ### R20mm - Days with Precip ≥ 20mm: Annual count of days with precipitation ≥ 20mm.
    # message("===== R20mm - Days with Precip ≥ 20mm =====")
    R20mm <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 20,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 21))

    ### Rnnmm - Days with Precip ≥ user-defined threshold: Annual count of days with precipitation ≥ nnmm.
    # message("===== Rnnmm - Days with Precip ≥ user-defined threshold =====")
    Rnnmm <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = RRThreshold,
        returnValues = FALSE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 22))

    ### CDD - Consecutive Dry Days: Maximum number of consecutive days with RR < 1mm.
    # message("===== CDD - Consecutive Dry Days =====")
    CDD <- Helper_Threshold(
        RR,
        operator = "<",
        threshold = 1,
        returnValues = FALSE,
        returnSummary = max_run_of_ones,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 23))

    ### CWD - Consecutive Wet Days: Maximum number of consecutive days with RR ≥ 1mm.
    # message("===== CWD - Consecutive Wet Days =====")
    CWD <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 1,
        returnValues = FALSE,
        returnSummary = max_run_of_ones,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 24))

    ### R95pTOT - Annual Precip from RR > 95th Percentile: Total precipitation from wet days (RR > 95th percentile of base period).
    # message("===== R95pTOT - Annual Precip from RR > 95th Percentile =====")
    R95pTOT <- Helper_Threshold(
        RR,
        operator = ">",
        threshold = RR_Base[[names(RR_Base)[1]]],
        threshMode = "ETCCDIQuantiles",
        returnValues = TRUE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 25))

    ### R99pTOT - Annual Precip from RR > 99th Percentile: Total precipitation from wet days (RR > 99th percentile of base period).
    # message("===== R99pTOT - Annual Precip from RR > 99th Percentile =====")
    R99pTOT <- Helper_Threshold(
        RR,
        operator = ">",
        threshold = RR_Base[[names(RR_Base)[1]]],
        threshMode = "ETCCDIQuantiles",
        returnValues = TRUE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 26))

    ### PRCPTOT - Annual Total Precipitation on Wet Days: Sum of precipitation on wet days (RR ≥ 1mm) over a year.
    # message("===== PRCPTOT - Annual Total Precipitation on Wet Days =====")
    RCPTOT <- Helper_Threshold(
        RR,
        operator = ">=",
        threshold = 1,
        returnValues = TRUE,
        returnSummary = sum_non_na,
        returnTResolution = TResolution
    )
    pb$tick(tokens = list(layer = 27))

    ## return
    ## Fusing objects
    Return_ls <- list(
        FD = FD,
        SU = SU,
        ID = ID,
        TR = TR,
        GSL = GSL,
        TXx = TXx,
        TNx = TNx,
        TXn = TXn,
        TNn = TNn,
        TN10p = TN10p,
        TX10p = TX10p,
        TN90p = TN90p,
        TX90p = TX90p,
        WSDI = WSDI,
        CSDI = CSDI,
        DTR = DTR,
        Rx1day = Rx1day,
        Rx5day = Rx5day,
        SDII = SDII,
        R10 = R10mm,
        R20 = R20mm,
        Rnnmm = Rnnmm,
        CDD = CDD,
        CWD = CWD,
        R95pTOT = R95pTOT,
        R99pTOT = R99pTOT,
        RCPTOT = RCPTOT
    )

    ## Combine into a single CFDataset with appropriate variable names + long_name attributes
    long_names <- c(
        FD = "Number of frost days",
        SU = "Number of summer days",
        ID = "Number of icing days",
        TR = "Number of tropical nights",
        GSL = "Growing Season Length",
        TXx = "Max of Daily Max Temp",
        TNx = "Max of Daily Min Temp",
        TXn = "Min of Daily Max Temp",
        TNn = "Min of Daily Min Temp",
        TN10p = "Percent Days TN < 10th Percentile",
        TX10p = "Percent Days TX < 10th Percentile",
        TN90p = "Percent Days TN > 90th Percentile",
        TX90p = "Percent Days TX > 90th Percentile",
        WSDI = "Warm Spell Duration Index",
        CSDI = "Cold Spell Duration Index",
        DTR = "Daily Temperature Range",
        Rx1day = "Max 1-day Precipitation",
        Rx5day = "Max 5-day Precipitation",
        SDII = "Simple Precipitation Intensity Index",
        R10 = "Days with Precip ≥ 10mm",
        R20 = "Days with Precip ≥ 20mm",
        Rnnmm = "Days with Precip ≥ user-defined threshold",
        CDD = "Consecutive Dry Days",
        CWD = "Consecutive Wet Days",
        R95pTOT = "Total Precipitation from RR > 95th Percentile",
        R99pTOT = "Total Precipitation from RR > 99th Percentile",
        RCPTOT = "Total Precipitation on Wet Days"
    )

    ds <- ncdfCF::create_ncdf()
    for (nm in names(Return_ls)) { # this loops over the CFVariables, removes their attributes and sets correct names
        cfvar <- Return_ls[[nm]]
        raw <- cfvar$raw()
        new_var <- ncdfCF::as_CF(nm, raw)

        if (nm %in% names(long_names)) {
            new_var$set_attribute("long_name", "NC_CHAR", long_names[[nm]])
        }

        ds$add_variable(new_var)
    }

    ## optionally write dataset to disk
    if (!is.null(fileName)) {
        ds$save(fileName)
        ds <- NC_Read(fileName)
    }

    ## return dataset to user
    ds
}
