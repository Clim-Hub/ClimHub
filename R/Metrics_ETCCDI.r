#' @title Calculate ETCCDI Indices from List Input
#'
#' @description This function calculates \href{https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/jgrd.50203}{ETCCDIs} from a named list of CFVariable objects. Currently, only some ETCCDI are supported.
#'
#' @details Input requirements are conditional on the selected ETCCDI indices in `indices`.
#' To reduce memory usage and speed up calculations, supply only the inputs required for the selected indices.
#'
#' | ETCCDI | Full name | Required data input | Verbose description | Unit | Default temporal resolution |
#' |:--|:--|:--|:--|:--|:--|
#' | FD | Number of frost days | TN | TODO | TODO | TODO |
#' | SU | Number of summer days | TX | TODO | TODO | TODO |
#' | ID | Number of icing days | TX | TODO | TODO | TODO |
#' | TR | Number of tropical nights | TN | TODO | TODO | TODO |
#' | GSL | Growing Season Length | TX, TN | TODO | TODO | TODO |
#' | TXx | Max of Daily Max Temp | TX | TODO | TODO | TODO |
#' | TNx | Max of Daily Min Temp | TN | TODO | TODO | TODO |
#' | TXn | Min of Daily Max Temp | TX | TODO | TODO | TODO |
#' | TNn | Min of Daily Min Temp | TN | TODO | TODO | TODO |
#' | TN10p | Percent Days TN < 10th Percentile | TN, TN_Base | TODO | TODO | TODO |
#' | TX10p | Percent Days TX < 10th Percentile | TX, TX_Base | TODO | TODO | TODO |
#' | TN90p | Percent Days TN > 90th Percentile | TN, TN_Base | TODO | TODO | TODO |
#' | TX90p | Percent Days TX > 90th Percentile | TX, TX_Base | TODO | TODO | TODO |
#' | WSDI | Warm Spell Duration Index | TX, TX_Base | TODO | TODO | TODO |
#' | CSDI | Cold Spell Duration Index | TN, TX_Base | TODO | TODO | TODO |
#' | DTR | Daily Temperature Range | TX, TN | TODO | TODO | TODO |
#' | Rx1day | Max 1-day Precipitation | RR | TODO | TODO | TODO |
#' | Rx5day | Max 5-day Precipitation | RR | TODO | TODO | TODO |
#' | SDII | Simple Precipitation Intensity Index | RR | TODO | TODO | TODO |
#' | R10mm | Days with Precip ≥ 10mm | RR | TODO | TODO | TODO |
#' | R20mm | Days with Precip ≥ 20mm | RR | TODO | TODO | TODO |
#' | Rnnmm | Days with Precip ≥ user-defined threshold | RR | TODO | TODO | TODO |
#' | CDD | Consecutive Dry Days | RR | TODO | TODO | TODO |
#' | CWD | Consecutive Wet Days | RR | TODO | TODO | TODO |
#' | R95pTOT | Total Precipitation from RR > 95th Percentile | RR, RR_Base | TODO | TODO | TODO |
#' | R99pTOT | Total Precipitation from RR > 99th Percentile | RR, RR_Base | TODO | TODO | TODO |
#' | RCPTOT | Total Precipitation on Wet Days | RR | TODO | TODO | TODO |
#'
#' @param projectionList List. List of `CFVariable` objects required by the selected ETCCDI indices. Include only named elements that are needed for `indices`: "TX" (daily maximum air temperature, K), "TN" (daily minimum air temperature, K), and/or "RR" (daily total precipitation, mm).
#' @param baseLineList Optional, list. List of `CFDataset` objects containing baseline quantiles required only for quantile-based ETCCDI indices. Include only named elements needed for `indices`: "TX_Base", "TN_Base", and/or "RR_Base". If no quantile-based ETCCDI is selected, this argument can be omitted.
#' @param TResolution Character. Temporal resolution for calculation of ETCCDI. Supports "year" (default), "month" and "season".
#' @param RRThreshold Numeric. Custom threshold for daily precipiation in mm for calculation of Rnnmm. Defaults to 42.
#' @param indices Optional, character. Character vector of ETCCDI abbreviations to calculate. If missing, all supported indices in this function are calculated.
#' @param fileName Character, optional. Character. A file name for the produced file, including path and ".nc" file ending. If no value is supplied, the dataset is not written to disk but returned as a `CFDataset` object in memory. If a file name is supplied and a file with that name already exists, the function will attempt to load and return that file instead of recalculating the indices.
#'
#' @importFrom ncdfCF as_CF
#' @importFrom ncdfCF create_ncdf
#'
#' @return A `CFDataset` containing a CFVariable for each ETCCDI. Each variable is named by its ETCCDI acronym and has a `long_name` attribute describing the index.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
Metrics_ETCCDI <- function(projectionList, baseLineList, TResolution = "year", RRThreshold = 42, indices, fileName) {
    ## fileName handling
    if (!missing(fileName)) {
        fileName <- normalizePath(fileName, mustWork = FALSE)
    }

    ## File Check =========
    if (!missing(fileName)) {
        FCheck <- Helper_FileCheck(fileName = fileName, loadFun = NC_Read, load = TRUE, verbose = TRUE)
        if (!is.null(FCheck)) {
            return(FCheck)
        }
    }

    ## index selection handling
    supportedIndices <- c(
        "FD", "SU", "ID", "TR", "GSL", "TXx", "TNx", "TXn", "TNn", "TN10p",
        "TX10p", "TN90p", "TX90p", "WSDI", "CSDI", "DTR", "Rx1day", "Rx5day",
        "SDII", "R10mm", "R20mm", "Rnnmm", "CDD", "CWD", "R95pTOT", "R99pTOT", "RCPTOT"
    )
    if (missing(indices)) {
        selectedIndices <- supportedIndices
    } else {
        if (!is.character(indices)) {
            stop("'indices' must be a character vector of ETCCDI abbreviations.")
        }
        if (length(indices) == 0) {
            stop("'indices' cannot be an empty vector.")
        }
        unknownIndices <- setdiff(indices, supportedIndices)
        if (length(unknownIndices) > 0) {
            stop(
                paste0(
                    "Unsupported ETCCDI abbreviation(s) in 'indices': ",
                    paste(unknownIndices, collapse = ", "),
                    ". Supported abbreviations are: ",
                    paste(supportedIndices, collapse = ", "),
                    "."
                )
            )
        }
        selectedIndices <- unique(indices)
    }

    ## Setting up Progress Bar
    pb <- Helper_Progress(iterLength = length(selectedIndices), text = "ETCCDI Calculation")

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
    if (!missing(baseLineList)) {
        list2env(baseLineList, env = environment())
    }

    ## validate specification
    # skipping this for the time being
    # InCheck_ls <- list(
    #     Unit_Projection_TX = list(
    #         Input = unlist(TX$attributes$value[TX$attributes$name == "units"]),
    #         Allowed = c("K"),
    #         Operator = "in"
    #     ),
    #     Unit_Projection_TN = list(
    #         Input = unlist(TN$attributes$value[TN$attributes$name == "units"]),
    #         Allowed = c("K"),
    #         Operator = "in"
    #     ),
    #     Unit_Projection_RR = list(
    #         Input = unlist(RR$attributes$value[RR$attributes$name == "units"]),
    #         Allowed = c("mm"),
    #         Operator = "in"
    #     )
    # )
    # Helper_InputChecker(inputCheck = InCheck_ls)
    # should also check here for:
    #  1. temporal resolution being days
    #  2. time ranges being neat years
    #  3. time across all inputs in Rasters being the same
    #  4. Check that CRS is EPS:4326
    #  5. same as above for elements of baseLineList

    # stop("Add extra checks")
    # stop("do quantile calculations for percentile based ETCCDI")

    ## ETCCDI calculators
    indexCalculators <- list(
        FD = function() {
            Helper_Threshold(
                CFVariable = TN,
                operator = "<",
                threshold = 273.15,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        },
        SU = function() {
            Helper_Threshold(
                TX,
                operator = ">",
                threshold = 273.15 + 25,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        },
        ID = function() {
            Helper_Threshold(
                TX,
                operator = "<",
                threshold = 273.15,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        },
        TR = function() {
            Helper_Threshold(
                TN,
                operator = ">",
                threshold = 273.15 + 20,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        },
        GSL = function() {
            TM <- (TN + TX) / 2
            Helper_ETCCDIGSL(TM)
        },
        TXx = function() {
            TX$summarise("Tresholded", max_non_na, TResolution)
        },
        TNx = function() {
            TN$summarise("Tresholded", max_non_na, TResolution)
        },
        TXn = function() {
            TX$summarise("Tresholded", min_non_na, TResolution)
        },
        TNn = function() {
            TN$summarise("Tresholded", min_non_na, TResolution)
        },
        TN10p = function() {
            Helper_Threshold(
                TN,
                operator = "<",
                threshold = TN_Base[[names(TN_Base)[1]]],
                threshMode = "ETCCDIQuantiles",
                returnValues = FALSE,
                returnSummary = percentage_non_na,
                returnTResolution = TResolution
            )
        },
        TX10p = function() {
            Helper_Threshold(
                TX,
                operator = "<",
                threshold = TX_Base[[names(TX_Base)[1]]],
                threshMode = "ETCCDIQuantiles",
                returnValues = FALSE,
                returnSummary = percentage_non_na,
                returnTResolution = TResolution
            )
        },
        TN90p = function() {
            Helper_Threshold(
                TN,
                operator = ">",
                threshold = TN_Base[[names(TN_Base)[2]]],
                threshMode = "ETCCDIQuantiles",
                returnValues = FALSE,
                returnSummary = percentage_non_na,
                returnTResolution = TResolution
            )
        },
        TX90p = function() {
            Helper_Threshold(
                TX,
                operator = ">",
                threshold = TX_Base[[names(TX_Base)[2]]],
                threshMode = "ETCCDIQuantiles",
                returnValues = FALSE,
                returnSummary = percentage_non_na,
                returnTResolution = TResolution
            )
        },
        WSDI = function() {
            Helper_Threshold(
                TX,
                operator = ">",
                threshold = TX_Base[[names(TX_Base)[2]]],
                threshMode = "ETCCDIQuantiles",
                returnValues = FALSE,
                returnSummary = sum_run_of_ones,
                returnTResolution = TResolution
            )
        },
        CSDI = function() {
            Helper_Threshold(
                TN,
                operator = "<",
                threshold = TX_Base[[names(TX_Base)[1]]],
                threshMode = "ETCCDIQuantiles",
                returnValues = FALSE,
                returnSummary = sum_run_of_ones,
                returnTResolution = TResolution
            )
        },
        DTR = function() {
            dtr <- TX - TN
            dtr$summarise("Tresholded", mean_non_na, TResolution)
        },
        Rx1day = function() {
            RR$summarise("Tresholded", max_non_na, TResolution)
        },
        Rx5day = function() {
            RR$summarise("Tresholded", max_sum_over_5, TResolution)
        },
        SDII = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 1,
                returnValues = TRUE,
                returnSummary = mean_non_na,
                returnTResolution = TResolution
            )
        },
        R10mm = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 10,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        },
        R20mm = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 20,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        },
        Rnnmm = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = RRThreshold,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        },
        CDD = function() {
            Helper_Threshold(
                RR,
                operator = "<",
                threshold = 1,
                returnValues = FALSE,
                returnSummary = max_run_of_ones,
                returnTResolution = TResolution
            )
        },
        CWD = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 1,
                returnValues = FALSE,
                returnSummary = max_run_of_ones,
                returnTResolution = TResolution
            )
        },
        R95pTOT = function() {
            Helper_Threshold(
                RR,
                operator = ">",
                threshold = RR_Base[[names(RR_Base)[1]]],
                threshMode = "ETCCDIQuantiles",
                returnValues = TRUE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        },
        R99pTOT = function() {
            Helper_Threshold(
                RR,
                operator = ">",
                threshold = RR_Base[[names(RR_Base)[2]]],
                threshMode = "ETCCDIQuantiles",
                returnValues = TRUE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        },
        RCPTOT = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 1,
                returnValues = TRUE,
                returnSummary = sum_non_na,
                returnTResolution = TResolution
            )
        }
    )

    ## calculate only selected ETCCDI objects
    Return_ls <- list()
    for (i in seq_along(selectedIndices)) {
        idx <- selectedIndices[[i]]
        Return_ls[[idx]] <- indexCalculators[[idx]]()
        pb$tick(tokens = list(layer = i))
    }

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
        R10mm = "Days with Precip ≥ 10mm",
        R20mm = "Days with Precip ≥ 20mm",
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
    if (!missing(fileName)) {
        ds$save(fileName)
        ds <- NC_Read(fileName)
    }

    ## return dataset to user
    ds
}
