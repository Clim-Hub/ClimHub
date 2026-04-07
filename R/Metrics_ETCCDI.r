#' @title Calculate ETCCDI Indices from List Input
#'
#' @description This function calculates \href{https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/jgrd.50203}{ETCCDIs} from a named list of CFVariable objects. Currently, only some ETCCDI are supported.
#'
#' @details Input requirements are conditional on the selected ETCCDI indices in the `indices` argument (see table below). To reduce memory usage and speed up calculations, supply only the inputs required for the selected indices.
#'
#' | **ETCCDI** | **Name** | **Required Data** | **Description** | **Unit** | **Default `TResolution`** |
#' |---|---|---|---|---|---|
#' | FD | Number of frost days | `TN` | Annual count of days when TN (daily minimum temperature) < 0°C. | days | year |
#' | SU | Number of summer days | `TX` | Annual count of days when TX (daily maximum temperature) > 25°C. | days | year |
#' | ID | Number of icing days | `TX` | Annual count of days when TX (daily maximum temperature) < 0°C. | days | year |
#' | TR | Number of tropical nights | `TN` | Annual count of days when TN (daily minimum temperature) > 20°C. | days | year |
#' | GSL | Growing Season Length | `TX`, `TN` | Annual count of days between the first occurrence of at least 6 consecutive days with (TN+TX)/2 > 5°C and the first occurrence after 1st July (Northern Hemisphere) or 1st January (Southern Hemisphere) of at least 6 consecutive days with (TN+TX)/2 < 5°C | days | year |
#' | TXx | Max of Daily Max Temp | `TX` | Maximum daily maximum temperature in each month. | K | month |
#' | TNx | Max of Daily Min Temp | `TN` | Maximum daily minimum temperature in each month. | K | month |
#' | TXn | Min of Daily Max Temp | `TX` | Minimum daily maximum temperature in each month. | K | month |
#' | TNn | Min of Daily Min Temp | `TN` | Minimum daily minimum temperature in each month. | K | month |
#' | TN10p | Percent Days TN < 10th Percentile | `TN`, `TN_Base` | Percent of days, per year, where TN < 10th percentile of base period. | % | year |
#' | TX10p | Percent Days TX < 10th Percentile | `TX`, `TX_Base` | Percent of days, per year, where TX < 10th percentile of base period. | % | year |
#' | TN90p | Percent Days TN > 90th Percentile | `TN`, `TN_Base` | Percent of days, per year, where TN > 90th percentile of base period. | % | year |
#' | TX90p | Percent Days TX > 90th Percentile | `TX`, `TX_Base` | Percent of days, per year, where TX > 90th percentile of base period. | % | year |
#' | WSDI | Warm Spell Duration Index | `TX`, `TX_Base` | Annual count of days contained within runs of 6+ consecutive days when TX > 90th percentile of base period. | days | year |
#' | CSDI | Cold Spell Duration Index | `TN`, `TX_Base` | Annual count of days contained within runs of 6+ consecutive days when TN < 10th percentile of base period. | days | year |
#' | DTR | Daily Temperature Range | `TX`, `TN` | Monthly mean difference between daily max (TX) and min (TN) temperatures. | K | month |
#' | Rx1day | Max 1-day Precipitation | `RR` | Maximum precipitation in a single day each month. | mm | month |
#' | Rx5day | Max 5-day Precipitation | `RR` | Maximum precipitation over any 5 consecutive days in each month. | mm | month |
#' | SDII | Simple Precipitation Intensity Index | `RR` | Mean precipitation amount on wet days (RR ≥ 1mm). | mm/day | year |
#' | R10mm | Days with Precip ≥ 10mm | `RR` | Annual count of days with precipitation ≥ 10mm. | days | year |
#' | R20mm | Days with Precip ≥ 20mm | `RR` | Annual count of days with precipitation ≥ 20mm. | days | year |
#' | Rnnmm | Days with Precip ≥ user-defined threshold | `RR` | Annual count of days with precipitation ≥ nnmm. | days | year |
#' | CDD | Consecutive Dry Days | `RR` | Maximum number of consecutive days with RR < 1mm. | days | year |
#' | CWD | Consecutive Wet Days | `RR` | Maximum number of consecutive days with RR ≥ 1mm. | days | year |
#' | R95pTOT | Total Precipitation from RR > 95th Percentile | `RR`, `RR_Base` | Total precipitation from wet days (RR > 95th percentile of base period). | mm | year |
#' | R99pTOT | Total Precipitation from RR > 99th Percentile | `RR`, `RR_Base` | Total precipitation from wet days (RR > 99th percentile of base period). | mm | year |
#' | RCPTOT | Total Precipitation on Wet Days | `RR` | Sum of precipitation on wet days (RR ≥ 1mm) over a year. | mm | year |
#'
#' @param projectionList List. List of `CFVariable` objects required by the selected ETCCDI indices via the `indices` argument. Include only named elements that are needed for `indices`: "TX" (daily maximum air temperature in Kelvin), "TN" (daily minimum air temperature in Kelvin), and/or "RR" (daily total precipitation in mm). See details for required data input per index.
#' @param baseLineList Optional, list. List of `CFDataset` objects containing baseline quantiles required only for quantile-based ETCCDI indices. Include only named elements needed for `indices`: "TX_Base", "TN_Base", and/or "RR_Base" (note that these must be in Kelvin, Kelvin, and mm, respectively). If no quantile-based ETCCDI is selected, this argument can be omitted. See details for required quantile baselines per index.
#' @param indices Optional, character. Character vector of ETCCDI abbreviations to calculate (see first column of the details table). If missing, all supported indices in this function are calculated. See details for supported indices.
#' @param TResolution Optional, character. Temporal resolution for ETCCDI calculation. Supports "year", "month" and "season". If omitted, each selected index is calculated using its default temporal resolution from the details table. If provided, the same resolution is used for all selected indices and output indices whose default resolution differs are prefixed with "ALT_".
#' @param RRThreshold Numeric. Custom threshold for daily precipiation in mm for calculation of Rnnmm. Defaults to 42.
#' @param fileName Character, optional. Character. A file name for the produced file, including path and ".nc" file ending. If no value is supplied, the dataset is not written to disk but returned as a `CFDataset` object in memory. If a file name is supplied and a file with that name already exists, the function will attempt to load and return that file instead of recalculating the indices.
#'
#' @importFrom ncdfCF as_CF
#' @importFrom ncdfCF create_ncdf
#'
#' @return A `CFDataset`. If all calculated ETCCDI indices have the same temporal resolution (e.g., when `TResolution` is specified or a subset of `indices` is selected that share a temporal resolution default), the returned `CFDataset` contains all variables in the root group. If indices span multiple temporal resolutions (e.g., when `TResolution` is omitted and some selected `indices` default to "year" while others default to "month"), the returned `CFDataset` contains one subgroup per temporal resolution, each containing only variables of that temporal resolution.
#'
#' Each variable is named by its ETCCDI acronym and has a `long_name` attribute describing the index. Acronyms of indices calculated with a non-default temporal resolution are prefixed with "ALT_" to indicate that these are alternative calculations.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' ## Directory for Data and Output -----------
#' Dir.Data <- file.path(getwd(), "ExampleData")
#' dir.create(Dir.Data, showWarnings = FALSE)
#'
#' ## Downloading and preparing data -----------
#' ### Variables for which we need data
#' vars <- c("minimum_air_temperature", "maximum_air_temperature", "precipitation_flux")
#' names(vars) <- c("tasmin", "tasmax", "pr")
#'
#' ### Data (Down-)Loading and Preparation
#' Data_ls <- lapply(seq_along(vars), FUN = function(i) {
#'     print(as.character(vars[i]))
#'     ## these need to be made into quantiles to define base-period for ETCCDI calculation
#'     KiN_Base <- Access_KlimaiNorge2100(
#'         variable = as.character(vars[i]),
#'         dateStart = "1971-01-01", # note that the standard base period starts 1961, but KiN does start in 1971 so we use that as the start date
#'         dateStop = "2000-12-31",
#'         extent = c(9, 11, 59, 61),
#'         model = "noresm-r1i1p1f1-hclim",
#'         method = "eqm",
#'         scenario = "ssp370",
#'         fileName = file.path(Dir.Data, paste0("KiN_", names(vars)[i], ".nc"))
#'     )
#'
#'     ## these are the files for calculation of ETCCDI metrics in future
#'     KiN_2090 <- Access_KlimaiNorge2100(
#'         variable = as.character(vars[i]),
#'         dateStart = "2090-01-01",
#'         dateStop = "2099-12-31",
#'         extent = c(9, 11, 59, 61),
#'         model = "noresm-r1i1p1f1-hclim",
#'         method = "eqm",
#'         scenario = "ssp370",
#'         fileName = file.path(Dir.Data, paste0("KiN_", names(vars)[i], "_2090.nc"))
#'     )
#'
#'     ## these are the files for baseline quantiles
#'     QuantF <- file.path(Dir.Data, paste0("KiN_", names(vars)[i], "_BaseLineQuantiles.nc"))
#'     if (!file.exists(QuantF)) {
#'         if (names(vars)[i] == "pr") {
#'             probs_vec <- c(0.95, 0.99)
#'             input <- KiN_Base[[names(vars)[i]]] * 86400 # to get from mm/day to kg m-2 s-1
#'             input$set_attribute("units", "NC_CHAR", "mm")
#'         } else {
#'             probs_vec <- c(0.1, 0.9)
#'             input <- KiN_Base[[names(vars)[i]]]
#'         }
#'         Quant <- Metrics_BootstrapQuantiles(
#'             CFVariable = input,
#'             probs = probs_vec,
#'             bootstrapWindow = 5
#'         )
#'         Quant$save(QuantF)
#'     } else {
#'         Quant <- NC_Read(QuantF)
#'     }
#'
#'     return(list(projection = KiN_2090, baseLine = Quant))
#' })
#' names(Data_ls) <- names(vars)
#'
#' ### Data Object listing
#' TX <- Data_ls[["tasmax"]][["projection"]][["tasmax"]]
#' TN <- Data_ls[["tasmin"]][["projection"]][["tasmin"]]
#' RR <- Data_ls[["pr"]][["projection"]][["pr"]] * 86400 # to get from mm/day to kg m-2 s-1
#' RR$set_attribute("units", "NC_CHAR", "mm")
#'
#' TX_Base <- Data_ls[["tasmax"]][["baseLine"]]
#' TN_Base <- Data_ls[["tasmin"]][["baseLine"]]
#' RR_Base <- Data_ls[["pr"]][["baseLine"]]
#'
#' rm(Data_ls) # remove list to free up memory
#'
#' ## ETCCDI calculation -----------
#' ### All ETCCDI
#' Metrics_ETCCDI(
#'     projectionList = list(TX = TX, TN = TN, RR = RR),
#'     baseLineList = list(TX_Base = TX_Base, TN_Base = TN_Base, RR_Base = RR_Base),
#'     fileName = file.path(Dir.Data, "ETCCDI_Metrics.nc")
#' )
#'
#' ### Select ETCCDI
#' Metrics_ETCCDI(
#'     projectionList = list(TX = TX, TN = TN, RR = RR),
#'     baseLineList = list(TX_Base = TX_Base, TN_Base = TN_Base, RR_Base = RR_Base),
#'     indices = c("FD", "TXx", "R95pTOT")
#' )
#'
#' ### Select ETCCDI with custom temporal resolutions
#' Metrics_ETCCDI(
#'     projectionList = list(RR = RR),
#'     # baseLineList = list(RR_Base = RR_Base), # we do not need to supply the baseline quantiles if we do not select any indices that depend on them
#'     indices = c("Rnnmm"),
#'     RRThreshold = 21, # this is the threshold for the Rnnmm metric, in mm
#'     TResolution = "season"
#' )
#'
#' Metrics_ETCCDI(
#'     projectionList = list(TX = TX, TN = TN, RR = RR),
#'     baseLineList = list(TX_Base = TX_Base, TN_Base = TN_Base, RR_Base = RR_Base),
#'     indices = c("FD", "SU", "ID", "TR"),
#'     TResolution = "month" # note that this is not the default for these indices, so the returned object will bear the prefix "ALT_" for each index to indicate that this is an alternative calculation with a different temporal resolution
#' )
#' }
#' @export
Metrics_ETCCDI <- function(projectionList, baseLineList, indices, TResolution, RRThreshold = 42, fileName) {
    ## fileName handling
    if (!missing(fileName)) {
        fileName <- normalizePath(fileName, mustWork = FALSE)
    }

    ## index selection handling (moved before FCheck)
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

    ## ETCCDI default temporal resolution handling (moved before FCheck)
    defaultTResolution <- c(
        FD = "year",
        SU = "year",
        ID = "year",
        TR = "year",
        GSL = "year",
        TXx = "month",
        TNx = "month",
        TXn = "month",
        TNn = "month",
        TN10p = "year",
        TX10p = "year",
        TN90p = "year",
        TX90p = "year",
        WSDI = "year",
        CSDI = "year",
        DTR = "month",
        Rx1day = "month",
        Rx5day = "month",
        SDII = "year",
        R10mm = "year",
        R20mm = "year",
        Rnnmm = "year",
        CDD = "year",
        CWD = "year",
        R95pTOT = "year",
        R99pTOT = "year",
        RCPTOT = "year"
    )
    userProvidedTResolution <- !missing(TResolution)
    if (userProvidedTResolution) {
        if (!is.character(TResolution) || length(TResolution) != 1) {
            stop("'TResolution' must be a single character value: 'year', 'month', or 'season'.")
        }
        providedTResolution <- tolower(TResolution)
        if (is.na(providedTResolution) || !providedTResolution %in% c("year", "month", "season")) {
            stop("'TResolution' must be one of 'year', 'month', or 'season'.")
        }
    } else {
        providedTResolution <- NULL
    }
    get_index_t_resolution <- function(indexName) {
        if (userProvidedTResolution) {
            providedTResolution
        } else {
            defaultTResolution[[indexName]]
        }
    }

    ## Create tResolution_ls and identify unique resolutions (for FCheck)
    tResolution_ls <- list()
    for (idx in selectedIndices) {
        tResolution_ls[[idx]] <- get_index_t_resolution(idx)
    }
    unique_resolutions <- unique(unlist(tResolution_ls))

    ## File Check with awareness of temporal resolution structure =========
    if (!missing(fileName)) {
        FCheck <- Helper_FileCheck(fileName = fileName, loadFun = NC_Read, load = TRUE, verbose = TRUE)
        if (!is.null(FCheck)) {
            return(FCheck)
        }
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
                returnTResolution = get_index_t_resolution("FD")
            )
        },
        SU = function() {
            Helper_Threshold(
                TX,
                operator = ">",
                threshold = 273.15 + 25,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = get_index_t_resolution("SU")
            )
        },
        ID = function() {
            Helper_Threshold(
                TX,
                operator = "<",
                threshold = 273.15,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = get_index_t_resolution("ID")
            )
        },
        TR = function() {
            Helper_Threshold(
                TN,
                operator = ">",
                threshold = 273.15 + 20,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = get_index_t_resolution("TR")
            )
        },
        GSL = function() {
            TM <- (TN + TX) / 2
            Helper_ETCCDIGSL(TM)
        },
        TXx = function() {
            TX$summarise("Tresholded", max_non_na, get_index_t_resolution("TXx"))
        },
        TNx = function() {
            TN$summarise("Tresholded", max_non_na, get_index_t_resolution("TNx"))
        },
        TXn = function() {
            TX$summarise("Tresholded", min_non_na, get_index_t_resolution("TXn"))
        },
        TNn = function() {
            TN$summarise("Tresholded", min_non_na, get_index_t_resolution("TNn"))
        },
        TN10p = function() {
            Helper_Threshold(
                TN,
                operator = "<",
                threshold = TN_Base[[names(TN_Base)[1]]],
                threshMode = "ETCCDIQuantiles",
                returnValues = FALSE,
                returnSummary = percentage_non_na,
                returnTResolution = get_index_t_resolution("TN10p")
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
                returnTResolution = get_index_t_resolution("TX10p")
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
                returnTResolution = get_index_t_resolution("TN90p")
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
                returnTResolution = get_index_t_resolution("TX90p")
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
                returnTResolution = get_index_t_resolution("WSDI")
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
                returnTResolution = get_index_t_resolution("CSDI")
            )
        },
        DTR = function() {
            dtr <- TX - TN
            dtr$summarise("Tresholded", mean_non_na, get_index_t_resolution("DTR"))
        },
        Rx1day = function() {
            RR$summarise("Tresholded", max_non_na, get_index_t_resolution("Rx1day"))
        },
        Rx5day = function() {
            RR$summarise("Tresholded", max_sum_over_5, get_index_t_resolution("Rx5day"))
        },
        SDII = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 1,
                returnValues = TRUE,
                returnSummary = mean_non_na,
                returnTResolution = get_index_t_resolution("SDII")
            )
        },
        R10mm = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 10,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = get_index_t_resolution("R10mm")
            )
        },
        R20mm = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 20,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = get_index_t_resolution("R20mm")
            )
        },
        Rnnmm = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = RRThreshold,
                returnValues = FALSE,
                returnSummary = sum_non_na,
                returnTResolution = get_index_t_resolution("Rnnmm")
            )
        },
        CDD = function() { #  !! ISSUES HERE, TALK TO NINA
            Helper_Threshold(
                RR,
                operator = "<",
                threshold = 1,
                returnValues = FALSE,
                returnSummary = max_run_of_ones,
                returnTResolution = get_index_t_resolution("CDD")
            )
        },
        CWD = function() { #  !! ISSUES HERE, TALK TO NINA
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 1,
                returnValues = FALSE,
                returnSummary = max_run_of_ones,
                returnTResolution = get_index_t_resolution("CWD")
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
                returnTResolution = get_index_t_resolution("R95pTOT")
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
                returnTResolution = get_index_t_resolution("R99pTOT")
            )
        },
        RCPTOT = function() {
            Helper_Threshold(
                RR,
                operator = ">=",
                threshold = 1,
                returnValues = TRUE,
                returnSummary = sum_non_na,
                returnTResolution = get_index_t_resolution("RCPTOT")
            )
        }
    )

    ## calculate only selected ETCCDI objects (tResolution_ls already created before FCheck)
    Return_ls <- list()
    for (i in seq_along(selectedIndices)) {
        idx <- selectedIndices[[i]]
        Return_ls[[idx]] <- indexCalculators[[idx]]()
        pb$tick(tokens = list(layer = i))
    }

    ## Combine into CFDataset(s) with appropriate variable names + long_name attributes
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

    # Determine what to return and save (unique_resolutions already created before FCheck)
    if (length(unique_resolutions) == 1) {
        # Single temporal resolution - base dataset is sufficient
        return_ds <- ncdfCF::create_ncdf()
        for (nm in names(Return_ls)) {
            cfvar <- Return_ls[[nm]]
            raw <- cfvar$raw()
            outputName <- nm
            ## adding variable with "ALT_" prefix if user provided a temporal resolution that differs from the default for this index
            if (userProvidedTResolution && defaultTResolution[[nm]] != providedTResolution) {
                outputName <- paste0("ALT_", nm)
            }
            new_var <- ncdfCF::as_CF(outputName, raw)
            if (nm %in% names(long_names)) {
                new_var$set_attribute("long_name", "NC_CHAR", long_names[[nm]])
            }
            return_ds$add_variable(new_var)
        }
    } else {
        # Multiple temporal resolutions - create one dataset with one subgroup per resolution
        return_ds <- ncdfCF::create_ncdf()
        # List of shared objects that go into the root group (.. = parent group of the sub-group, i.e. the root group)
        latlonnames <- c(
            if (exists("TX")) names(TX$axes) else character(0),
            if (exists("TN")) names(TN$axes) else character(0),
            if (exists("RR")) names(RR$axes) else character(0)
        )
        latlonnames <- unique(latlonnames)
        latlonnames <- latlonnames[!(latlonnames %in% "time")]
        latlon <- as.list(rep("..", length(latlonnames) * 2 + 1))
        names(latlon) <- c(latlonnames, paste0(latlonnames, "_bnds"), "height")

        for (tres in unique_resolutions) {
            subgroup_name <- as.character(tres)
            resolution_group <- return_ds$root$create_subgroup(subgroup_name)
            var_names_for_this_res <- names(tResolution_ls)[unlist(tResolution_ls) == tres]

            for (nm in var_names_for_this_res) {
                cfvar <- Return_ls[[nm]]
                raw <- cfvar$raw()
                outputName <- nm
                if (userProvidedTResolution && defaultTResolution[[nm]] != providedTResolution) {
                    outputName <- paste0("ALT_", nm)
                }
                new_var <- ncdfCF::as_CF(outputName, raw)
                if (nm %in% names(long_names)) {
                    new_var$set_attribute("long_name", "NC_CHAR", long_names[[nm]])
                }
                resolution_group$add_variable(new_var, locations = latlon)
            }
        }
    }

    ## saving if fileName provided, otherwise just return dataset object
    if (!missing(fileName)) {
        return_ds$save(fileName)
        return_ds <- NC_Read(fileName)
    }

    ## return dataset(s) to user
    return_ds
}
