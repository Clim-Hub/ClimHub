#' @title Access CERRA Reanalysis Data for Europe
#'
#' @description Downloads and processes CERRA sub-daily regional reanalysis data for Europe on single levels from 1984 to present via the \href{https://cds.climate.copernicus.eu/#!/home}{Climate Data Store (CDS)} hosted by the \href{https://cds.climate.copernicus.eu/about-c3s}{Copernicus Climate Change Service (C3S)}. By default, this function breaks down download calls into intervals to avoid failing queries, downloads data from \href{https://www.ecmwf.int/}{ECMWF} servers according to user-specification, and fuses the downloaded files together. The actual download time depends on ECMWF download queues. Users need an \href{https://cds.climate.copernicus.eu/api-how-to}{API key} and must accept terms and conditions for the specific queried dataset(s).
#'
#' @param variable Character. An overview of CERRA Reanalysis Data on Single Levels variables can be obtained with `Discovery_Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param levelType Character. One of "Surface or atmosphere" or "Soil". Setting aligns with choice of variable. Options for `levelType` per variable can be found in the output returned by `Discovery_Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param soilLayer Optional, Character. One of either "" (empty string) or "Top layer". Setting aligns with choice of variable. Options for `soilLayer` per variable can be found in the output returned by `Discovery_Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param dataType Character. One of either "Ensemble members" or "Reanalysis". Setting aligns with choice of variable. Options for `dataType` per variable can be found in the output returned by `Discovery_Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param productType Character. One of either "Analysis" or "Forecast". Setting aligns with choice of variable. Options for `productType` per variable can be found in the output returned by `Discovery_Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param dateStart Character. "YYYY-MM-DD HH" date at which to start time series of downloaded data. Data is available daily at hours 00, 03, 06, 09, 12, 15, 18, and 21 (in UTC time zone).
#' @param dateStop Character. "YYYY-MM-DD HH" date at which to stop time series of downloaded data. Data is available daily at hours 00, 03, 06, 09, 12, 15, 18, and 21 (in UTC time zone).
#' @param timeZone Optional, Character. Time zone in which to represent and evaluate time dimension of data. See the output of OlsonNames() for a full overview of supported specifications. Default is UTC.
#' @param leadTimeHour Optional, Integer. Lead time of reanalysis. CERRA Reanalysis Data on Single Levels leadtimes can be obtained with `Discovery_QuickFacts("CDS_reanalysis-cerra-single-levels")$leadtime`. This argument only takes effect when the `productType` argument is "Forecast".
#' @param fileName Character. A file name for the produced file, including path.
#' @param compression Optional, Integer. Compression level between 1 to 9 applied to final .nc file. Same as compression argument in terra::writeCDF(). Defaults to NA.
#' @param apiKey Character. ECMWF cds API key.
#' @param apiUser Character. ECMWF cds user number.
#' @param tChunkSize Optional, Numeric. Number of layers to bundle in each individual download. Default is 6000 to adhere to most restrictive CDS limits: https://cds.climate.copernicus.eu/live/limits.
#' @param removeTemporary Optional, Logical. Whether to delete temporary files after completion. Defaults to TRUE.
#' @param writeFile Optional, Logical. Whether to write final SpatRaster to disk as an .nc or to return information from memory. Setting to FALSE will prohibit removal of temporary files. Defaults to TRUE.
#' @param closeConnections Optional, Logical. Whether to close all connections at the end of function execution. When executing this function often after another, this can be useful to avoid errors. Defaults to TRUE.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom terra rast
#' @importFrom terra time
#' @importFrom terra nlyr
#' @importFrom terra metags
#'
#' @return A SpatRaster object containing the downloaded data, and a file in the specified directory. The SpatRaster contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#'  - *Citation* - A string which to use for in-line citation of the data product obtained.
#'  - *Call_* - A set of strings matching arguments supplied to the download function call.
#'
#' \strong{ATTENTION:} If data is loaded again from disk at a later point with a different function, take note that the time zone will have to be set anew and existing time parameters in the .nc contents will need to be converted to the desired time zone. Likewise, citation and download-call metadata will not be loaded properly from a .nc when loading data through a different function. `ClimHub` download funtions handle these .nc specific issues when loading .nc files created previously with `ClimHub` download funtions from disk automatically.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' CERRA_analysis_rast <- Access_reanalysis_cerra_single_levels(
#'     # CDS API call specification
#'     variable = "2m temperature",
#'     levelType = "Surface or atmosphere",
#'     soilLayer = "",
#'     dataType = "Reanalysis",
#'     productType = "Analysis",
#'     leadTimeHour = "",
#'     # time-window, default set to range of dataset-type
#'     dateStart = "1993-01-01 01:00",
#'     dateStop = "1995-12-31 22:00",
#'     timeZone = "CET",
#'     # file naming and output
#'     fileName = file.path(getwd(), "CERRA_Analysis.nc"),
#'     compression = NA,
#'     writeFile = FALSE,
#'     removeTemporary = FALSE,
#'     # API credentials
#'     apiUser = apiUser,
#'     apiKey = apiKey,
#'     # Quality of life and efficiency settings
#'     tChunkSize = 6000,
#'     closeConnections = TRUE
#' )
#'
#' CERRA_ensembles_rast <- Access_reanalysis_cerra_single_levels(
#'     # CDS API call specification
#'     variable = "2m relative humidity",
#'     levelType = "Surface or atmosphere",
#'     soilLayer = "",
#'     dataType = "Ensemble members",
#'     productType = "Analysis",
#'     leadTimeHour = "",
#'     # time-window, default set to range of dataset-type
#'     dateStart = "1993-01-01 01:00",
#'     dateStop = "1993-01-31 22:00",
#'     timeZone = "CET",
#'     # file naming and output
#'     fileName = "CERRA_Ensembles.nc",
#'     compression = NA,
#'     writeFile = TRUE,
#'     removeTemporary = TRUE,
#'     # API credentials
#'     apiUser = apiUser,
#'     apiKey = apiKey,
#'     # Quality of life and efficiency settings
#'     tChunkSize = 6000,
#'     closeConnections = TRUE
#' )
#' }
#' @export
Access_reanalysis_cerra_single_levels <- function(
    variable, levelType, soilLayer, dataType, productType, leadTimeHour,
    dateStart, dateStop, timeZone = "UTC",
    fileName, compression = NA, writeFile = TRUE, removeTemporary = TRUE,
    apiUser, apiKey,
    tChunkSize = 6000, closeConnections = TRUE) {
    ## Input Checks =================================
    ### Catching Most Frequent Issues ---------
    if (closeConnections) {
        on.exit(closeAllConnections())
    }
    #--- checking if API User and Key have been supplied
    if (exists("apiUser") + exists("apiKey") != 2) {
        stop("Please provide a value for the apiUser and apiKey arguments.")
    }
    #--- making apiUser into a character string
    apiUser <- as.character(apiUser)
    #--- Set leadTimeHour to "" when we aren't querying a forecast
    if (productType != "Forecast") {
        leadTimeHour <- ""
    }

    ### Specification Validation ---------
    message("###### Checking Request Validity")
    ### fileName
    if (missing(fileName)) {
        stop("Please specify a filename.")
    }
    fileName <- normalizePath(fileName, mustWork = FALSE)
    #--- Time Reformatting
    Dates_df <- Helper_MakeUTC(dates = c(
        as.POSIXct(dateStart, tz = timeZone),
        as.POSIXct(dateStop, tz = timeZone)
    ))
    #--- actual checks
    QuickFacts_ls <- Discovery_QuickFacts("CDS_reanalysis-cerra-single-levels")
    Variables_df <- Discovery_Variables("CDS_reanalysis-cerra-single-levels")
    InCheck_ls <- list(
        Variable = list(
            Input = variable,
            Allowed = Variables_df$name,
            Operator = "in"
        ),
        Time = list(
            Input = Dates_df$UTC,
            Allowed = c(
                QuickFacts_ls$time$extent[1],
                gsub(" UTC", "", Helper_MakeUTC(dates = as.POSIXct(paste0(format(Sys.time(), "%Y-%m-%d %H"), ":00")))$UTC) # assuming current day and hour as possible end since dataset is released ongoingly
            ),
            Operator = "exceeds"
        ),
        TimeZone = list(
            Input = timeZone,
            Allowed = OlsonNames(),
            Operator = "in"
        ),
        LevelType = list(
            Input = levelType,
            Allowed = unlist(Variables_df[Variables_df$name == variable, ]$"level type"),
            Operator = "in"
        ),
        SoilLayer = list(
            Input = soilLayer,
            Allowed = unlist(Variables_df[Variables_df$name == variable, ]$"soil layer"),
            Operator = "in"
        ),
        DataType = list(
            Input = dataType,
            Allowed = unlist(Variables_df[Variables_df$name == variable, ]$"data type"),
            Operator = "in"
        ),
        ProductType = list(
            Input = productType,
            Allowed = unlist(Variables_df[Variables_df$name == variable, ]$"product type"),
            Operator = "in"
        ),
        LeadTimeHour = list(
            Input = leadTimeHour,
            Allowed = ifelse(productType == "Forecast", QuickFacts_ls$leadtime, ""),
            Operator = "in"
        ),
        LeadTimeHour = list(
            Input = length(leadTimeHour),
            Allowed = 1,
            Operator = "in"
        )
    )
    Helper_InputChecker(inputCheck = InCheck_ls)

    #--- Metadata
    Citation <- paste0(QuickFacts_ls$name, " (DOI:", QuickFacts_ls$doi, ") data provided by the ECMWF CDS obtained on ", Sys.Date())
    names(Citation) <- "Citation"
    callargs <- mget(names(formals()), sys.frame(sys.nframe()))
    callargs[sapply(callargs, is.null)] <- "NULL"
    callargs[sapply(callargs, class) == "name"] <- ""
    names(callargs) <- paste("Call", names(callargs), sep = "_")
    Meta_vec <- c(Citation, unlist(callargs))

    #--- time assignment, we do this in input timezone format to assign accurate times to final file layers
    if (dataType != "Ensemble members") {
        TRes <- paste(QuickFacts_ls$time$resolution, QuickFacts_ls$time$unit)
    } else {
        TRes <- paste(QuickFacts_ls$time$resolution * 2, QuickFacts_ls$time$unit)
    }
    TimeAssing <- seq(
        from = Dates_df$IN[1],
        to = Dates_df$IN[2],
        by = TRes
    )
    if (dataType == "Ensemble members") {
        TimeAssing <- rep(TimeAssing, each = 10)
    }

    ## File Check =========
    FCheck <- Helper_FileCheck(fileName = fileName, loadFun = NC_Read, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
        terra::time(FCheck) <- TimeAssing
        return(FCheck)
    }

    ## The CDS Request =================================
    # message("###### CDS Request & Data Download")

    ### Building ---------
    message("Building CDS Request(s)")

    #--- Name resolving
    QueryVariable <- gsub(" ", "_", tolower(variable))

    #--- Create timewindows of donwloads
    Allowed_seq <- seq( # sequence that the dataset might support ("might" as end-date cannot be checked)
        from = as.POSIXct(QuickFacts_ls$time$extent[1], tz = "UTC"),
        to = Helper_MakeUTC(dates = as.POSIXct(paste0(format(Sys.time(), "%Y-%m-%d %H"), ":00")))$UTC,
        by = paste(QuickFacts_ls$time$resolution, QuickFacts_ls$time$unit)
    )
    warning(paste("Cannot validate user-specified dateStop argument as", QuickFacts_ls$name, "is released continuously. You may want to consult the download tab at", QuickFacts_ls$url, "to ensure that the data you query is available."))

    RequestWindows <- Helper_CDS_MakeRequestWindows(
        allowedSeq = Allowed_seq,
        queriedSeq = Helper_MakeUTC(dates = TimeAssing)$UTC,
        tChunkSize = tChunkSize
    )

    #--- Building request list
    QueryParams_ls <- list(
        "dataset_short_name" = "reanalysis-cerra-single-levels",
        "dataset" = "reanalysis-cerra-single-levels",
        "product_type" = gsub(" ", "_", tolower(productType)),
        "data_type" = gsub(" ", "_", tolower(dataType)),
        "level_type" = gsub(" ", "_", tolower(levelType)),
        "soil_layer" = gsub(" ", "_", tolower(soilLayer)),
        "variable" = QueryVariable,
        "time" = paste0(unique(format(Helper_MakeUTC(dates = TimeAssing)$UTC, "%H")), ":00"),
        "leadtime_hour" = leadTimeHour,
        "data_format" = "grib"
    )
    CDSRequests_ls <- Helper_CDS_MakeRequests(queryTimeWindows = RequestWindows, queryParams = QueryParams_ls, dir = dirname(fileName))

    ### Executing ---------
    message("Executing CDS Request(s)")

    #--- Register credentials
    Helper_CDS_RegisterAPICredentials(apiUser = apiUser, apiKey = apiKey)

    #--- Looping over requests
    FilesToLoad <- Helper_CDS_ExecuteRequests(cdsRequests = CDSRequests_ls, apiUser = apiUser, apiKey = apiKey)

    ## The Data =================================
    # message("###### The Data")

    ### Loading ---------
    message("###### Loading Downloaded Data from Disk")
    CDS_rast <- Helper_LoadFiles(fileName = FilesToLoad)
    if (nlyr(CDS_rast) != length(TimeAssing)) {
        warning("The downloaded data does not contain as many layers as ClimHub expected. You may want to retry this function execution after deleting the downloaded files or file an issue on the ClimHub GitHub here: https://github.com/ErikKusch/ClimHub/issues. To investigate the issue directly on your end, the downloaded data has been returned to your working environment, but the download function was terminated prematurely.")
        return(CDS_rast)
    }
    terra::time(CDS_rast) <- TimeAssing

    ### Time-Window Extraction ---------
    message("###### Extracting Requested Time-Period")
    TimeLyr <- which(
        time(CDS_rast) >= dateStart &
            time(CDS_rast) <= dateStop
    )
    CDS_rast <- CDS_rast[[TimeLyr]]

    ### Checking for correct layer number ---------

    ## Exports =================================
    message("###### Data Export & Return")

    ### Assign additional information
    terra::metags(CDS_rast) <- Meta_vec

    ### write file
    if (writeFile) {
        NC_Write(
            spatRaster = CDS_rast, fileName = fileName,
            varName = variable,
            longName = gsub(pattern = " ", replacement = "_", tolower(variable)),
            unit = Variables_df[Variables_df$name == variable, ]$unit,
            meta = Meta_vec, compression = compression
        )
        CDS_rast <- NC_Read(fileName = fileName)
    }

    ### unlink temporary files
    if (removeTemporary & writeFile) {
        unlink(FilesToLoad)
    }

    ### return object
    return(CDS_rast)
}