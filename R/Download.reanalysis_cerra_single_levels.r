#' Downloading CERRA sub-daily regional reanalysis data for Europe on single levels from 1984 to present Data from ECMWF Climate Data Store
#'
#' This function is used to obtain data from the \href{https://cds.climate.copernicus.eu/datasets/reanalysis-cerra-single-levels?tab=overview}{CERRA sub-daily regional reanalysis data for Europe on single levels from 1984 to present} via the \href{https://cds.climate.copernicus.eu/#!/home}{Climate Data Store (CDS)} hosted by the \href{https://cds.climate.copernicus.eu/about-c3s}{Copernicus Climate Change Service (C3S)}. By default, this function breaks down download calls into intervals so as to avoid submitting queries which fail, downloads queried data from \href{https://www.ecmwf.int/}{ECMWF} servers according to user-specification, and fuses the downloaded files together according to user-demands. The actual time to download is dependent on ECMWF download queues. Users need an \href{https://cds.climate.copernicus.eu/api-how-to}{API key} for download staging and accept terms and conditions for the specific queried dataset(s).
#'
#' @param Variable Character. An overview of CERRA Reanalysis Data on Single Levels variables can be obtained with `Meta.Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param LevelType Character. One of "Surface or atmosphere" or "Soil". Setting aligns with choice of variable. Options for `LevelType` per variable can be found in the output returned by `Meta.Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param SoilLayer Character. One of either "" (empty string) or "Top layer". Setting aligns with choice of variable. Options for `SoilLayer` per variable can be found in the output returned by `Meta.Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param DataType Character. One of either "Ensemble members" or "Reanalysis". Setting aligns with choice of variable. Options for `DataType` per variable can be found in the output returned by `Meta.Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param ProductType Character. One of either "Analysis" or "Forecast". Setting aligns with choice of variable. Options for `ProductType` per variable can be found in the output returned by `Meta.Variables("CDS_reanalysis-cerra-single-levels")`.
#' @param DateStart Character. "YYYY-MM-DD HH" date at which to start time series of downloaded data. Data is available daily at hours 00, 03, 06, 09, 12, 15, 18, and 21 (in UTC time zone).
#' @param DateStop Character. "YYYY-MM-DD HH" date at which to stop time series of downloaded data. Data is available daily at hours 00, 03, 06, 09, 12, 15, 18, and 21 (in UTC time zone).
#' @param TimeZone Character. Time zone in which to represent and evaluate time dimension of data. See the output of OlsonNames() for a full overview of supported specifications. Default is UTC.
#' @param LeadtimeHour Integer. Lead time of reanalysis. CERRA Reanalysis Data on Single Levels leadtimes can be obtained with `Meta.QuickFacts("CDS_reanalysis-cerra-single-levels")$leadtime`. This argument only takes effect when the `ProductType` argument is "Forecast".
#' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
#' @param FileName Character. A file name for the produced file.
#' @param Compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
#' @param API_Key Character; ECMWF cds API key.
#' @param API_User Character; ECMWF cds user number.
#' @param TChunkSize Numeric. Number of layers to bundle in each individual download. Default is 6000 to adhere to most restrictive CDS limits: https://cds.climate.copernicus.eu/live/limits.
#' @param RemoveTemporary Logical. Whether to delete temporary files after completion.
#' @param WriteFile Logical. Whether to write final SpatRaster to disk as an .nc or to return information from memory. Note that setting WriteFile = FALSE will prohibit removal of
#' @param closeConnections Logical. Whether to close all connections at the end of function execution. When executing this function often after another, this can be very useful to avoid errors.
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
#' @examples
#' \dontrun{
#' CERRA_analysis_rast <- Download.reanalysis_cerra_single_levels(
#'     # CDS API call specification
#'     Variable = "2m temperature",
#'     LevelType = "Surface or atmosphere",
#'     SoilLayer = "",
#'     DataType = "Reanalysis",
#'     ProductType = "Analysis",
#'     LeadTimeHour = "",
#'     # time-window, default set to range of dataset-type
#'     DateStart = "1993-01-01 01:00",
#'     DateStop = "1995-12-31 22:00",
#'     TimeZone = "CET",
#'     # file naming and output
#'     Dir = getwd(),
#'     FileName = "CERRA_Analsysis.nc",
#'     Compression = NA,
#'     WriteFile = FALSE,
#'     RemoveTemporary = FALSE,
#'     # API credentials
#'     API_User = API_User,
#'     API_Key = API_Key,
#'     # Quality of life and efficiency settings
#'     TChunkSize = 6000,
#'     closeConnections = TRUE
#' )
#'
#' CERRA_ensembles_rast <- Download.reanalysis_cerra_single_levels(
#'     # CDS API call specification
#'     Variable = "2m relative humidity",
#'     LevelType = "Surface or atmosphere",
#'     SoilLayer = "",
#'     DataType = "Ensemble members",
#'     ProductType = "Analysis",
#'     LeadTimeHour = "",
#'     # time-window, default set to range of dataset-type
#'     DateStart = "1993-01-01 01:00",
#'     DateStop = "1993-01-31 22:00",
#'     TimeZone = "CET",
#'     # file naming and output
#'     Dir = getwd(),
#'     FileName = "CERRA_Ensembles.nc",
#'     Compression = NA,
#'     WriteFile = TRUE,
#'     RemoveTemporary = TRUE,
#'     # API credentials
#'     API_User = API_User,
#'     API_Key = API_Key,
#'     # Quality of life and efficiency settings
#'     TChunkSize = 6000,
#'     closeConnections = TRUE
#' )
#' }
#' @export
Download.reanalysis_cerra_single_levels <- function(
    Variable, LevelType, SoilLayer, DataType, ProductType, LeadTimeHour,
    DateStart, DateStop, TimeZone = "UTC",
    Dir = getwd(), FileName, Compression = NA, WriteFile = TRUE, RemoveTemporary = TRUE,
    API_User, API_Key,
    TChunkSize = 6000, closeConnections = TRUE) {
    ## Input Checks =================================
    ### Catching Most Frequent Issues ---------
    if (closeConnections) {
        on.exit(closeAllConnections())
    }
    #--- checking if API User and Key have been supplied
    if (exists("API_User") + exists("API_Key") != 2) {
        stop("Please provide a value for the API_User and API_Key arguments.")
    }
    #--- making API_User into a character string
    API_User <- as.character(API_User)
    #--- Set LeadTimeHour to "" when we aren't querying a forecast
    if (ProductType != "Forecast") {
        LeadTimeHour <- ""
    }

    ### Specification Validation ---------
    message("###### Checking Request Validity")
    #--- FileName
    if (missing(FileName)) {
        stop("Please specify a filename.")
    }
    FileName <- paste0(file_path_sans_ext(FileName), ".nc")
    #--- Time Reformatting
    Dates_df <- Helper.MakeUTC(DatesVec = c(
        as.POSIXct(DateStart, tz = TimeZone),
        as.POSIXct(DateStop, tz = TimeZone)
    ))
    #--- actual checks
    QuickFacts_ls <- Meta.QuickFacts("CDS_reanalysis-cerra-single-levels")
    Variables_df <- Meta.Variables("CDS_reanalysis-cerra-single-levels")
    InCheck_ls <- list(
        Variable = list(
            Input = Variable,
            Allowed = Variables_df$name,
            Operator = "in"
        ),
        Time = list(
            Input = Dates_df$UTC,
            Allowed = c(
                QuickFacts_ls$time$extent[1],
                gsub(" UTC", "", Helper.MakeUTC(as.POSIXct(paste0(format(Sys.time(), "%Y-%m-%d %H"), ":00")))$UTC) # assuming current day and hour as possible end since dataset is released ongoingly
            ),
            Operator = "exceeds"
        ),
        TimeZone = list(
            Input = TimeZone,
            Allowed = OlsonNames(),
            Operator = "in"
        ),
        LevelType = list(
            Input = LevelType,
            Allowed = unlist(Variables_df[Variables_df$name == Variable, ]$"level type"),
            Operator = "in"
        ),
        SoilLayer = list(
            Input = SoilLayer,
            Allowed = unlist(Variables_df[Variables_df$name == Variable, ]$"soil layer"),
            Operator = "in"
        ),
        DataType = list(
            Input = DataType,
            Allowed = unlist(Variables_df[Variables_df$name == Variable, ]$"data type"),
            Operator = "in"
        ),
        ProductType = list(
            Input = ProductType,
            Allowed = unlist(Variables_df[Variables_df$name == Variable, ]$"product type"),
            Operator = "in"
        ),
        LeadTimeHour = list(
            Input = LeadTimeHour,
            Allowed = ifelse(ProductType == "Forecast", QuickFacts_ls$leadtime, ""),
            Operator = "in"
        ),
        LeadTimeHour = list(
            Input = length(LeadTimeHour),
            Allowed = 1,
            Operator = "in"
        )
    )
    Helper.InputChecker(InCheck_ls)

    #--- Metadata
    Citation <- paste0(QuickFacts_ls$name, " (DOI:", QuickFacts_ls$doi, ") data provided by the ECMWF CDS obtained on ", Sys.Date())
    names(Citation) <- "Citation"
    callargs <- mget(names(formals()), sys.frame(sys.nframe()))
    callargs[sapply(callargs, is.null)] <- "NULL"
    callargs[sapply(callargs, class) == "name"] <- ""
    names(callargs) <- paste("Call", names(callargs), sep = "_")
    Meta_vec <- c(Citation, unlist(callargs))

    #--- time assignment, we do this in input timezone format to assign accurate times to final file layers
    if (DataType != "Ensemble members") {
        TRes <- paste(QuickFacts_ls$time$resolution, QuickFacts_ls$time$unit)
    } else {
        TRes <- paste(QuickFacts_ls$time$resolution * 2, QuickFacts_ls$time$unit)
    }
    TimeAssing <- seq(
        from = Dates_df$IN[1],
        to = Dates_df$IN[2],
        by = TRes
    )
    if (DataType == "Ensemble members") {
        TimeAssing <- rep(TimeAssing, each = 10)
    }

    ## File Check =========
    FCheck <- WriteRead.FileCheck(FName = FileName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
        FCheck <- WriteRead.NC(NC = FCheck, FName = file.path(Dir, FileName), Attrs = Meta_vec)
        terra::time(FCheck) <- TimeAssing
        return(FCheck)
    }

    ## The CDS Request =================================
    # message("###### CDS Request & Data Download")

    ### Building ---------
    message("Building CDS Request(s)")

    #--- Name resolving
    QueryVariable <- gsub(" ", "_", tolower(Variable))

    #--- Create timewindows of donwloads
    Allowed_seq <- seq( # sequence that the dataset might support ("might" as end-date cannot be checked)
        from = as.POSIXct(QuickFacts_ls$time$extent[1], tz = "UTC"),
        to = Helper.MakeUTC(as.POSIXct(paste0(format(Sys.time(), "%Y-%m-%d %H"), ":00")))$UTC,
        by = paste(QuickFacts_ls$time$resolution, QuickFacts_ls$time$unit)
    )
    warning(paste("Cannot validate user-specified DateStop argument as", QuickFacts_ls$name, "is released continuously. You may want to consult the download tab at", QuickFacts_ls$url, "to ensure that the data you query is available."))

    RequestWindows <- Helper.CDS.MakeRequestWindows(
        Allowed_seq = Allowed_seq,
        Queried_seq = Helper.MakeUTC(TimeAssing)$UTC,
        TChunkSize
    )

    #--- Building request list
    QueryParams_ls <- list(
        "dataset_short_name" = "reanalysis-cerra-single-levels",
        "dataset" = "reanalysis-cerra-single-levels",
        "product_type" = gsub(" ", "_", tolower(ProductType)),
        "data_type" = gsub(" ", "_", tolower(DataType)),
        "level_type" = gsub(" ", "_", tolower(LevelType)),
        "soil_layer" = gsub(" ", "_", tolower(SoilLayer)),
        "variable" = QueryVariable,
        "time" = paste0(unique(format(Helper.MakeUTC(TimeAssing)$UTC, "%H")), ":00"),
        "leadtime_hour" = LeadTimeHour,
        "data_format" = "grib"
    )
    CDSRequests_ls <- Helper.CDS.MakeRequests(QueryTimeWindows = RequestWindows, QueryParams_ls = QueryParams_ls)

    ### Executing ---------
    message("Executing CDS Request(s)")

    #--- Register credentials
    Helper.CDS.RegisterAPICredentials(API_User = API_User, API_Key = API_Key)

    #--- Looping over requests
    FilesToLoad <- Helper.CDS.ExecuteRequests(CDSRequests_ls, API_User, API_Key)

    ## The Data =================================
    # message("###### The Data")

    ### Loading ---------
    message("###### Loading Downloaded Data from Disk")
    CDS_rast <- Helper.LoadFiles(FilesToLoad)
    if (nlyr(CDS_rast) != length(TimeAssing)) {
        warning("The downloaded data does not contain as many layers as ClimHub expected. You may want to retry this function execution after deleting the downloaded files or file an issue on the ClimHub GitHub here: https://github.com/ErikKusch/ClimHub/issues. To investigate the issue directly on your end, the downloaded data has been returned to your working environment, but the download function was terminated prematurely.")
        return(CDS_rast)
    }
    terra::time(CDS_rast) <- TimeAssing

    ### Time-Window Extraction ---------
    message("###### Extracting Requested Time-Period")
    TimeLyr <- which(
        time(CDS_rast) >= DateStart &
            time(CDS_rast) <= DateStop
    )
    CDS_rast <- CDS_rast[[TimeLyr]]

    ### Checking for correct layer number ---------

    ## Exports =================================
    message("###### Data Export & Return")

    ### Assign additional information
    terra::metags(CDS_rast) <- Meta_vec

    ### write file
    if (WriteFile) {
        CDS_rast <- WriteRead.NC(
            NC = CDS_rast, FName = file.path(Dir, FileName),
            Variable = Variable,
            LongVar = gsub(pattern = " ", replacement = "_", tolower(Variable)),
            Unit = Variables_df[Variables_df$name == Variable, ]$unit,
            Attrs = Meta_vec, Write = TRUE, Compression = Compression
        )
    }

    ### unlink temporary files
    if (RemoveTemporary & WriteFile) {
        unlink(FilesToLoad)
    }

    ### return object
    return(CDS_rast)
}
