#' @title Access NORA3 3km Norwegian Reanalysis Data
#'
#' @description Downloads and processes data from the NORA3 data product hosted through \href{https://thredds.met.no/thredds/projects/nora3.html}{thredds.met.no}.
#' Specifically, this function provides access to the NORA3 files contained within \href{https://thredds.met.no/thredds/catalog/nora3/catalog.html}{nora3}.
#'
#' @param variable Character. An overview of NORA3 variables can be obtained with `Discovery_Variables(dataSet = "NORA3")`.
#' @param dateStart Character. "YYYY-MM-DD HH" date at which to start time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param dateStop Character. "YYYY-MM-DD HH" date at which to stop time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param leadTimeHour Integer. Lead time of reanalysis. NORA3 leadtimes can be obtained with `Discovery_QuickFacts("NORA3")$leadtime`.
#' @param cores Optional, Integer. Number of cores to use for parallel downloads. Default NULL defines no parallelisation.
#' @param fileName Character. A file name for the produced file, including path.
#' @param compression Optional, Integer. Compression level between 1 to 9 applied to final .nc file. Same as compression argument in terra::writeCDF(). Defaults to NA.
#' @param removeTemporary Optional, Logical. Whether to delete temporary files after completion. Defaults to TRUE.
#' @param writeFile Optional, Logical. Whether to write final SpatRaster to disk as an .nc or to return information from memory. Setting to FALSE will prohibit removal of temporary files. Defaults to TRUE.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom terra metags
#' @importFrom terra names
#' @importFrom terra time
#' @importFrom stringr str_pad
#'
#' @return SpatRaster. Contains the downloaded data and metadata attributes that can be retrieved with terra::metags(...):
#'  - *Citation* - A string for in-line citation of the data product
#'  - *Call_* - A set of strings matching arguments supplied to the download function call
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' NORA3 <- Access_NORA3(
#'     variable = "TS (Surface temperature)", # which variable
#'     dateStart = "1961-08-01 00", dateStop = "1961-08-02 18", # time-window
#'     leadTimeHour = 3, cores = 1,
#'     fileName = "NORA3.nc", compression = 9, # file storing
#'     removeTemporary = TRUE
#' )
#' }
#' @export
Access_NORA3 <- function(
    variable, # which variable
    dateStart, dateStop, # time-window
    leadTimeHour, # NORA3 specific arguments
    cores = 1,
    fileName, compression = NA, # file storing
    removeTemporary = TRUE,
    writeFile = TRUE) {
    ## Input Checks ============
    message("###### Checking Request Validity")

    ### fileName
    if (missing(fileName)) {
        stop("Please specify a filename.")
    }
    fileName <- normalizePath(fileName, mustWork = FALSE)

    ### time-window exceeded, we do this in UTC to avoid daylight savings shenanigans
    Start <- as.POSIXct(paste0(dateStart, ":00:00"), tz = "UTC")
    Stop <- as.POSIXct(paste0(dateStop, ":00:00"), tz = "UTC")

    ### actual checks
    QuickFacts_ls <- Discovery_QuickFacts("NORA3")
    warning(paste("Cannot validate user-specified dateStop argument as", QuickFacts_ls$name, "is released continuously. You may want to consult the download tab at", QuickFacts_ls$url, "to ensure that the data you query is available."))
    InCheck_ls <- list(
        Variable = list(
            Input = variable,
            Allowed = Discovery_Variables("NORA3")$name,
            Operator = "in"
        ),
        Time = list(
            Input = c(Start, Stop),
            Allowed = c(QuickFacts_ls$time$extent[1], paste0(format(Sys.time(), "%Y-%m-%d %H"), ":00")), # assuming current day and hour as possible end since dataset is released ongoingly
            Operator = "exceeds"
        ),
        leadTimeHour = list(
            Input = leadTimeHour,
            Allowed = QuickFacts_ls$leadtime,
            Operator = "in"
        ),
        HourCheck = list(
            Input = format(c(Start, Stop), "%H"),
            Allowed = c("00", "06", "12", "18"),
            Operator = "in"
        )
    )
    Helper_InputChecker(inputCheck = InCheck_ls)

    ## Metadata
    Citation <- paste0("NORA3 (DOI:", Discovery_DOI(dataSet = "NORA3"), ") data provided by the The Norwegian Meteorological institute obtained on ", Sys.Date())
    names(Citation) <- "Citation"
    callargs <- mget(names(formals()), sys.frame(sys.nframe()))
    callargs[sapply(callargs, is.null)] <- "NULL"
    callargs[sapply(callargs, class) == "name"] <- ""
    names(callargs) <- paste("Call", names(callargs), sep = "_")
    Meta_vec <- c(Citation, unlist(callargs))

    ## Data files & extraction varnames =========
    NORA3_df <- Discovery_Variables("NORA3")
    FilePrefix <- NORA3_df$datafile[variable == NORA3_df$name]
    ExtractVar <- NORA3_df$varname[variable == NORA3_df$name]
    Unit <- NORA3_df$unit[variable == NORA3_df$name]

    ## Download preparations =========
    ## temporary files names, we do this in UTC to avoid daylight savings shenanigans
    TimeAssing <- Datetimes <- seq(
        from = Start,
        to = Stop,
        by = "6 hour"
    )
    Datetimes <- format(Datetimes, "%Y%m%d%H")
    FNames <- paste0("TEMP_", "fc", Datetimes, "_", stringr::str_pad(leadTimeHour, 3, "left", 0), FilePrefix, ".nc")

    ## File Check =========
    FCheck <- Helper_FileCheck(fileName = fileName, loadFun = NC_Read, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
        terra::time(FCheck) <- TimeAssing
        return(FCheck)
    }

    ## Download execution =========
    message("###### Data Download")
    URLS <- sapply(FNames, FUN = function(FName) {
        Year <- substr(FName, 8, 11)
        Month <- substr(FName, 12, 13)
        Day <- substr(FName, 14, 15)
        Hour <- substr(FName, 16, 17)
        paste("https://thredds.met.no/thredds/fileServer/nora3", Year, Month, Day, Hour,
            gsub(FName, pattern = "TEMP_", replacement = ""),
            sep = "/"
        )
    })
    FilestoLoad <- Helper_DirectDownload(url = URLS, fileName = FNames, cores = cores)

    ## Loading Data =================================
    message("###### Loading Downloaded Data from Disk")
    MetNo_rast <- Helper_LoadFiles(fileName = FilestoLoad, dates = TimeAssing)

    ## Variable Extraction =================================
    message("###### Extracting Requested Variable")
    VarLyr <- which(startsWith(names(MetNo_rast), ExtractVar))
    MetNo_rast <- MetNo_rast[[VarLyr]]

    ## Exports =================================
    message("###### Data Export & Return")

    # ### Assign additional information, handled above now
    # terra::time(MetNo_rast) <- TimeAssing
    terra::metags(MetNo_rast) <- Meta_vec

    ### write file
    if (writeFile) {
        NC_Write(
            spatRaster = MetNo_rast, fileName = fileName,
            varName = variable,
            longName = ExtractVar,
            unit = Unit,
            meta = Meta_vec, compression = compression
        )
        MetNo_rast <- NC_Read(fileName = fileName)
    }

    ### unlink temporary files
    if (removeTemporary & writeFile) {
        unlink(FNames)
    }

    ### return object
    return(MetNo_rast)
}
