#' @title Download Klima i Norge 2100 data from the Norwegian Meteorological Institute
#'
#' @description Downloads and processes data from the \href{https://www.miljodirektoratet.no/publikasjoner/2015/september-2015/klima-i-norge-2100/}{Klima_i_Norge_2100} data product hosted through \href{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge_2100/catalog.html}{thredds.met.no}. 
#' Specifically, this function provides access to the following datasets:
#'  1. \href{https://publikasjoner.nve.no/rapport/2016/rapport2016_59.pdf}{Gridded 1 x 1 km climate and hydrological projections for Norway} data contained within \href{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge_2100/utgave2015/catalog.html}{utgave 2015}.
#'
#' @param variable Character. An overview of Klima i Norge variables can be obtained with `Discovery_Variables(dataSet = "KlimaiNorge2100")`.
#' @param dateStart Character. "YYYY-MM-DD" date at which to start time series of downloaded data. Data is available daily at hourly intervals.
#' @param dateStop Character. "YYYY-MM-DD" date at which to stop time series of downloaded data. Data is available daily at hourly intervals.
#' @param model Character. An overview of climate models from which data can be obtained can be obtained with `Discovery_QuickFacts("KlimaiNorge2100")$models`.
#' @param scenario Character. An overview of climate models from which data can be obtained can be obtained with `Discovery_QuickFacts("KlimaiNorge2100")$scenarios`. Note that this choice only affects data post-dating 2005-12-31.
#' @param cores Optional, Integer. Number of cores to use for parallel downloads. Default NULL defines no parallelisation.
#' @param fileName Character. A file name for the produced file, including path.
#' @param compression Optional, Integer. Compression level between 1 to 9 applied to final .nc file. Same as compression argument in terra::writeCDF(). Defaults to NA.
#' @param removeTemporary Optional, Logical. Whether to delete temporary files after completion. Defaults to TRUE.
#' @param writeFile Optional, Logical. Whether to write final SpatRaster to disk as an .nc or to return information from memory. Setting to FALSE will prohibit removal of temporary files. Defaults to TRUE.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom terra metags
#' @importFrom terra time
#'
#' @return SpatRaster. Contains the downloaded data and metadata attributes that can be retrieved with terra::metags(...):
#'  - *Citation* - A string for in-line citation of the data product
#'  - *Call_* - A set of strings matching arguments supplied to the download function call
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' Access_KlimaiNorge2100(
#'     variable = "Maximum Air Temperature",
#'     dateStart = "2004-08-01",
#'     dateStop = "2006-09-17",
#'     model = "CNRM_CCLM",
#'     scenario = "rcp85",
#'     cores = 1,
#'     fileName = "KlimaiNorge2100.nc",
#'     compression = 9,
#'     removeTemporary = TRUE
#' )
#' }
#' @export
Access_KlimaiNorge2100 <- function(
    variable, # which variable
    dateStart, dateStop, # time-window
    model,
    scenario = c("rcp45", "rcp85"), # Klima i Norge 2100 specific arguments
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
    InCheck_ls <- list(
        Variable = list(
            Input = variable,
            Allowed = Discovery_Variables("KlimaiNorge2100")$name,
            Operator = "in"
        ),
        Time = list(
            Input = c(Start, Stop),
            Allowed = Discovery_QuickFacts("KlimaiNorge2100")$time$extent,
            Operator = "exceeds"
        ),
        Models = list(
            Input = model,
            Allowed = Discovery_QuickFacts("KlimaiNorge2100")$models,
            Operator = "in"
        ),
        Scenarios = list(
            Input = scenario,
            Allowed = Discovery_QuickFacts("KlimaiNorge2100")$scenarios,
            Operator = "in"
        )
    )
    Helper_InputChecker(inputCheck = InCheck_ls)

    ## Metadata
    Citation <- paste0("Klima i Norge 2100 (ISBN:", Discovery_QuickFacts(dataSet = "KlimaiNorge2100")$isbn, ") data provided by the The Norwegian Meteorological institute obtained on ", Sys.Date())
    names(Citation) <- "Citation"
    callargs <- mget(names(formals()), sys.frame(sys.nframe()))
    callargs[sapply(callargs, is.null)] <- "NULL"
    callargs[sapply(callargs, class) == "name"] <- ""
    names(callargs) <- paste("Call", names(callargs), sep = "_")
    Meta_vec <- c(Citation, unlist(callargs))

    ## Data files & extraction varnames =========
    KlimaiNorge2100_df <- Discovery_Variables("KlimaiNorge2100")
    FilePrefix <- KlimaiNorge2100_df$prefix[variable == KlimaiNorge2100_df$name]
    Unit <- KlimaiNorge2100_df$unit[variable == KlimaiNorge2100_df$name]

    ## Download preparations =========
    ## temporary files names, we do this in UTC to avoid daylight savings shenanigans
    TimeAssing <- Datetimes <- seq(
        from = Start,
        to = Stop,
        by = "1 day"
    )
    Datetimes <- unique(format(Datetimes, "%Y"))
    FNames <- paste("TEMP", ifelse(Datetimes < 2006, "hist", scenario), model, FilePrefix, "daily", Datetimes, "v4.nc", sep = "_")

    ## File Check =========
    FCheck <- Helper_FileCheck(fileName = fileName, loadFun = NC_Read, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
        terra::time(FCheck) <- TimeAssing
        return(FCheck)
    }

    ## Download execution =========
    message("###### Data Download")
    URLS <- sapply(FNames, FUN = function(FName) {
        FNameInfo <- unlist(strsplit(FName, split = "_"))
        paste("https://thredds.met.no/thredds/fileServer/KSS/Klima_i_Norge_2100/utgave2015", FilePrefix, model, FNameInfo[2],
            gsub(FName, pattern = "TEMP_", replacement = ""),
            sep = "/"
        )
    })
    FilestoLoad <- Helper_DirectDownload(url = URLS, fileName = FNames, cores = cores, verbose = TRUE)

    ## Loading Data =================================
    message("###### Loading Downloaded Data from Disk")
    MetNo_rast <- Helper_LoadFiles(fileName = FilestoLoad)

    ## Time-Window Extraction =================================
    message("###### Extracting Requested Time-Period")
    TimeLyr <- which(
        as.POSIXct(time(MetNo_rast), tz = "UTC") >= Start &
            as.POSIXct(time(MetNo_rast), tz = "UTC") <= Stop
    )
    MetNo_rast <- MetNo_rast[[TimeLyr]]

    ## Exports =================================
    message("###### Data Export & Return")

    ### Assign additional information
    terra::time(MetNo_rast) <- TimeAssing
    terra::metags(MetNo_rast) <- Meta_vec

    ### write file
    if (writeFile) {
        NC_Write(
            spatRaster = MetNo_rast, fileName = fileName,
            varName = variable,
            longName = gsub(pattern = " ", replacement = "_", tolower(variable)),
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
