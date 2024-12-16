#' Downloading Klima i Norge 2100 Data from the Norwegian Meteorological Instite
#'
#' This function is used to obtain the \href{https://www.miljodirektoratet.no/publikasjoner/2015/september-2015/klima-i-norge-2100/}{Klima_i_Norge_2100} data product hosted through \href{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge_2100/catalog.html}{thredds.met.no}. Specifically, this function makes available the following datasets:
#'  1. \href{https://publikasjoner.nve.no/rapport/2016/rapport2016_59.pdf}{Gridded 1 x 1 km climate and hydrological projections for Norway} data contained within \href{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge_2100/utgave2015/catalog.html}{utgave 2015}.
#'
#' @param Variable Character. An overview of Klima i Norge variables can be obtained with `Meta.Variables(dataset = "KlimaiNorge2100")`.
#' @param DateStart Character. "YYYY-MM-DD" date at which to start time series of downloaded data. Data is available daily at hourly intervals.
#' @param DateStop Character. "YYYY-MM-DD" date at which to stop time series of downloaded data. Data is available daily at hourly intervals.
#' @param Model Character. An overview of climate models from which data can be obtained can be obtained with `Meta.QuickFacts("KlimaiNorge2100")$models`.
#' @param Scenario Character. An overview of climate models from which data can be obtained can be obtained with `Meta.QuickFacts("KlimaiNorge2100")$scenarios`. Note that this choice only affects data post-dating 2005-12-31.
#' @param Cores Integer. How many cores to use for parallel downloads. Default NULL defines no parallelisation.
#' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
#' @param FileName Character. A file name for the produced file.
#' @param Compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
#' @param RemoveTemporary Logical. Whether to delete temporary files after completion.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom terra metags
#' @importFrom terra time
#'
#' @return A SpatRaster object containing the downloaded data, and a file in the specified directory. The SpatRaster contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#'  - *Citation* - A string which to use for in-line citation of the data product obtained.
#'  - *Call_* - A set of strings matching arguments supplied to the download function call
#'
#'
#' @examples
#' \dontrun{
#' Download.KlimaiNorge2100(
#'     Variable = "Maximum Air Temperature",
#'     DateStart = "1999-08-01",
#'     DateStop = "2010-09-17",
#'     Model = "CNRM_CCLM",
#'     Scenario = "rcp85",
#'     Cores = 1,
#'     Dir = getwd(),
#'     FileName = "KlimaiNorge2100",
#'     Compression = 9,
#'     RemoveTemporary = TRUE
#' )
#' }
#' @export
Download.KlimaiNorge2100 <- function(
    Variable, # which variable
    DateStart, DateStop, # time-window
    Model, Scenario, # Klima i Norge 2100 specific arguments
    Cores = 1,
    Dir = getwd(), FileName, Compression = 9, # file storing
    RemoveTemporary = TRUE) {
    ## Input Checks ============
    message("###### Checking Request Validity")

    ### FileName
    if (missing(FileName)) {
        stop("Please specify a filename.")
    }
    FileName <- paste0(file_path_sans_ext(FileName), ".nc")

    ### time-window exceeded, we do this in UTC to avoid daylight savings shenanigans
    Start <- as.POSIXct(paste0(DateStart, ":00:00"), tz = "UTC")
    Stop <- as.POSIXct(paste0(DateStop, ":00:00"), tz = "UTC")

    ### actual checks
    InCheck_ls <- list(
        Variable = list(
            Input = Variable,
            Allowed = Meta.Variables("KlimaiNorge2100")$name,
            Operator = "in"
        ),
        Time = list(
            Input = c(Start, Stop),
            Allowed = Meta.QuickFacts("KlimaiNorge2100")$time$extent,
            Operator = "exceeds"
        ),
        Models = list(
            Input = Model,
            Allowed = Meta.QuickFacts("KlimaiNorge2100")$models,
            Operator = "in"
        ),
        Scenarios = list(
            Input = Scenario,
            Allowed = Meta.QuickFacts("KlimaiNorge2100")$scenarios,
            Operator = "in"
        )
    )
    Helper.InputChecker(InCheck_ls)

    ## Metadata
    Citation <- paste0("Klima i Norge 2100 (ISBN:", Meta.QuickFacts(dataset = "KlimaiNorge2100")$isbn, ") data provided by the The Norwegian Meteorological institute obtained on ", Sys.Date())
    names(Citation) <- "Citation"
    callargs <- mget(names(formals()), sys.frame(sys.nframe()))
    callargs[sapply(callargs, is.null)] <- "NULL"
    callargs[sapply(callargs, class) == "name"] <- ""
    names(callargs) <- paste("Call", names(callargs), sep = "_")
    Meta_vec <- c(Citation, unlist(callargs))

    ## Data files & extraction varnames =========
    KlimaiNorge2100_df <- Meta.Variables("KlimaiNorge2100")
    FilePrefix <- KlimaiNorge2100_df$prefix[Variable == KlimaiNorge2100_df$name]
    Unit <- KlimaiNorge2100_df$unit[Variable == KlimaiNorge2100_df$name]

    ## Download preparations =========
    ## temporary files names, we do this in UTC to avoid daylight savings shenanigans
    TimeAssing <- Datetimes <- seq(
        from = Start,
        to = Stop,
        by = "1 day"
    )
    Datetimes <- unique(format(Datetimes, "%Y"))
    FNames <- paste("TEMP", ifelse(Datetimes < 2006, "hist", Scenario), Model, FilePrefix, "daily", Datetimes, "v4.nc", sep = "_")

    ## File Check =========
    FCheck <- WriteRead.FileCheck(FName = FileName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
        FCheck <- WriteRead.NC(NC = FCheck, FName = file.path(Dir, FileName), Attrs = Meta_vec)
        terra::time(FCheck) <- TimeAssing
        return(FCheck)
    }

    ## Download execution =========
    message("###### Data Download")
    URLS <- sapply(FNames, FUN = function(FName) {
        FNameInfo <- unlist(strsplit(FName, split = "_"))
        paste("https://thredds.met.no/thredds/fileServer/KSS/Klima_i_Norge_2100/utgave2015", FilePrefix, Model, FNameInfo[2],
            gsub(FName, pattern = "TEMP_", replacement = ""),
            sep = "/"
        )
    })
    FilestoLoad <- Helper.DirectDownload(URLS = URLS, Names = FNames, Cores = Cores, Dir = Dir)

    ## Loading Data =================================
    message("###### Loading Downloaded Data from Disk")
    MetNo_rast <- Helper.LoadFiles(FilestoLoad)

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

    ### write file
    MetNo_rast <- WriteRead.NC(
        NC = MetNo_rast, FName = file.path(Dir, FileName),
        Variable = Variable,
        Unit = Unit,
        Attrs = Meta_vec, Write = TRUE, Compression = Compression
    )

    ### unlink temporary files
    if (RemoveTemporary) {
        unlink(file.path(Dir, FNames))
    }

    ### return object
    return(MetNo_rast)
}
