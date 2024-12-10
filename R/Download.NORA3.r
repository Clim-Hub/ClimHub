#' Downloading NORA3 3km NOrwegian Reanalysis Data from the Norwegian Meteorological Instite
#'
#' This function is used to obtain the NORA3 data product hosted through \href{https://thredds.met.no/thredds/projects/nora3.html}{thredds.met.no}. Specifically, this function makes available the following datasets:
#'  1. NORA3 files contained within \href{https://thredds.met.no/thredds/catalog/nora3/catalog.html}{nora3}.
#'
#' @param Variable Character. An overview of NORA3 variables can be obtained with Meta.Variables(dataset = "NORA3").
#' @param DateStart Character. "YYYY-MM-DD HH" date at which to start time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param DateStop Character. "YYYY-MM-DD HH" date at which to stop time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param Leadtime Integer. Lead time of reanalysis, either 3, 4, 5, 6, 7, 8 or 9.
#' @param Cores Integer. How many cores to use for parallel downloads. Default NULL defines no parallelisation.
#' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
#' @param FileName Character. A file name for the produced file.
#' @param Compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
#' @param RemoveTemporary Logical. Whether to delete temporary files after completion.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom terra metags
#' @importFrom terra crs
#' @importFrom terra names
#' @importFrom terra time
#' @importFrom stringr str_pad
#'
#' @return A SpatRaster object containing the downloaded data, and a file in the specified directory. The SpatRaster contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#'  - *Citation* - A string which to use for in-line citation of the data product obtained}.
#'
#'
#' @examples
#' \dontrun{
#' NORA3 <- Download.NORA3(
#'     Variable = "TS (Surface temperature)", # which variable
#'     DateStart = "1961-08-01 00", DateStop = "1961-08-02 18", # time-window
#'     Leadtime = 3, Cores = 1,
#'     Dir = getwd(), FileName = "NORA3", Compression = 9, # file storing
#'     RemoveTemporary = TRUE
#' )
#' }
#' @export
Download.NORA3 <- function(
    Variable = "TS (Surface temperature)", # which variable
    DateStart = "1961-08-01 00", DateStop = "2022-12-31 18", # time-window
    Leadtime = 3, Cores = 1,
    Dir = getwd(), FileName, Compression = 9, # file storing
    RemoveTemporary = TRUE) {
    ## Input Checks ============
    message("###### Checking Request Validity")
    # maybe bundle this into one function that takes standard Variable, time, and filename and checks everything else via a list argument?
    ### variable in dataset
    if (!(Variable %in% Meta.Variables(dataset = "NORA3")$name)) {
        stop("Please specify a valid variable for the NORA3 data set. You can get an overview of valid variables for NORA3 data by calling Meta.Variables(dataset = 'NORA3').")
    }

    ### time-window exceeded, we do this in UTC to avoid daylight savings shenanigans
    Start <- as.POSIXct(paste0(DateStart, ":00:00"), tz = "UTC")
    Stop <- as.POSIXct(paste0(DateStop, ":00:00"), tz = "UTC")
    if (sum(format(c(Start, Stop), "%H") %in% c("00", "06", "12", "18")) != 2) {
        stop("Please specify DateStart and DateStop such that they are strings of 'YYYY-MM-DD HH' where HH can be '00', '06', '12', or '18'.")
    }

    if (Start < as.POSIXct(Meta.QuickFacts(dataset = "NORA3")$time$extent[1], tz = "UTC")) {
        stop(paste(
            "Please specify DateStart so that it does not predate the NORA3 data layers. The earliest date you can specify for NORA3 is:",
            format(as.POSIXct(Meta.QuickFacts(dataset = "NORA3")$time$extent[1], tz = "UTC"), "%Y-%m-%d %H")
        ))
    }
    if (Start > as.POSIXct(Meta.QuickFacts(dataset = "NORA3")$time$extent[2], tz = "UTC")) {
        stop(paste(
            "Please specify DateStop so that it does not postdate the NORA3 data layers. The latest date you can specify for NORA3 is:",
            format(as.POSIXct(Meta.QuickFacts(dataset = "NORA3")$time$extent[2], tz = "UTC"), "%Y-%m-%d %H")
        ))
    }

    ### Leadtime
    if (!(Leadtime %in% 3:9)) {
        stop("Please specify a valid leadtime. Valid leadtimes are 3, 4, 5, 6, 7, 8, and 9.")
    }

    ### FileName
    if (missing(FileName)) {
        stop("Please specify a filename.")
    }

    ## FileName Specification
    FileName <- paste0(file_path_sans_ext(FileName), ".nc")

    ## Metadata
    Citation <- paste0("NORA3 (DOI:", Meta.DOI(dataset = "NORA3"), ") data provided by the The Norwegian Meteorological institute obtained on ", Sys.Date())
    names(Citation) <- "Citation"
    callargs <- mget(names(formals()), sys.frame(sys.nframe()))
    callargs[sapply(callargs, is.null)] <- "NULL"
    callargs[sapply(callargs, class) == "name"] <- ""
    names(callargs) <- paste("Call", names(callargs), sep = "_")
    Meta_vec <- c(Citation, unlist(callargs))

    ## Data files & extraction varnames =========
    NORA3_df <- Meta.Variables("NORA3")
    FilePrefix <- NORA3_df$datafile[Variable == NORA3_df$name]
    ExtractVar <- NORA3_df$varname[Variable == NORA3_df$name]

    ## File Check =========

    ## Download preparations =========
    ## temporary files names, we do this in UTC to avoid daylight savings shenanigans
    TimeAssing <- Datetimes <- seq(
        from = Start,
        to = Stop,
        by = "6 hour"
    )
    Datetimes <- format(Datetimes, "%Y%m%d%H")
    FNames <- paste0("TEMP_", "fc", Datetimes, "_", stringr::str_pad(Leadtime, 3, "left", 0), FilePrefix, ".nc")

    FCheck <- WriteRead.FileCheck(FName = FileName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
        FCheck <- WriteRead.NC(NC = FCheck, FName = file.path(Dir, FileName), Attrs = Meta_vec)
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
    FilestoLoad <- Helper.DirectDownload(URLS = URLS, Names = FNames, Cores = Cores, Dir = Dir)

    ## Loading Data =================================
    message("###### Loading Downloaded Data from Disk")
    MetNo_rast <- Helper.LoadFiles(FilestoLoad)
    # terra::crs(MetNo_rast) <- Meta.QuickFacts("NORA3")$space$crs

    ## Variable Extraction =================================
    message("###### Extracting Requested Variable")
    VarLyr <- grep(ExtractVar, names(MetNo_rast))
    MetNo_rast <- MetNo_rast[[VarLyr]]

    ## Exports =================================
    message("###### Data Export & Return")

    ### Assign additional information
    terra::time(MetNo_rast) <- TimeAssing

    ### write file
    MetNo_rast <- WriteRead.NC(
        NC = MetNo_rast, FName = file.path(Dir, FileName),
        Variable = Variable,
        Unit = ifelse(length(unique(terra::units(MetNo_rast))) > 1, unique(terra::units(MetNo_rast))[2], unique(terra::units(MetNo_rast))), # clunky, but won't need this when integrating proper metadata solution
        Attrs = Meta_vec, Write = TRUE, Compression = Compression
    )

    ### unlink temporary files
    if (RemoveTemporary) {
        unlink(file.path(Dir, FNames))
    }

    ### return object
    return(MetNo_rast)
}
