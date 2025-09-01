#' Downloading NORA3 3km NOrwegian Reanalysis Data from the Norwegian Meteorological Instite
#'
#' This function is used to obtain the NORA3 data product hosted through \href{https://thredds.met.no/thredds/projects/nora3.html}{thredds.met.no}. Specifically, this function makes available the following datasets:
#'  1. NORA3 files contained within \href{https://thredds.met.no/thredds/catalog/nora3/catalog.html}{nora3}.
#'
#' @param Variable Character. An overview of NORA3 variables can be obtained with `Meta.Variables(dataset = "NORA3")`.
#' @param DateStart Character. "YYYY-MM-DD HH" date at which to start time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param DateStop Character. "YYYY-MM-DD HH" date at which to stop time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param Leadtime Integer. Lead time of reanalysis. NORA3 leadtimes can be obtained with `Meta.QuickFacts("NORA3")$leadtime`.
#' @param Cores Integer. How many cores to use for parallel downloads. Default NULL defines no parallelisation.
#' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
#' @param FileName Character. A file name for the produced file.
#' @param Compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
#' @param RemoveTemporary Logical. Whether to delete temporary files after completion.
#' @param WriteFile Logical. Whether to write final SpatRaster to disk as an .nc or to return information from memory. Note that setting WriteFile = FALSE will prohibit removal of temporary files.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom terra metags
#' @importFrom terra names
#' @importFrom terra time
#' @importFrom stringr str_pad
#'
#' @return A SpatRaster object containing the downloaded data, and a file in the specified directory. The SpatRaster contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#'  - *Citation* - A string which to use for in-line citation of the data product obtained.
#'  - *Call_* - A set of strings matching arguments supplied to the download function call.
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
    Variable, # which variable
    DateStart, DateStop, # time-window
    Leadtime, # NORA3 specific arguments
    Cores = 1,
    Dir = getwd(), FileName, Compression = NA, # file storing
    RemoveTemporary = TRUE,
    WriteFile = TRUE) {
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
    QuickFacts_ls <- QuickFacts_ls
    warning(paste("Cannot validate user-specified DateStop argument as", QuickFacts_ls$name, "is released continuously. You may want to consult the download tab at", QuickFacts_ls$url, "to ensure that the data you query is available."))
    InCheck_ls <- list(
        Variable = list(
            Input = Variable,
            Allowed = Meta.Variables("NORA3")$name,
            Operator = "in"
        ),
        Time = list(
            Input = c(Start, Stop),
            Allowed = c(QuickFacts_ls$time$extent[1], paste0(format(Sys.time(), "%Y-%m-%d %H"), ":00")), # assuming current day and hour as possible end since dataset is released ongoingly
            Operator = "exceeds"
        ),
        Leadtime = list(
            Input = Leadtime,
            Allowed = QuickFacts_ls$leadtime,
            Operator = "in"
        ),
        HourCheck = list(
            Input = format(c(Start, Stop), "%H"),
            Allowed = c("00", "06", "12", "18"),
            Operator = "in"
        )
    )
    Helper.InputChecker(InCheck_ls)

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
    Unit <- NORA3_df$unit[Variable == NORA3_df$name]

    ## Download preparations =========
    ## temporary files names, we do this in UTC to avoid daylight savings shenanigans
    TimeAssing <- Datetimes <- seq(
        from = Start,
        to = Stop,
        by = "6 hour"
    )
    Datetimes <- format(Datetimes, "%Y%m%d%H")
    FNames <- paste0("TEMP_", "fc", Datetimes, "_", stringr::str_pad(Leadtime, 3, "left", 0), FilePrefix, ".nc")

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
    MetNo_rast <- Helper.LoadFiles(FilestoLoad, TimeAssign = TimeAssing)

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
    if (WriteFile) {
        MetNo_rast <- WriteRead.NC(
            NC = MetNo_rast, FName = file.path(Dir, FileName),
            Variable = Variable,
            LongVar = ExtractVar,
            Unit = Unit,
            Attrs = Meta_vec, Write = TRUE, Compression = Compression
        )
    }

    ### unlink temporary files
    if (RemoveTemporary & WriteFile) {
        unlink(file.path(Dir, FNames))
    }

    ### return object
    return(MetNo_rast)
}
