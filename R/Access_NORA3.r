#' @title Access NORA3 3km Norwegian Reanalysis Data
#'
#' @description Downloads and processes data from the NORA3 data product hosted through \href{https://thredds.met.no/thredds/projects/nora3.html}{thredds.met.no}.
#' Specifically, this function provides access to the NORA3 files contained within \href{https://thredds.met.no/thredds/catalog/nora3/catalog.html}{nora3}.
#'
#' @param variable Character. An overview of NORA3 variables can be obtained with `Discovery_Variables(dataSet = "NORA3")`.
#' @param dateStart Character. "YYYY-MM-DD HH" date at which to start time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param dateStop Character. "YYYY-MM-DD HH" date at which to stop time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param extent Optional. The extent to subset the data to, in decimal degrees of longitude and latitude. A numeric vector of length 4 with values minimum and maximum longitude and minimum and maximum latitude, in that order. Defaults to `NULL`, returning full spatial range of data.
#' @param leadTimeHour Integer. Lead time of reanalysis. NORA3 lead times can be obtained with `Discovery_QuickFacts("NORA3")$leadtime`.
#' @param fileName Character. A file name for the produced file, including path.
#' @param compression Optional, integer. Compression level between 1 to 9 applied to final netCDF file. Defaults to 1. Currently not used due to ncdfCF saving scheme.
#' @return A `CFDataset` object which contains the downloaded data and relevant metadata attributes. If specified, also writes a netCDF file to disk.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' NORA3 <- Access_NORA3(
#'     variable = "TS", # which variable
#'     dateStart = "1961-08-01 00", dateStop = "1961-08-02 18", # time-window
#'     extent = c(0, 40, 50, 60),
#'     leadTimeHour = 3,
#'     fileName = "NORA3.nc", compression = 9 # file storing
#' )
#' }
#' @export
Access_NORA3 <- function(
    variable, # which variable
    dateStart, dateStop, # time-window
    extent = NULL,
    leadTimeHour, # NORA3 specific arguments
    fileName, compression = 1 # file storing
) {
    ## Input Checks ============
    message("###### Checking Request Validity")
  ### fileName
  if (missing(fileName))
    fileName <- NULL
  if (!is.null(fileName))
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

    # Commenting this out so you can also subset on x/y coordinates
    # if (exists("extent")) {
    #     InCheck_ls <- c(
    #         InCheck_ls,
    #         list(
    #             Extent_Longitude = list(
    #                 Input = extent[1:2],
    #                 Allowed = QuickFacts_ls$space$extent[1:2],
    #                 Operator = "exceeds"
    #             ),
    #             Extent_Latitude = list(
    #                 Input = extent[3:4],
    #                 Allowed = QuickFacts_ls$space$extent[3:4],
    #                 Operator = "exceeds"
    #             )
    #         )
    #     )
    # }

    Helper_InputChecker(inputCheck = InCheck_ls)

    ## Data files & extraction varnames =========
    NORA3_df <- Discovery_Variables("NORA3")
    FilePrefix <- NORA3_df$datafile[NORA3_df$name == variable]

    ## Download preparations =========
    ## temporary files names used for URL creation, we do this in UTC to avoid daylight savings shenanigans
    DateTimes <- seq(
        from = Start,
        to = Stop,
        by = "6 hour"
    )
    Datetimes <- format(DateTimes, "%Y%m%d%H")
    FNames <- paste0("TEMP_", "fc", Datetimes, "_", stringr::str_pad(leadTimeHour, 3, "left", 0), FilePrefix, ".nc")

    ## File Check =========
    if (!is.null(fileName)) {
      FCheck <- Helper_FileCheck(fileName = fileName, loadFun = ncdfCF::open_ncdf, load = TRUE, verbose = TRUE)
      if (!is.null(FCheck))
          return(FCheck)
    }

    ## Subsetting parameters =========
    subset <- if (!is.null(extent)) {
      if (all(extent < 1000))  # Differentiate between latitude/longitude and x/y
        list(longitude = extent[1:2], latitude = extent[3:4])
      else
        list(x = extent[1:2], y = extent[3:4])
    } else list()
    subset <- c(subset, list(time = c(as.character(Start), as.character(Stop + 21600)))) # Stop date/hour inclusive

    ## Download execution =========
    message("###### Data Download")
    URLs <- sapply(FNames, FUN = function(FName) {
        Year <- substr(FName, 8, 11)
        Month <- substr(FName, 12, 13)
        Day <- substr(FName, 14, 15)
        Hour <- substr(FName, 16, 17)
        paste("https://thredds.met.no/thredds/dodsC/nora3", Year, Month, Day, Hour,
            gsub(FName, pattern = "TEMP_", replacement = ""),
            sep = "/"
        )
    })

    MetNo_cf <- Helper_AccessCF(URLs = URLs, variable = variable, subset = subset)

    ## Exports =================================
    message("###### Data Export & Return")

    ## Metadata
    callargs <- paste0("ClimHub::", paste(deparse(match.call()), collapse = ", "))
    Citation <- paste0("NORA3 (DOI:", Discovery_DOI(dataSet = "NORA3"), ") data provided by the The Norwegian Meteorological institute obtained on ", Sys.Date())
    MetNo_cf$set_attribute("source", "NC_CHAR", Citation)
    MetNo_cf$set_attribute("comment", "NC_CHAR", paste("Created on", Sys.time(), "with the Access_NORA3() function from ClimHub version", packageVersion("ClimHub")))
    MetNo_cf$set_attribute("provenance", "NC_CHAR", callargs)

    ### Optionally write file and return
    if (is.null(fileName)) {
      ds <- ncdfCF::create_ncdf()
      ds$add_variable(MetNo_cf)
      ds
    } else
      MetNo_cf$save(fileName)
}
