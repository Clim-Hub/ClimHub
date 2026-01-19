#' @title Download Klima i Norge 2100 data from the Norwegian Meteorological Institute
#'
#' @description Downloads and processes data from the \href{https://www.met.no/nyhetsarkiv/framtidens-klima-i-norge-flere-oversvommelser-mer-torke-og-mindre-sno/_/attachment/inline/cc1ae8f3-277d-4077-adb9-313e5e28b947:b8073784517edb9392f78c9d82bc5b296db2fc3c/Klima%20i%20Norge%20digital%20low.pdf}{Klima_i_Norge_2100} data product hosted through \href{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge/utgave2025/catalog.html}{thredds.met.no}.
#' Specifically, this function provides access to the following datasets:
#'  1. Gridded 1 x 1 km climate and hydrological projections for Norway data at daily scales contained within \href{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries/catalog.html}{DailyTimeSeries}.
#'
#' \textbf{Note: not all combinations of variables, models, scenarios, and bias-correction methods are available. If you encounter an error, please first consult whether data is provided for the requested combination at \url{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries/catalog.html}. You will likely see an error like: `Error: Error opening netCDF resource`}
#'
#' @param variable Character. An overview of Klima i Norge variables can be obtained with `Discovery_Variables(dataSet = "KlimaiNorge2100")`.
#' @param dateStart Character. "YYYY-MM-DD" date at which to start time series of downloaded data. Data is available daily at hourly intervals.
#' @param dateStop Character. "YYYY-MM-DD" date at which to stop time series of downloaded data. Data is available daily at hourly intervals.
#' @param extent Optional, SpatExtent. A spatial extent to subset the data to. Defaults to NULL returning full spatial range of data.
#' @param method Character. An overview of bias-correction methods from which data can be obtained can be obtained with `Discovery_QuickFacts("KlimaiNorge2100")$methods`.
#' @param model Character. An overview of climate models from which data can be obtained can be obtained with `Discovery_QuickFacts("KlimaiNorge2100")$models`.
#' @param scenario Character. An overview of climate models from which data can be obtained can be obtained with `Discovery_QuickFacts("KlimaiNorge2100")$scenarios`. Note that this choice only affects data post-dating 2020-12-31.
#' @param fileName Character. A file name for the produced file, including path.
#' @param compression Optional, Integer. Compression level between 1 to 9 applied to final .nc file. Same as compression argument in terra::writeCDF(). Defaults to NA. Currently not used due to ncdfCF saving scheme.
#' @param writeFile Optional, Logical. Whether to write final CFVariable to disk as an .nc or to return information from memory. Defaults to TRUE.
#'
#' @importFrom tools file_path_sans_ext
#'
#' @return A CFVariable object which contains the downloaded data and relevant metadata attributes. If specified, also writes a NetCDF file to disk.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' Access_KlimaiNorge2100(
#'     variable = "mean_air_temperature",
#'     dateStart = "2019-08-01",
#'     dateStop = "2022-09-17", ,
#'     extent = terra::ext(c(0, 10, 60, 65)),
#'     method = "EQM - Empirical Quantile Mapping",
#'     model = "noresm-r1i1p1-remo",
#'     scenario = "rcp45",
#'     fileName = "KlimaiNorge2100.nc",
#'     compression = 9,
#'     writeFile = TRUE
#' )
#' }
#' @export
Access_KlimaiNorge2100 <- function(
    variable, # which variable
    dateStart, dateStop, # time-window
    method,
    model,
    scenario,
    fileName, compression = NA, # file storing
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
        Methods = list(
            Input = method,
            Allowed = Discovery_QuickFacts("KlimaiNorge2100")$methods,
            Operator = "in"
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

    ## Data files & extraction varnames =========
    KlimaiNorge2100_df <- Discovery_Variables("KlimaiNorge2100")
    FilePrefix <- KlimaiNorge2100_df$datafile[KlimaiNorge2100_df$name == variable]
    FileString <- KlimaiNorge2100_df$string[KlimaiNorge2100_df$name == variable]
    Unit <- KlimaiNorge2100_df$unit[KlimaiNorge2100_df$name == variable]

    ## Download preparations =========
    ## temporary files names, we do this in UTC to avoid daylight savings shenanigans
    TimeAssign <- Datetimes <- seq(
        from = Start,
        to = Stop,
        by = "1 day"
    )
    Datetimes <- unique(format(Datetimes, "%Y"))
    FNames <- paste("TEMP", ifelse(Datetimes < 2020, "hist", scenario), model, FilePrefix, "daily", Datetimes, "v4.nc", sep = "_")

    ## File Check =========
    FCheck <- Helper_FileCheck(fileName = fileName, loadFun = ncdfCF::open_ncdf, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
        # terra::time(FCheck) <- TimeAssign # not needed anymore as soon as we switch to CFDataset handling
        return(FCheck)
    }

    ## Download execution =========
    message("###### Data Download")
    URLS <- sapply(FNames, FUN = function(FName) {
        FNameInfo <- unlist(strsplit(FName, split = "_"))
        paste("https://thredds.met.no/thredds/dodsC/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries", FilePrefix, method,
            FNameInfo[2], # this is the scenario now
            model,
            paste0(model, "_", FNameInfo[2], "_eqm-", FileString, "_norway_1km_", FilePrefix, "_daily_", FNameInfo[6], ".nc4"),
            sep = "/"
        )
    })

    MetNo_cf <- Helper_AccessCF(
        URLS = URLS,
        extent = NULL, #!! does not work for some reason wit extent
        time = as.character(c(TimeAssign[1], tail(TimeAssign, 1))),
        variable = FilePrefix
    )

    ## Exports =================================
    message("###### Data Export & Return")

    ## Metadata
    callargs <- paste0("ClimHub::", deparse(match.call()))
    Citation <- paste0("Klima i Norge (DOI:", Discovery_DOI(dataSet = "KlimaiNorge2100"), ") data provided by the The Norwegian Meteorological institute obtained on ", Sys.Date())
    MetNo_cf$set_attribute("source", "NC_CHAR", Citation)
    MetNo_cf$set_attribute("comment", "NC_CHAR", paste("Created on", Sys.time(), "with the Access_KlimaiNorge2100() function from ClimHub version", packageVersion("ClimHub")))
    MetNo_cf$set_attribute("provenance", "NC_CHAR", callargs)

    ### write file
    if (writeFile) {
        ## writing itself
        MetNo_cf$save(fileName) # this loses the extra attributes set just above?!
        MetNo_cf <- open_ncdf(fileName)
    }

    ### return object
    return(MetNo_cf)
}
