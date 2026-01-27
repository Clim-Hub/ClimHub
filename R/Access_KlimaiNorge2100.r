#' @title Download Klima i Norge 2100 data from the Norwegian Meteorological Institute
#'
#' @description Downloads and processes data from the \href{https://www.met.no/nyhetsarkiv/framtidens-klima-i-norge-flere-oversvommelser-mer-torke-og-mindre-sno/_/attachment/inline/cc1ae8f3-277d-4077-adb9-313e5e28b947:b8073784517edb9392f78c9d82bc5b296db2fc3c/Klima%20i%20Norge%20digital%20low.pdf}{Klima_i_Norge_2100} data product hosted through \href{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge/utgave2025/catalog.html}{thredds.met.no}.
#' Specifically, this function provides access to the following datasets:
#'  1. Gridded 1 x 1 km climate and hydrological projections for Norway data at daily scales contained within \href{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries/catalog.html}{DailyTimeSeries}.
#'
#' \textbf{Note: not all combinations of variables, models, scenarios, and bias-correction methods are available. If you encounter an error, please first consult whether data is provided for the requested combination at \url{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries/catalog.html}. The function is designed to give you informative errors when this happens and you should find the offending URL in your console.}
#'
#' @param variable Character. An overview of Klima i Norge variables can be obtained with `Discovery_Variables(dataSet = "KlimaiNorge2100")`.
#' @param dateStart Character. "YYYY-MM-DD" date at which to start time series of downloaded data. Data is available at daily resolution starting from `1971-01-01`.
#' @param dateStop Character. "YYYY-MM-DD" date at which to stop time series of downloaded data, inclusive of this date. Data is available at daily resolution up to `2100-12-31`.
#' @param extent Optional. The extent to subset the data to, in coordinates of the projection or decimal degrees of longitude and latitude. A numeric vector of length 4 with values minimum and maximum X/longitude and minimum and maximum Y/latitude, in that order. Valid ranges for projected X and Y coordinates are `(-74500, 1119500, 6450500, 7999500)`, for longitude/latitude values `(-1.309, 32.515, 57.765, 72.095)`. Defaults to `NULL`, returning full spatial range of data.
#' @param method Character. An overview of bias-correction methods from which data can be obtained can be obtained with `Discovery_QuickFacts("KlimaiNorge2100")$methods`.
#' @param model Character. An overview of climate models from which data can be obtained can be obtained with `Discovery_QuickFacts("KlimaiNorge2100")$models`.
#' @param scenario Character. An overview of climate models from which data can be obtained can be obtained with `Discovery_QuickFacts("KlimaiNorge2100")$scenarios`. Note that this choice only affects data post-dating 2020-12-31.
#' @param fileName Character, optional. A file name for the produced file, including path. If `NULL` or missing, a virtual data set will be returned from this function.
#' @param compression Optional, integer. Compression level between 1 to 9 applied to final netCDF file. Defaults to NA (no compression applied). Currently not used due to ncdfCF saving scheme.
#' @return A `CFDataset` object which contains the downloaded data and relevant metadata attributes. If specified, also writes a netCDF file to disk.
#'
#' @author Erik Kusch, Patrick Van Laake
#'
#' @examples
#' \dontrun{
#' Access_KlimaiNorge2100(
#'     variable = "mean_air_temperature",
#'     dateStart = "2019-08-01",
#'     dateStop = "2022-09-17",
#'     extent = c(0, 10, 60, 65),
#'     method = "eqm",
#'     model = "noresm-r1i1p1-remo",
#'     scenario = "rcp45",
#'     fileName = "KlimaiNorge2100.nc",
#'     compression = 9
#' )
#' }
#' @export
Access_KlimaiNorge2100 <- function(
    variable, # which variable
    dateStart, dateStop, # time-window
    extent = NULL,
    method,
    model,
    scenario,
    fileName, compression = NA # file storing
    ) {
    ## Input Checks ============
    message("###### Checking Request Validity")

    ### fileName
    if (missing(fileName)) {
        fileName <- NULL
    }
    if (!is.null(fileName)) {
        fileName <- normalizePath(fileName, mustWork = FALSE)
    }

    ### time-window exceeded, we do this in UTC to avoid daylight savings shenanigans
    Start <- as.POSIXct(paste0(dateStart, "T00:00:00"), tz = "UTC")
    Stop <- as.POSIXct(paste0(dateStop, "T00:00:00"), tz = "UTC")

    ### actual checks
    QuickFacts_ls <- Discovery_QuickFacts("KlimaiNorge2100")
    InCheck_ls <- list(
        Variable = list(
            Input = variable,
            Allowed = Discovery_Variables("KlimaiNorge2100")$name,
            Operator = "in"
        ),
        Time = list(
            Input = c(Start, Stop),
            Allowed = QuickFacts_ls$time$extent,
            Operator = "exceeds"
        ),
        Methods = list(
            Input = method,
            Allowed = QuickFacts_ls$methods,
            Operator = "in"
        ),
        Models = list(
            Input = model,
            Allowed = QuickFacts_ls$models,
            Operator = "in"
        ),
        Scenarios = list(
            Input = scenario,
            Allowed = QuickFacts_ls$scenarios,
            Operator = "in"
        )
    )

    # Commenting this out so you can also subset on Xc/Yc coordinates
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
    KlimaiNorge2100_df <- Discovery_Variables("KlimaiNorge2100")
    FilePrefix <- KlimaiNorge2100_df$datafile[KlimaiNorge2100_df$name == variable]
    FileString <- KlimaiNorge2100_df$string[KlimaiNorge2100_df$name == variable]

    ## Download preparations =========
    ## temporary files names, we do this in UTC to avoid daylight savings shenanigans
    DateTimes <- seq(
        from = Start,
        to = Stop,
        by = "1 day"
    )
    Datetimes <- unique(format(DateTimes, "%Y"))
    FNames <- paste("TEMP", ifelse(Datetimes <= 2020, "hist", scenario), model, FilePrefix, "daily", Datetimes, "v4.nc", sep = "_")

    ## File Check =========
    if (!is.null(fileName)) {
        FCheck <- Helper_FileCheck(fileName = fileName, loadFun = ncdfCF::open_ncdf, load = TRUE, verbose = TRUE)
        if (!is.null(FCheck)) {
            return(FCheck)
        }
    }

    ## Subsetting parameters =========
    subset <- if (!is.null(extent)) {
        if (all(extent < 1000)) { # Differentiate between lat/lon and Xc/Yc
            list(lon = extent[1:2], lat = extent[3:4])
        } else {
            list(Xc = extent[1:2], Yc = extent[3:4])
        }
    } else {
        list()
    }
    subset <- c(subset, list(time = c(as.character(Start), as.character(Stop + 86400)))) # Stop date inclusive

    ## Download execution =========
    message("###### Data Download")
    URLs <- sapply(FNames, FUN = function(FName) {
        FNameInfo <- unlist(strsplit(FName, split = "_"))
        paste("https://thredds.met.no/thredds/dodsC/KSS/Klima_i_Norge/utgave2025/DailyTimeSeries", FilePrefix, method,
            FNameInfo[2], # this is the scenario now
            model,
            paste0(model, "_", FNameInfo[2], "_eqm-", FileString, "_norway_1km_", FilePrefix, "_daily_", FNameInfo[6], ".nc4"),
            sep = "/"
        )
    })

    MetNo_cf <- Helper_AccessCF(URLs = URLs, variable = FilePrefix, subset = subset)

    ## Exports =================================
    message("###### Data Export & Return")

    ## Metadata
    callargs <- paste0("ClimHub::", paste(deparse(match.call()), collapse = ", "))
    Citation <- paste0("Klima i Norge (DOI:", Discovery_DOI(dataSet = "KlimaiNorge2100"), ") data provided by the The Norwegian Meteorological institute obtained on ", Sys.Date())
    MetNo_cf$set_attribute("source", "NC_CHAR", Citation)
    MetNo_cf$set_attribute("comment", "NC_CHAR", paste("Created on", Sys.time(), "with the Access_KlimaiNorge2100() function from ClimHub version", packageVersion("ClimHub")))
    MetNo_cf$set_attribute("provenance", "NC_CHAR", callargs)

    ### Optionally write file and return
    if (is.null(fileName)) {
        ds <- ncdfCF::create_ncdf()
        ds$add_variable(MetNo_cf)
        ds
    } else {
        MetNo_cf$save(fileName)
    }
}
