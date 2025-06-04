### SPLIT LAYERS OF RASTER BY TIME ========================================================
#' Split raster layers by time component
#'
#' Creates a list object containing layers of input SpatRaster object according to time component and user-defined split.
#'
#' @param Raster A SpatRaster with a time dimension/component.
#' @param TimeSplit Character. Indicating by which division of the time component to split. Supported values are c("Year", "Season", "Month", "YearMonth")
#'
#' @importFrom terra split
#' @importFrom terra time
#'
#' @return A list of SpatRasters.
#'
#' @examples
#' Data_rast <- terra::rast(system.file("extdata", "KiN_AT.nc", package = "ClimHub"))[[1:360]]
#' Helper.TimeSplit(Raster = Data_rast, TimeSplit = "Year")
#' Helper.TimeSplit(Raster = Data_rast, TimeSplit = "Season")
#' Helper.TimeSplit(Raster = Data_rast, TimeSplit = "Month")
#' Helper.TimeSplit(Raster = Data_rast, TimeSplit = "YearMonth")
#'
Helper.TimeSplit <- function(Raster, TimeSplit) {
    ## validate specification
    InCheck_ls <- list(
        TimeSplit = list(
            Input = TimeSplit,
            Allowed = c("Year", "Season", "Month", "YearMonth"),
            Operator = "in"
        ),
        TimeComponent = list(
            Input = class(terra::time(Raster))[1],
            Allowed = "POSIXct",
            Operator = "in"
        )
    )
    Helper.InputChecker(InCheck_ls)

    ## splitting
    if (TimeSplit == "Year") {
        Split <- format(terra::time(Raster), "%Y")
    }
    if (TimeSplit == "Season") {
        MonthsSeason <- c("Winter" = c(12, 1, 2), "Spring" = c(3, 4, 5), "Summer" = c(6, 7, 8), "Autumn" = c(9, 10, 11))
        Matches <- names(MonthsSeason)[match(as.numeric(format(terra::time(Raster), "%m")), MonthsSeason)]
        Split <- substr(Matches, 1, nchar(Matches) - 1)
    }
    if (TimeSplit == "Month") {
        Split <- format(terra::time(Raster), "%m")
    }
    if (TimeSplit == "YearMonth") {
        Split <- format(terra::time(Raster), "%Y-%m")
    }

    ## return
    Return <- terra::split(Raster, f = Split)
    names(Return) <- unique(Split)
    return(Return)
}
