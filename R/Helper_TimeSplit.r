#' @title Split raster layers by time component
#'
#' @description Creates a list object containing layers of input SpatRaster object according to time component and user-defined split.
#'
#' @param spatRaster A SpatRaster with a time dimension/component.
#' @param tResolution Character. Indicating by which division of the time component to split. Supported values are c("Year", "Season", "Month", "YearMonth")
#'
#' @importFrom terra split
#' @importFrom terra time
#'
#' @return A list of SpatRasters.
#' 
#' @author Erik Kusch
#'
#' @examples
#' Data_rast <- terra::rast(system.file("extdata", "KiN_AT.nc", package = "ClimHub"))[[1:360]]
#' Helper_TimeSplit(spatRaster = Data_rast, tResolution = "Year")
#' Helper_TimeSplit(spatRaster = Data_rast, tResolution = "Season")
#' Helper_TimeSplit(spatRaster = Data_rast, tResolution = "Month")
#' Helper_TimeSplit(spatRaster = Data_rast, tResolution = "YearMonth")
Helper_TimeSplit <- function(spatRaster, tResolution) {
    ## validate specification
    InCheck_ls <- list(
        tResolution = list(
            Input = tResolution,
            Allowed = c("Year", "Season", "Month", "YearMonth"),
            Operator = "in"
        ),
        TimeComponent = list(
            Input = class(terra::time(spatRaster))[1],
            Allowed = "POSIXct",
            Operator = "in"
        )
    )
    Helper_InputChecker(inputCheck = InCheck_ls)

    ## splitting
    if (tResolution == "Year") {
        Split <- format(terra::time(spatRaster), "%Y")
    }
    if (tResolution == "Season") {
        MonthsSeason <- c("Winter" = c(12, 1, 2), "Spring" = c(3, 4, 5), "Summer" = c(6, 7, 8), "Autumn" = c(9, 10, 11))
        Matches <- names(MonthsSeason)[match(as.numeric(format(terra::time(spatRaster), "%m")), MonthsSeason)]
        Split <- substr(Matches, 1, nchar(Matches) - 1)
    }
    if (tResolution == "Month") {
        Split <- format(terra::time(spatRaster), "%m")
    }
    if (tResolution == "YearMonth") {
        Split <- format(terra::time(spatRaster), "%Y-%m")
    }

    ## return
    Return <- terra::split(spatRaster, f = Split)
    names(Return) <- unique(Split)
    return(Return)
}
