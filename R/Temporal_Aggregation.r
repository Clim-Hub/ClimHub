#' @title Carry out temporal aggregation
#'
#' @description Takes a SpatRaster and user-specifications of temporal aggregation and carries it out.
#'
#' @param spatRaster A SpatRaster within which coverage should be identified
#' @param tResolution Character. User-specified temporal resolution
#' @param tStep Numeric. User-specified time step
#' @param fun User-defined aggregation function
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra time
#' @importFrom terra app
#' @importFrom terra nlyr
#' @importFrom terra varnames
#' @importFrom lubridate tz
#'
#' @return A SpatRaster
#' 
#' @author Erik Kusch
#' 
#' @examples
#' Data_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' Temporal_Aggregration(
#'     spatRaster = Data_rast,
#'     tResolution = "day",
#'     tStep = 2,
#'     fun = mean
#' )
#' @export
Temporal_Aggregration <- function(spatRaster, tResolution, tStep, fun, verbose = TRUE) {
    ## formatting
    Form <- substr(tResolution, 1, 1)
    Form <- ifelse(Form %in% c("h", "y"), toupper(Form), Form)
    LayerFormat <- format(terra::time(spatRaster), paste0("%", Form))
    TZone <- lubridate::tz(terra::time(spatRaster))

    ## identify which layers need fusing to achieve desired resolution
    TimeDiff <- sapply(terra::time(spatRaster), FUN = function(xDate) {
        length(seq(
            from = terra::time(spatRaster)[1],
            to = xDate,
            by = tResolution
        )) - 1
    })
    AggrIndex <- floor(TimeDiff / tStep) + 1

    ## Checks
    ### check if time component is present
    if (is.null(terra::time(spatRaster))) {
        stop("The data you have supplied does not contain a time component. Please investigate or assign this component using the terra::time() functionality.")
    }

    ### Is the specified temporal resolution finer than what is available in the data?
    if (any(AggrIndex > terra::nlyr(spatRaster))) {
        stop("Generation of aggregation indices for the layers in your supplied data led to erroneous records. This is likely caused by you specifying a temporal resolution finer that what the data you supplied contains natively.")
    }

    # ### Can we cleanly aggregate all data?, taken out as it introduced some issues
    # if (!all(table(AggrIndex) == tStep)) {
    #     message(paste0("Your specified temporal resolution (", tStep, "-", tResolution, " intervals) does not align cleanly with the underlying temporal component of the data you have supplied. Temporal.Aggregation will proceed but discard some information from the data you supplied to achieve consistent temporal aggregation intervals."))
    # }
    # KeepIndices <- names(table(AggrIndex))[which(table(AggrIndex) == tStep)]
    # AggrIndex <- AggrIndex[AggrIndex %in% KeepIndices]

    ## Actual Aggregation
    ### make progress bar
    pb <- Helper_Progress(iterLength = length(unique(AggrIndex)), text = "Temporal Aggregation")
    
    ### aggregating data
    Aggregated_rast <- as.list(rep(NA, length(unique(AggrIndex))))
    for (AggrIter in unique(AggrIndex)) {
        Aggregated_rast[[AggrIter]] <- app(
            x = spatRaster[[which(AggrIndex == AggrIter)]],
            fun = fun
        )
        if(verbose){pb$tick(tokens = list(layer = AggrIter))}
    }
    Aggregated_rast <- do.call(c, Aggregated_rast)

    ## assigning meta-information
    ### Time to Layers
    if (tResolution == "year") {
        terra::time(Aggregated_rast) <- as.POSIXct(
            paste0(LayerFormat[which(!duplicated(AggrIndex))], "-01-01"),
            tz = TZone
        )
    }
    if (tResolution == "month") {
        terra::time(Aggregated_rast) <- as.POSIXct(
            paste0(format(terra::time(spatRaster)[which(!duplicated(AggrIndex))], "%Y-%m"), "-01"),
            tz = TZone
        )
    }
    if (tResolution == "day") {
        terra::time(Aggregated_rast) <- as.POSIXct(
            format(terra::time(spatRaster)[which(!duplicated(AggrIndex))], "%Y-%m-%d"),
            tz = TZone
        )
    }
    if (tResolution == "hour") {
        terra::time(Aggregated_rast) <- as.POSIXct(
            terra::time(spatRaster)[which(!duplicated(AggrIndex))],
            tz = TZone
        )
    }

    ## variable naming
    terra::varnames(Aggregated_rast) <- terra::varnames(spatRaster)[1]

    ## return
    return(Aggregated_rast)
}