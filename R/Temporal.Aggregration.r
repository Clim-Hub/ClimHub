### TEMPORAL AGGREGATION =======================================================
#' Carry out temporal aggregation
#'
#' Takes a SpatRaster and user-specifications of temporal aggregation and carries it out.
#'
#' @param Raster A SpatRaster within which coverage should be identified
#' @param TResolution Character. User-specified temporal resolution
#' @param TStep Numeric. User-specified time step
#' @param FUN User-defined aggregation function
#'
#' @importFrom terra time
#' @importFrom terra app
#' @importFrom terra nlyr
#' @importFrom terra varnames
#' @importFrom lubridate tz
#' @importFrom progress progress_bar
#'
#' @return A SpatRaster
#' @examples
#' Data_rast <- rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))
#' Temporal.Aggregration(
#'     Raster = Data_rast,
#'     TResolution = "day",
#'     TStep = 2,
#'     FUN = mean
#' )
#' @export
Temporal.Aggregration <- function(Raster, TResolution, TStep, FUN) {
    ## formatting
    Form <- substr(TResolution, 1, 1)
    Form <- ifelse(Form %in% c("h", "y"), toupper(Form), Form)
    LayerFormat <- format(terra::time(Raster), paste0("%", Form))
    TZone <- lubridate::tz(terra::time(Raster))

    ## identify which layers need fusing to achieve desired resolution
    TimeDiff <- sapply(terra::time(Raster), FUN = function(xDate) {
        length(seq(
            from = terra::time(Raster)[1],
            to = xDate,
            by = TResolution
        )) - 1
    })
    AggrIndex <- floor(TimeDiff / TStep) + 1

    ## Checks
    ### check if time component is present
    if (is.null(terra::time(Raster))) {
        stop("The data you have supplied does not contain a time component. Please investigate or assign this component using the terra::time() functionality.")
    }

    ### Is the specified temporal resolution finer than what is available in the data?
    if (any(AggrIndex > terra::nlyr(Raster))) {
        stop("Generation of aggregation indices for the layers in your supplied data led to erroneous records. This is likely caused by you specifying a temporal resolution finer that what the data you supplied contains natively.")
    }

    ### Can we cleanly aggregate all data?
    if (!all(table(AggrIndex) == TStep)) {
        message(paste0("Your specified temporal resolution (", TStep, "-", TResolution, " intervals) does not align cleanly with the underlying temporal component of the data you have supplied. Temporal.Aggregation will proceed but discard some information from the data you supplied to achieve consistent temporal aggregation intervals."))
    }
    KeepIndices <- names(table(AggrIndex))[which(table(AggrIndex) == TStep)]
    AggrIndex <- AggrIndex[AggrIndex %in% KeepIndices]

    ## Actual Aggregation
    ### make progress bar
    pb <- progress_bar$new(
        format = "Temporal Aggregation (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
        total = length(unique(AggrIndex)),
        width = getOption("width"),
        clear = FALSE
    )
    progressIter <- 1:length(unique(AggrIndex)) # token reported in progress bar

    ### aggregating data
    Aggregated_rast <- as.list(rep(NA, length(unique(AggrIndex))))
    for (AggrIter in unique(AggrIndex)) {
        Aggregated_rast[[AggrIter]] <- app(
            x = Raster[[which(AggrIndex == AggrIter)]],
            fun = FUN
        )
        pb$tick(tokens = list(layer = progressIter[AggrIter]))
    }
    Aggregated_rast <- do.call(c, Aggregated_rast)

    ## assigning meta-information
    ### Time to Layers
    if (TResolution == "year") {
        terra::time(Aggregated_rast) <- as.POSIXct(
            paste0(LayerFormat[which(!duplicated(AggrIndex))], "-01-01"),
            tz = TZone
        )
    }
    if (TResolution == "month") {
        terra::time(Aggregated_rast) <- as.POSIXct(
            paste0(format(terra::time(Raster)[which(!duplicated(AggrIndex))], "%Y-%m"), "-01"),
            tz = TZone
        )
    }
    if (TResolution == "day") {
        terra::time(Aggregated_rast) <- as.POSIXct(
            format(terra::time(Raster)[which(!duplicated(AggrIndex))], "%Y-%m-%d"),
            tz = TZone
        )
    }
    if (TResolution == "hour") {
        terra::time(Aggregated_rast) <- as.POSIXct(
            terra::time(Raster)[which(!duplicated(AggrIndex))],
            tz = TZone
        )
    }

    ## variable naming
    terra::varnames(Aggregated_rast) <- terra::varnames(Raster)

    ## return
    return(Aggregated_rast)
}
