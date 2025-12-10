#' @title Spatial Aggregation of SpatRaster Data
#'
#' @description Aggregation of spatraster data to either (1) match a pre-established spatraster or (2) coarsen spatial resolution by a specific factor.
#'
#' @param spatRasterFrom A SpatRaster whose cells are to be aggregated.
#' @param spatRasterTo A SpatRaster to whose cell boundaries values in cells of spatRasterFrom should be aggregated.
#' @param fun User-defined function by which to aggregate values of cells (e.g., mean, sd, sum, min, max).
#' @param ... additional arguments passed to `fun`, such as `na.rm=TRUE`
#' @param factor Integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor) or three integers (when also aggregating over layers). Passed on to `terra::aggregate`.
#' @param cores Integer. Number of cores for parallelisation if desired.
#'
#' @importFrom terra res
#' @importFrom terra aggregate
#'
#' @return A spatRaster.
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Spatial_Disaggregate}}
#' 
#' @examples
#' ## loading example data
#' Fine_rast <- NC_Read(system.file("extdata", "Sognefjord_TX.nc", package = "ClimHub"))[[1]] # a 1x1km spatRaster
#' # Viz_SpatRast(Fine_rast)
#' 
#' ## coarsening by a factor of 3
#' Coarse_rast1 <- Spatial_Aggregate(spatRasterFrom = Fine_rast, fun = "mean", factor = 3)
#' # Viz_SpatRast(Coarse_rast1)
#' 
#' ## coarsening by a factor of 5 and extracting SD
#' Coarse_rast2 <- Spatial_Aggregate(spatRasterFrom = Fine_rast, fun = "sd", factor = 5)
#' # Viz_SpatRast(Coarse_rast2)
#' 
#' ## coarsening by a factor of 3, but ignoring NAs
#' Coarse_rast3 <- Spatial_Aggregate(spatRasterFrom = Fine_rast, fun = "mean", na.rm = TRUE, factor = 3)
#' # Viz_SpatRast(Coarse_rast3)
#' 
#' ## coarsening to match existing raster; takes longer but is more versatile
#' Coarse_rast4 <- Spatial_Aggregate(spatRasterFrom = Fine_rast, spatRasterTo = Coarse_rast3, fun = "mean", na.rm = TRUE)
#' # Viz_SpatRast(Coarse_rast4)
#' @export
Spatial_Aggregate <- function(
    spatRasterFrom, spatRasterTo,
    fun = "mean", ...,
    factor, cores = 1) {
    if (!missing(spatRasterTo) & !missing(factor)) {
        stop("Please provide only one of either spatRasterTo or factor in your function specification.")
    }

    ## aggregate by other spatRaster
    if (!missing(spatRasterTo)) {
        ## check if this is even aggregation
        if(any(terra::res(spatRasterFrom) > terra::res(spatRasterTo))){stop("Resolution of your spatraster to be aggregated must be finer than that of the spatraster to whose specifications to aggregate. Currently, you are asking for disaggregation. You may want to either switch the specifications of spatRasterFrom and spatRasterTo to carry out aggregation or use the function Spatial_Disaggregate() for actual disaggregation.")}
        return_rast <- Helper_PolySummary(spatRasterFrom, spatRasterTo, fun, ...)
    }

    ## aggregate by fact
    if (!missing(factor)) {
        return_rast <- terra::aggregate(x = spatRasterFrom, fact = factor, fun = fun, ..., cores = cores)
    }

    ## make metadata congruent with spatRasterFrom and return
    return_rast <- Helper_AttrOverride(spatRasterFrom = spatRasterFrom, spatRasterTo = return_rast)
    
    return(return_rast)
}