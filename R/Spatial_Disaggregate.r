#' @title Spatial Disaggregation of SpatRaster Data
#'
#' @description Disaggregation of spatraster data to either (1) match a pre-established spatraster or (2) coarsen spatial resolution by a specific factor.
#'
#' @param spatRasterFrom A SpatRaster whose cells are to be disaggregated.
#' @param spatRasterTo A SpatRaster to whose cell boundaries values in cells of spatRasterFrom should be disaggregated.
#' @param fun User-defined function by which to disaggregate values of cells. Supported functions are (1) mean, sum, min, max, etc. when specifying `spatRasterTo` and (2) either "near" for nearest or "bilinear" for bilinear interpolation when specufing `factor` (passed on to `terra::disagg` function as `method` argument).
#' @param ... additional arguments passed to `fun`, such as `na.rm=TRUE` when specifying `spatRasterTo`
#' @param factor Integer. Disaggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor) or three integers (when also disaggregating over layers). Passed on to `terra::disagg`.
#'
#' @importFrom terra res
#' @importFrom terra disagg
#'
#' @return A spatRaster.
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Spatial_Aggregate}}
#'
#' @examples
#' ## loading example data
#' Coarse_rast <- NC_Read(system.file("extdata", "Sognefjord_TX.nc", package = "ClimHub"))[[1]] # a 1x1km spatRaster
#' # Viz_SpatRast(Coarse_rast)
#'
#' ## coarsening by a factor of 2
#' Fine_rast1 <- Spatial_Disaggregate(spatRasterFrom = Coarse_rast, fun = "near", factor = 2)
#' # Viz_SpatRast(Fine_rast1)
#'
#' ## coarsening by a factor of 5 and using bilinear method
#' Fine_rast2 <- Spatial_Disaggregate(spatRasterFrom = Coarse_rast, fun = "bilinear", factor = 5)
#' # Viz_SpatRast(Fine_rast2)
#'
#' ## coarsening to match existing raster; takes longer but is more versatile
#' \dontrun{
#' Fine_rast3 <- Spatial_Disaggregate(spatRasterFrom = Coarse_rast, spatRasterTo = Fine_rast1, fun = "mean", na.rm = TRUE)
#' # Viz_SpatRast(Fine_rast3)
#' }
#' @export
Spatial_Disaggregate <- function(
    spatRasterFrom, spatRasterTo,
    fun, ...,
    factor) {
    if (!missing(spatRasterTo) & !missing(factor)) {
        stop("Please provide only one of either spatRasterTo or factor in your function specification.")
    }

    ## aggregate by other spatRaster
    if (!missing(spatRasterTo)) {
        ## check if this is even aggregation
        if (any(terra::res(spatRasterFrom) < terra::res(spatRasterTo))) {
            stop("Resolution of your spatraster to be disaggregated must be coarser than that of the spatraster to whose specifications to disaggregate. Currently, you are asking for aggregation. You may want to either switch the specifications of spatRasterFrom and spatRasterTo to carry out aggregation or use the function Spatial_Aggregate() for actual aggregation.")
        }
        return_rast <- Helper_PolySummary(spatRasterFrom, spatRasterTo, fun, ...)
    }

    ## aggregate by fact
    if (!missing(factor)) {
        return_rast <- terra::disagg(x = spatRasterFrom, fact = factor, method = fun)
    }

    ## make metadata congruent with spatRasterFrom and return
    return_rast <- Helper_AttrOverride(spatRasterFrom = spatRasterFrom, spatRasterTo = return_rast)

    return(return_rast)
}
