#' @title Summarise Cell Values of SpatRaster by Cell Polygon Structure of Another
#'
#' @description Convert to cell structure of one SpatRaster into polygons (a SpatVector) and extract values for these from another SpatRaster resulting in a SpatRaster with the cell structure of the former but values from the latter.
#'
#' @param spatRasterFrom A SpatRaster whose cells are to be summarised.
#' @param spatRasterTo A SpatRaster to whose cell boundaries/structure values in cells of spatRasterFrom should be summarised.
#' @param fun User-defined function by which to summarise values of cells. Supported functions are mean, sum, min, max and table.
#' @param ... additional arguments passed to `fun`, such as na.rm=TRUE
#'
#' @importFrom terra crs
#' @importFrom terra as.polygons
#' @importFrom terra extract
#' @importFrom terra values
#'
#' @return A spatRaster.
#'
#' @author Erik Kusch
#'
#' @examples
#' ## loading example data and making a coarse raster
#' Fine_rast <- NC_Read(system.file("extdata", "Sognefjord_TX.nc", package = "ClimHub"))[[1]] # a 1x1km spatRaster
#' # Viz_SpatRast(Fine_rast)
#' Coarse_rast <- terra::aggregate(Fine_rast, fact = 3)
#' # Viz_SpatRast(Coarse_rast)
#' ## aggregation
#' Aggr_rast <- Helper_PolySummary(spatRasterFrom = Fine_rast, spatRasterTo = Coarse_rast, na.rm = TRUE)
#' # Viz_SpatRast(Aggr_rast)
#' ## disaggregation
#' Disagg_rast <- Helper_PolySummary(spatRasterFrom = Coarse_rast, spatRasterTo = Fine_rast, na.rm = TRUE)
#' # Viz_SpatRast(Disagg_rast)
Helper_PolySummary <- function(spatRasterFrom, spatRasterTo, fun = "mean", ...){
    ## reproject if necessary
    if (terra::crs(spatRasterFrom) != terra::crs(spatRasterTo)) {
        spatRasterFrom <- Spatial_Reproject(spatRasterFrom, spatRasterTo, rasterResample = FALSE)
        warning("Had to reproject your spatRasterFrom to align with the spatRasterTo CRS. This was done automatically, but I recommend doing so yourself and investigating the reprojected objects.")
    }
    ## extraction
    polys <- terra::as.polygons(spatRasterTo[[1]], dissolve = FALSE, na.rm = FALSE) # make cell borders of spatRasterTo into polygons (a spatVector)
    aggrVals <- terra::extract(spatRasterFrom, polys, fun = fun, ...) # extract data by spatRasterTo cell polygons
    aggrVals <- aggrVals[, -1] # remove first column since it only contains IDs
    if (class(aggrVals) == "numeric") {
        aggrVals <- data.frame(aggrVals)
    } # ensure its a dataframe
    ## reassignment and return
    return_rast <- rast(spatRasterTo, nlyrs = ncol(aggrVals)) # create spatRaster to assign values to
    terra::values(return_rast) <- aggrVals # assign values
    return(return_rast)
}
