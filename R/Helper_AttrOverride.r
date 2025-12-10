#' @title Overriding of attributes of one SpatRaster with those from another
#'
#' @description Override attributes (time, varnames, longnames, units, and metags) of a SpatRaster with those from another SpatRaster.
#'
#' @param spatRasterFrom A SpatRaster whose attributes (time, varnames, longnames, units, and metags) are to be assigned to another SpatRaster.
#' @param spatRasterTo A SpatRaster whose attributes (time, varnames, longnames, units, and metags) are to be overriden by those from another SpatRaster.
#'
#' @importFrom terra time
#' @importFrom terra varnames
#' @importFrom terra longnames
#' @importFrom terra units
#' @importFrom terra metags
#'
#' @return A spatRaster.
#'
#' @author Erik Kusch
#'
#' @examples
#' ## loading example data
#' TX_rast <- NC_Read(system.file("extdata", "Sognefjord_TX.nc", package = "ClimHub"))[[1]] # a 1x1km spatRaster
#' RR_rast <- NC_Read(system.file("extdata", "Sognefjord_RR.nc", package = "ClimHub"))[[1]] # a 1x1km spatRaster
#' Helper_AttrOverride(spatRasterFrom = TX_rast, spatRasterTo = RR_rast)
Helper_AttrOverride <- function(spatRasterFrom, spatRasterTo){
        terra::time(spatRasterTo) <- terra::time(spatRasterFrom)
        terra::varnames(spatRasterTo) <- terra::varnames(spatRasterFrom)
        terra::longnames(spatRasterTo) <- terra::longnames(spatRasterFrom)
        terra::units(spatRasterTo) <- terra::units(spatRasterFrom)
        if(!is.null(terra::metags(spatRasterFrom))){
            terra::metags(spatRasterTo) <- terra::metags(spatRasterFrom)
        }
        return(spatRasterTo)
    }
