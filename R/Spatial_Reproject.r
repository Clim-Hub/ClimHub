#' @title Reprojecting spatial data.
#'
#' @description Reprojecting one spatial data product to match another.
#'
#' @param projFrom Either a SpatRaster or SF object which is to be reprojected.
#' @param projTo Prefably, a character string identifying desired EPSG ID. Can also be a SpatRaster or SF object to whose projection the other opbject is to be reprojected. The latter specifications are less stable.
#' @param rasterResample Logical. If TRUE and both objects are SpatRasters, projFrom is also resampled to match the resolution and origin of ProjTo.
#'
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom terra res
#' @importFrom terra origin
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#'
#' @return A SpatRaster or SF object reprojected to the same CRS as in ProjTo.
#'
#' @author Erik Kusch
#'
#' @examples
#' Data_rast <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1]]
#' ## reprojecting raster
#' data(Jotunheimen_sf)
#' Spatial_Reproject(Data_rast, Jotunheimen_sf)
#' ## reprojecting sf
#' data(Nor2K_sf)
#' Spatial_Reproject(Nor2K_sf, Data_rast)
#' @export
Spatial_Reproject <- function(projFrom, projTo, rasterResample = TRUE) {
    ## classes of arguments
    class_name <- lapply(list(projFrom, projTo), class)
    class_def <- lapply(class_name, getClass)
    package_name <- lapply(class_def, FUN = function(x) {
        x@package
    })

    ## SpatRaster to SpatRaster
    if (package_name[[1]] == "terra" & package_name[[2]] == "terra") {
        if (rasterResample) {
            Reprojected <- terra::project(projFrom, terra::crs(projTo), res = terra::res(projTo)[1], origin = terra::origin(projTo))
        } else {
            Reprojected <- terra::project(projFrom, terra::crs(projTo))
        }
    }

    ## SpatRaster to SF
    if (package_name[[1]] == "terra" & package_name[[2]] == "sf") {
        Reprojected <- terra::project(projFrom, paste0("epsg:", crs_info <- sf::st_crs(projTo)$epsg))
    }

    ## SpatRaster to EPSG
    if (package_name[[1]] == "terra" & package_name[[2]] == "methods") {
        Reprojected <- terra::project(projFrom, paste0("epsg:", projTo))
    }


    ## SF to SpatRaster
    if (package_name[[1]] == "sf" & package_name[[2]] == "terra") {
        Reprojected <- sf::st_transform(projFrom, st_crs(terra::crs(projTo)))
    }

    ## SF to SF
    if (package_name[[1]] == "sf" & package_name[[2]] == "sf") {
        Reprojected <- sf::st_transform(projFrom, sf::st_crs(projTo))
    }

    ## SF to EPSG
    if (package_name[[1]] == "sf" & package_name[[2]] == "methods") {
        Reprojected <- sf::st_transform(projFrom, projTo)
    }

    ## return
    return(Reprojected)
}
