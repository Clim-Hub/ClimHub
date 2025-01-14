### REPROJECTING SPATIAL DATA =========================================================
#' Reprojecting spatial data.
#'
#' Reprojecting one spatial data product to match another.
#'
#' @param ProjFrom Either a SpatRaster or SF object which is to be reprojected.
#' @param ProjTo Prefably, a character string identifying desired EPSG ID. Can also be a SpatRaster or SF object to whose projection the other opbject is to be reprojected. The latter specifications are less stable.
#'
#' @importFrom terra crs
#' @importFrom terra project
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#'
#' @return A SpatRaster or SF object reprojected to the same CRS as in ProjTo.
#'
#' @examples
#' Data_rast <- rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1]]
#' ## reprojecting raster
#' data(Jotunheimen_sf)
#' Spatial.Reproject(Data_rast, Jotunheimen_sf)
#' ## reprojecting sf
#' data(Nor2K_sf)
#' Spatial.Reproject(Nor2K_sf, Data_rast)
#' @export
Spatial.Reproject <- function(ProjFrom, ProjTo) {
    ## classes of arguments
    class_name <- lapply(list(ProjFrom, ProjTo), class)
    class_def <- lapply(class_name, getClass)
    package_name <- lapply(class_def, FUN = function(x) {
        x@package
    })

    ## SpatRaster to SpatRaster
    if (package_name[[1]] == "terra" & package_name[[2]] == "terra") {
        Reprojected <- terra::project(ProjFrom, terra::crs(ProjTo))
    }

    ## SpatRaster to SF
    if (package_name[[1]] == "terra" & package_name[[2]] == "sf") {
        Reprojected <- terra::project(ProjFrom, paste0("epsg:", crs_info <- sf::st_crs(ProjTo)$epsg))
    }

    ## SpatRaster to EPSG
    if (package_name[[1]] == "terra" & package_name[[2]] == "methods") {
        Reprojected <- terra::project(ProjFrom, paste0("epsg:", ProjTo))
    }


    ## SF to SpatRaster
    if (package_name[[1]] == "sf" & package_name[[2]] == "terra") {
        Reprojected <- sf::st_transform(ProjFrom, st_crs(terra::crs(ProjTo)))
    }

    ## SF to SF
    if (package_name[[1]] == "sf" & package_name[[2]] == "sf") {
        Reprojected <- sf::st_transform(ProjFrom, sf::st_crs(ProjTo))
    }

    ## SF to EPSG
    if (package_name[[1]] == "sf" & package_name[[2]] == "methods") {
        Reprojected <- sf::st_transform(ProjFrom, ProjTo)
    }

    ## return
    return(Reprojected)
}
