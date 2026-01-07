#' @title Visualise spatraster data and overlay sf polygons/points if desired.
#'
#' @description Uses the ggplot2 engine to easily create visualisations of spatraster data and overlay sf polygon or point data if desired.
#'
#' @param spatRaster A spatRaster object to visualise.
#' @param colour Colour palette for spatRaster visualisation.
#' @param legendTitle Optional. Character vector of title for colour legend. By default, the content of the terra::longnames() and terra::units() fields of the supplied spatRaster object.
#' @param panelLabels Optional. Character vector of labels to apply to each layer of the SpatRast. By default, the content of the terra::time() field of the supplied spatRaster object.
#' @param panelColumns Number of columns for panel arrangement of plots (each layer of the supplied spatRaster is visualised as an individual panel).
#' @param sfObj Optional. sf object which to overlay.
#' @param sfSize Optional. Size of sf overlay.
#' @param sfShape Optional. Shape of sf overlay if points.
#' @param sfColour Optional. Colour of sf overlay.
#' @param sfFill Optional. Fill colour of sf overlay.
#'
#' @importFrom viridis inferno
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 scale_fill_gradientn
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 geom_sf
#' @importFrom tidyterra geom_spatraster
#' @importFrom terra time
#' @importFrom terra longnames
#' @importFrom terra units
#'
#' @return A ggplot2 object visualising a raster.
#'
#' @author Erik Kusch
#'
#' @examples
#' ## base plotting
#' spatRaster <- terra::rast(system.file("extdata", "Sognefjord_TN.nc", package = "ClimHub"))[[1:2]]
#' Viz_SpatRast(spatRaster = spatRaster)
#'
#' ## changing colour and layout
#' p <- Viz_SpatRast(spatRaster = spatRaster, colour = viridis::viridis(1e3), panelColumns = 2)
#' p + ggplot2::theme(legend.position = "bottom")
#'
#' ## assigning custom panel and legend labels
#' p <- Viz_SpatRast(spatRaster = spatRaster, legendTitle = "Air Temperature [K]", panelLabels = c("First Panel", "Second Panel"), panelColumns = 2)
#' p + ggplot2::theme(legend.position = "bottom")
#'
#' ## adding an sf overlay
#' data("Jotunheimen_sf")
#' sfObj <- Jotunheimen_sf
#' Viz_SpatRast(spatRaster = spatRaster, sfObj = sfObj)
#'
#' ## larger area with more customisation
#' spatRaster <- terra::rast(system.file("extdata", "KiN_rast.nc", package = "ClimHub"))[[1]] / 10 # needs to be divided by 10 to get actual Kelvin data
#' Viz_SpatRast(spatRaster = spatRaster, legendTitle = "Mean Air Temperature [K]", sfObj = Jotunheimen_sf, sfColour = "white", sfSize = 0.5) +
#'     ggplot2::theme_dark() +
#'     ggplot2::theme(
#'         legend.position = "top",
#'         legend.key.width = unit(1.5, "cm"),
#'         legend.key.height = unit(1, "cm")
#'     ) +
#'     ggplot2::guides(
#'         fill =
#'             ggplot2::guide_colorbar(
#'                 title.position = "top",
#'                 title.hjust = 0.5
#'             )
#'     )
#' @export
Viz_SpatRast <- function(
    spatRaster,
    colour = viridis::inferno(100),
    legendTitle,
    panelLabels,
    panelColumns = 1,
    sfObj,
    sfSize = 1,
    sfShape = 1,
    sfColour = "black",
    sfFill = "NA") {
    ## inferring labels where needed
    if (missing(panelLabels)) {
        panelLabels <- as.character(terra::time(spatRaster))
    }
    names(spatRaster) <- panelLabels
    if (missing(legendTitle)) {
        legendTitle <- paste0(terra::longnames(spatRaster), " [", unique(terra::units(spatRaster)), "]")
    }

    ## make the base plot
    p <- ggplot2::ggplot() +
        tidyterra::geom_spatraster(data = spatRaster) +
        ggplot2::facet_wrap(~ factor(lyr, levels = panelLabels), ncol = panelColumns) +
        ggplot2::scale_fill_gradientn(name = legendTitle, colours = colour, na.value = "transparent") + # add colour and legend
        ggplot2::theme_bw() +
        ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")) + # reduce margins (for fusing of plots)
        ggplot2::theme(legend.key.size = ggplot2::unit(1.5, "cm"))
    ## adding sf
    if (!missing(sfObj)) { # if a shape has been designated
        p <- p +
            ggplot2::geom_sf(data = sfObj, colour = sfColour, fill = sfFill, size = sfSize, shape = sfShape) # add shape
    }
    return(p)
}
