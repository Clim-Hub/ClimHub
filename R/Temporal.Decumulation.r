### TEMPORAL DECUMULATION =======================================================
#' Carry out decumulation of cumulatively stored data
#'
#' Takes a SpatRaster and user-specifications of temporal cumulation and reverses the cumulative measurements into indvidual ones. Currently conceptualised to decumulate hourly CDS records.
#'
#' @param Raster A SpatRaster within which coverage should be identified
#' @param Interval Number of layers in the raster belonging to the same interval of cumulation.
#' @param Mode Character. Mode of cumulative storage of values. Currently supported: CDS (first layer per day is the cumulative sum of the previous day).
#' @param Cores Integer. Number of cores for parallelisation if desired.
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra as.data.frame
#' @importFrom terra nlyr
#' @importFrom terra values
#'
#' @return A SpatRaster
#' @examples
#'
#' Data_rast <- terra::rast(system.file("extdata", "KiN_AT.nc", package = "ClimHub"))[[1:360]]
#' Data_rast <- Spatial.CropMask(Data_rast, c(0, 7e4, 6.7e6, 6.77e6))
#' # single-core
#' SingleCore <- Temporal.Decumulation(
#'     Raster = Data_rast,
#'     Interval = 24,
#'     Mode = "CDS"
#' )
#' # multi-core
#' MultiCore <- Temporal.Decumulation(
#'     Raster = Data_rast,
#'     Interval = 24,
#'     Mode = "CDS",
#'     Cores = 2
#' )
#' ## both are the same?
#' all.equal(SingleCore, MultiCore)
#'
#' @export
Temporal.Decumulation <- function(Raster, Interval, Mode, Cores = 1, verbose = TRUE) {
    ## Make Raster into data.frame
    ### progress bar
    pb <- Helper.Progress(IterLength = nlyr(Raster), Text = "Turning Raster into DataFrame")
    df_ls <- lapply(1:nlyr(Raster), FUN = function(Iter) {
        df <- as.data.frame(Raster[[Iter]], na.rm = FALSE)
        if (verbose) {
            pb$tick(tokens = list(layer = Iter))
        }
        df
    })
    df <- do.call(cbind, df_ls)
    rm(df_ls)

    ## Limit data according to cumulation mode
    if (Mode == "CDS") {
        df <- df[, c(-1, -(ncol(df) - 22):-ncol(df))] # remove first and last 23 layers
        Out <- Raster[[-(nlyr(Raster) - 23):-nlyr(Raster)]]
    }
    if (Mode != "CDS") {
        stop("Only CDS cumulation is currently supported as the Mode argument.")
    }

    ## actual decumulation
    ### progress bar
    pb <- Helper.Progress(IterLength = nrow(df), Text = "Decumulation by Pixel in Raster")

    ### iteration code
    looptext <- "
    row <- as.numeric(df[Iter, ])

    # Split the row into groups of size `Interval`
    groups <- base::split(
        x = row,
        f = as.factor(ceiling(seq_along(row) / Interval))
    )
    # Decumulate each group
    decumulated_groups <- lapply(groups, function(group) {
        c(group[1], diff(group)) # First value stays as is; subsequent values are differences
    })
    # Combine all groups back into a single row
    list(Iter = unlist(decumulated_groups))
    "

    ### iterations
    Decumulls <- Helper.EvalLoopText(LoopText = looptext, Iters = 1:nrow(df), Objects = list(df = df, Interval = Interval, pb = pb), Cores = Cores, verbose = verbose)

    ## Reassing values to output and return it
    message("Assigning Decumulated Data to Raster")
    decumulated_df <- do.call(rbind, lapply(Decumulls, unlist))
    terra::values(Out) <- decumulated_df
    return(Out)
}
