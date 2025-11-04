#' @title Carry out decumulation of temporally cumulatively stored data
#'
#' @description Takes a SpatRaster and user-specifications of temporal cumulation and reverses the cumulative measurements into indvidual ones. Currently conceptualised to decumulate hourly CDS records.
#'
#' @param spatRaster A SpatRaster within which coverage should be identified
#' @param interval Number of layers in the spatRaster belonging to the same interval of cumulation.
#' @param cumulMode Character. cumulMode of cumulative storage of values. Currently supported: CDS (first layer per day is the cumulative sum of the previous day).
#' @param cores Integer. Number of cores for parallelisation if desired.
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom terra as.data.frame
#' @importFrom terra nlyr
#' @importFrom terra values
#'
#' @return A SpatRaster
#' 
#' @author Erik Kusch
#' 
#' @examples
#' ## Loading some data
#' Data_rast <- terra::rast(system.file("extdata", "KiN_Rast.nc", package = "ClimHub"))
#' ## cropping to a smaller area for the example
#' Data_rast <- terra::crop(Data_rast, c(0, 7e4, 6.7e6, 6.77e6))
#' ## creating a decumulation example dataset
#' Decumul_rast <- c(Data_rast, Data_rast, Data_rast, Data_rast,Data_rast[[1:8]])
#' ## decumulation examples
#' # single-core
#' SingleCore <- Temporal_Decumulation(
#'     spatRaster = Decumul_rast,
#'     interval = 24,
#'     cumulMode = "CDS"
#' )
#' # multi-core
#' MultiCore <- Temporal_Decumulation(
#'     spatRaster = Decumul_rast,
#'     interval = 24,
#'     cumulMode = "CDS",
#'     cores = 4
#' )
#' ## both are the same
#' all.equal(SingleCore, MultiCore)
#' @export
Temporal_Decumulation <- function(spatRaster, interval, cumulMode, cores = 1, verbose = TRUE) {
    ## Make spatRaster into data.frame
    ### progress bar
    pb <- Helper_Progress(iterLength = nlyr(spatRaster), text = "Turning spatRaster into DataFrame")
    df_ls <- lapply(1:nlyr(spatRaster), FUN = function(Iter) {
        df <- as.data.frame(spatRaster[[Iter]], na.rm = FALSE)
        if (verbose) {
            pb$tick(tokens = list(layer = Iter))
        }
        df
    })
    df <- do.call(cbind, df_ls)
    rm(df_ls)

    ## Limit data according to cumulation mode
    if (cumulMode == "CDS") {
        df <- df[, c(-1, -(ncol(df) - 22):-ncol(df))] # remove first and last 23 layers
        Out <- spatRaster[[-(nlyr(spatRaster) - 23):-nlyr(spatRaster)]]
    }
    if (cumulMode != "CDS") {
        stop("Only CDS cumulation is currently supported as the cumulMode argument.")
    }

    ## actual decumulation
    ### progress bar
    pb <- Helper_Progress(iterLength = nrow(df), text = "Decumulation by Pixel in spatRaster")

    ### iteration code
    looptext <- "
    row <- as.numeric(df[Iter, ])

    # Split the row into groups of size `interval`
    groups <- base::split(
        x = row,
        f = as.factor(ceiling(seq_along(row) / interval))
    )
    # Decumulate each group
    decumulated_groups <- lapply(groups, function(group) {
        c(group[1], diff(group)) # First value stays as is; subsequent values are differences
    })
    # Combine all groups back into a single row
    list(Iter = unlist(decumulated_groups))
    "

    ### iterations
    Decumulls <- Helper_EvalLoopText(loopText = looptext, iters = 1:nrow(df), objects = list(df = df, interval = interval, pb = pb), cores = cores, verbose = verbose)

    ## Reassing values to output and return it
    message("Assigning Decumulated Data to spatRaster")
    decumulated_df <- do.call(rbind, lapply(Decumulls, unlist))
    terra::values(Out) <- decumulated_df
    return(Out)
}
