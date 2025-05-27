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
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom snow stopCluster
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom terra nlyr
#'
#' @return A SpatRaster
#' @examples
#' 
#' Data_rast <- rast(system.file("extdata", "KiN_AT.nc", package = "ClimHub"))[[1:360]]
#' Data_rast <- terra::crop(Data_rast, c(0, 7e4, 6.7e6, 6.77e6))
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
    if(verbose){message("Turning Spatial Data into DataFrame")}
    df <- as.data.frame(Raster)

    ## Limit data according to cumulation mode
    if (Mode == "CDS") {
        df <- df[, c(-1, -(ncol(df) - 22):-ncol(df))] # remove first and last 23 layers
        Out <- Raster[[-(nlyr(Raster) - 23):-nlyr(Raster)]]
    }
    if (Mode != "CDS") {
        stop("Only CDS cumulation is currently supported as the Mode argument.")
    }

    ## progress bar
    pb <- Helper.Progress(IterLength = nrow(df), Text = "Decumulation")

    ## cluster opening
    if (Cores > 1) {
        cl <- makeCluster(Cores)
        on.exit(snow::stopCluster(cl))
        doSNOW::registerDoSNOW(cl)
        progress <- function(n) {
            pb$tick(tokens = list(layer = n))
        }
        ForeachObjects <- c("df", "Interval")
    }

    ## iteration code
    looptext <- "
    row <- as.numeric(df[CumulIter, ])

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

    ## Make downloads
    if (Cores > 1) {
        Decumulls <- foreach(
            CumulIter = 1:nrow(df),
            # .packages = c("httr"),
            .export = ForeachObjects,
            .options.snow = list(progress = progress)
        ) %dopar% { # parallel loop'
            eval(parse(text = looptext))
        } # end of parallel loop
    } else {
        Decumulls <- list("NA" = NA)
        for (CumulIter in 1:nrow(df)) {
            Fret <- eval(parse(text = looptext)) # evaluate the kriging specification per layer
            Decumulls <- c(Decumulls, Fret)
            if(verbose){pb$tick(tokens = list(layer = CumulIter))}
        }
        print(class(Decumulls))
        Decumulls <- Decumulls[-1] # get rid of NA slot I started with
    }

    ## Reassing values to output and return it
    message("Reassigning data to Raster")
    decumulated_df <- do.call(rbind, lapply(Decumulls, unlist))
    values(Out) <- decumulated_df
    return(Out)
}