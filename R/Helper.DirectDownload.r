### DIRECT DOWNLOAD CALLS ========================================================
#' Execute direct download calls
#'
#' Loops over all supplied file urls and file names, assuming these query NetCDF file from open hosts and downloads these files. In the process, it checks whether files that have been downloaded can be loaded from the disk. Parallelisation can speed this process up a lot.
#'
#' @param URLS Character. Vector of URLs for download.
#' @param Names Character. Vector of names for downloaded files.
#' @param Cores Integer. Number of cores for parallelisation if desired.
#' @param Dir Character. Path to directory where files should be stored
#'
#' @importFrom utils download.file
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom snow stopCluster
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom progress progress_bar
#'
#' @return A vector of filenames. Same as the Names argument.
#'
Helper.DirectDownload <- function(URLS, Names, Cores, Dir) {
    ## progress bar
    pb <- progress_bar$new(
        format = "Downloading (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
        total = length(Names), # 100
        width = getOption("width"),
        clear = FALSE
    )
    progressIter <- 1:length(Names) # token reported in progress bar

    ## cluster opening
    if (Cores > 1) {
        cl <- makeCluster(Cores)
        on.exit(snow::stopCluster(cl))
        doSNOW::registerDoSNOW(cl)
        progress <- function(n) {
            pb$tick(tokens = list(layer = progressIter[n]))
        }
        ForeachObjects <- c("Dir", "Names", "URLS")
    }

    ## iteration code
    looptext <- '
        URL <- URLS[DownIter]
        Name <- Names[DownIter]
        suppressWarnings(
            Ret_rast <- tryCatch(
                terra::rast(file.path(Dir, Name)),
                error = function(e) e
            )
        )
        while (class(Ret_rast)[1] == "simpleError") {
            ## donwload data
            download.file(
                url = URL,
                destfile = file.path(Dir, Name),
                method = "wget",
                quiet = TRUE
            )
            ## try to load raster from file, if error returns simpleError
            Ret_rast <- tryCatch(
                terra::rast(file.path(Dir, Name)),
                error = function(e) e
            )
            ## if loading file leads to error, remove file and try again
            if (class(Ret_rast)[1] == "simpleError") {
                unlink(file.path(Dir, Name))
            }
        }
        rm(Ret_rast) # remove the raster object so it does not interfere with future iterations
        Sys.sleep(0.05)
        file.path(Dir, Name)'

    ## Make downloads
    if (Cores > 1) {
        Downls <- foreach(
            DownIter = 1:length(Names),
            .packages = c("terra"),
            .export = ForeachObjects,
            .options.snow = list(progress = progress)
        ) %dopar% { # parallel loop'
            eval(parse(text = looptext))
        } # end of parallel loop
    } else {
        Downls <- c()
        for (DownIter in 1:length(Names)) {
            Fret <- eval(parse(text = looptext)) # evaluate the kriging specification per layer
            Downls <- c(Downls, Fret)
            pb$tick(tokens = list(layer = progressIter[DownIter]))
        }
    }

    ## return file names
    return(unlist(Downls))
}
