### DIRECT DOWNLOAD CALLS ========================================================
#' Execute direct download calls
#'
#' Loops over all supplied file urls and file names, assuming these query NetCDF file from open hosts and downloads these files. In the process, it checks whether files that have been downloaded can be loaded from the disk. Parallelisation can speed this process up a lot.
#'
#' @param URLS Character. Vector of URLs for download.
#' @param Names Character. Vector of names for downloaded files.
#' @param Cores Integer. Number of cores for parallelisation if desired.
#' @param Dir Character. Path to directory where files should be stored
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom utils download.file
#' @importFrom httr HEAD
#' @importFrom httr headers
#'
#' @return A vector of filenames. Same as the Names argument.
#'
#' @examples
#'Helper.DirectDownload(
#'    URLS = c(
#'        "https://thredds.met.no/thredds/fileServer/nora3/1961/08/01/00/fc1961080100_003_sfx.nc",
#'        "https://thredds.met.no/thredds/fileServer/nora3/1961/08/01/06/fc1961080106_003_sfx.nc"
#'    ),
#'    Names = c("TEMP_fc1961080100_003_sfx.nc", "TEMP_fc1961080106_003_sfx.nc"),
#'    Cores = 2,
#'    Dir = getwd()
#')
Helper.DirectDownload <- function(URLS, Names, Cores, Dir, verbose = TRUE) {
    ## progress bar
    pb <- Helper.Progress(IterLength = length(Names), Text = "Downloading")

    ## iteration code
    looptext <- '
        URL <- URLS[Iter]
        Name <- Names[Iter]
        ## get expected file size
        response <- httr::HEAD(URL)
        fsize_expected <- as.numeric(headers(response)$`content-length`)
        fsize_is <- file.size(file.path(Dir, Name))
        while (fsize_is != fsize_expected | is.na(fsize_is)) {
            ## unlink downloaded file, useful on reruns of while loop
            unlink(file.path(Dir, Name))
            ## donwload data
            download.file(
                url = URL,
                destfile = file.path(Dir, Name),
                method = "wget",
                quiet = TRUE
            )
            ## get size of downloaded file
            fsize_is <- file.size(file.path(Dir, Name))
        }
        if (exists("Ret_rast")) {
            rm(Ret_rast)
        } # remove the raster object so it does not interfere with future iterations
        Sys.sleep(0.05)
        file.path(Dir, Name)'
        
    ## iterations
    Downls <- Helper.EvalLoopText(LoopText = looptext, Iters = 1:length(Names), Packages = c("httr"), Objects = list(URLS = URLS, Names = Names, Dir = Dir, pb = pb), Cores = Cores, verbose = verbose)

    ## return file names
    return(unlist(Downls))
}
