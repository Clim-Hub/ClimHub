#' @title Execute direct download calls
#'
#' @description Loops over all supplied file url and file fileName, assuming these query NetCDF file from open hosts and downloads these files. In the process, it checks whether files that have been downloaded can be loaded from the disk. Parallelisation can speed this process up a lot.
#'
#' @param url Character. Vector of URLs for download.
#' @param fileName Character. Vector of fileName for downloaded files.
#' @param cores Integer. Number of cores for parallelisation if desired.
#' @param verbose Logical. If progress should be displayed in the console.
#'
#' @importFrom utils download.file
#' @importFrom httr HEAD
#' @importFrom httr headers
#'
#' @return A vector of filenames. Same as the fileName argument.
#'
#' @author Erik Kusch
#'
#' @examples
#' Helper_DirectDownload(
#'     url = c(
#'         "https://thredds.met.no/thredds/fileServer/nora3/1961/08/01/00/fc1961080100_003_sfx.nc",
#'         "https://thredds.met.no/thredds/fileServer/nora3/1961/08/01/06/fc1961080106_003_sfx.nc"
#'     ),
#'     fileName = c("TEMP_fc1961080100_003_sfx.nc", "TEMP_fc1961080106_003_sfx.nc"),
#'     cores = 2
#' )
Helper_DirectDownload <- function(url, fileName, cores, verbose = TRUE) {
    ## progress bar
    pb <- Helper_Progress(iterLength = length(fileName), text = "Downloading")

    ## iteration code
    looptext <- '
        URL <- url[Iter]
        Name <- fileName[Iter]
        ## get expected file size
        response <- httr::HEAD(URL)
        fsize_expected <- as.numeric(headers(response)$`content-length`)
        if(length(fsize_expected) == 0){
            if(grepl("https://thredds.met.no/thredds/", URL)){
                Add <- "https://status.met.no/"
            }else{
                Add <- URL
            }
            stop(paste0("Could not retrieve size of file which is meant to be downloaded from server. Online file server may not be reachable. You should probably check server functionality by accessing: ", Add, "."))
        }
        fsize_is <- file.size(Name)
        while (fsize_is != fsize_expected | is.na(fsize_is)) {
            ## unlink downloaded file, useful on reruns of while loop
            unlink(Name)
            ## donwload data
            download.file(
                url = URL,
                destfile = Name,
                method = "wget",
                quiet = TRUE
            )
            ## get size of downloaded file
            fsize_is <- file.size(Name)
        }
        if (exists("Ret_rast")) {
            rm(Ret_rast)
        } # remove the raster object so it does not interfere with future iterations
        Sys.sleep(0.05)
        Name'

    ## iterations
    Downls <- Helper_EvalLoopText(loopText = looptext, iters = 1:length(fileName), packages = c("httr"), objects = list(url = url, fileName = fileName, pb = pb), cores = cores, verbose = verbose)

    ## return file fileName
    return(unlist(Downls))
}
