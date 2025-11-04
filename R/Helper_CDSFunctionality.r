#' @title Register CDS API Credentials
#' @description Just checks if provided API user and Key have already been added to keychain and adds them if necessary.
#'
#' @param apiUser Optional, character. CDS API User.
#' @param apiKey Optional, character. CDS API Key.
#'
#' @importFrom ecmwfr wf_get_key
#' @importFrom ecmwfr wf_set_key
#'
#' @return No R object. An addition to the keychain if necessary.
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Helper_CDS_ExecuteRequests}}.
Helper_CDS_RegisterAPICredentials <- function(apiUser, apiKey) {
    if (packageVersion("ecmwfr") < "2.0.0") {
        stop("Please install an ecmwfr version >= 2.0.0")
    } else {
        if (!grepl("@", apiUser)) {
            stop("With the adoption of the new CDS (https://cds-beta.climate.copernicus.eu/), apiUser must be you E-mail registered with the new CDS.")
        }
        KeyRegisterCheck <- tryCatch(ecmwfr::wf_get_key(user = apiUser),
            error = function(e) {
                e
            }
        )
        if (any(class(KeyRegisterCheck) == "simpleError")) {
            ecmwfr::wf_set_key(
                user = apiUser,
                key = as.character(apiKey)
            )
        }
    }
}

#' @title Create Time Windows for CDS Queries
#' @description Make a list holding date ranges for which to make individual CDS queries.
#'
#' @param allowedSeq Optional, sequence. Sequence supported by queried data set.
#' @param queriedSeq Optional, sequence. Sequence queried by user.
#' @param tChunkSize Optional, numeric. Maximum amount of layers to include in each query.
#'
#' @importFrom stringr str_pad
#'
#' @return List. A list of dates for individual CDS queries as used by \code{\link{Helper_CDS_MakeRequest}}.
#'
#' @author Erik Kusch
Helper_CDS_MakeRequestWindows <- function(allowedSeq, queriedSeq, tChunkSize) {
    ## check if data layers are available
    if (sum(sapply(queriedSeq, FUN = function(x) {
        x %in% queriedSeq
    })) != length(queriedSeq)) {
        stop("Some or all of your qeried time-steps are not available from this dataset. You may want to consult the temporal extent and resolution of your chosen data via the Meta_QuickFacts() function. Keep in mind that your specification of the TimeZone argument can alter what data gets queried in UTC format which the QuickFacts output reports.")
    }

    ## make request to nearest day/month/year depending on temporal resolution of underlying data
    Queried_dates_seq <- unique(format(queriedSeq, "%Y-%m-%d"))
    TimeDiff <- diff(head(queriedSeq, 2))
    Ensembles <- 1
    if (TimeDiff == 0) { # we now assume that ensemble memnber data is queried with 10 ensemble members
        TimeDiff <- diff(c(queriedSeq[1], queriedSeq[11]))
        Ensembles <- 10
    }
    TResolution <- units(TimeDiff)
    TStep <- as.numeric(TimeDiff)

    if (TResolution == "hours") { ## alter ChunkSize to target full days we can pull at once
        tChunkSize <- floor(tChunkSize / (24 / TStep)) / Ensembles
    } else {
        stop("Non-hour resolution downloads for CDS not supported yet")
    }

    ## make request windows
    QueryTimeWindows <- split(Queried_dates_seq, ceiling(seq_along(Queried_dates_seq) / tChunkSize))
    QueryTimeWindows <- lapply(QueryTimeWindows, FUN = function(x) {
        queriedSeq[as.Date(queriedSeq) %in% as.Date(x)]
    })

    ## set up names of list for temporary file names
    names(QueryTimeWindows) <- str_pad(names(QueryTimeWindows), width = 5, side = "left", pad = "0")

    return(QueryTimeWindows)
}

#' @title Form CDS Requests
#' @description Loops over time windows of defined size and creates a list of CDS requests.
#'
#' @param queryTimeWindows Optional, list. List of date ranges created by \code{\link{Helper_CDS_MakeRequestWindows}}.
#' @param queryParams Optional, list. Query parameters to be added to request.
#' @param dir Optional, character. Where to store CDS request outcomes.
#'
#' @return List. Each element holding a list object representing a CDS request.
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Helper_CDS_MakeRequestWindows}}, \code{\link{Helper_CDS_RegisterAPICredentials}}, \code{\link{Helper_CDS_ExecuteRequests}}.
#'
Helper_CDS_MakeRequests <- function(queryTimeWindows, queryParams, dir = getwd()) {
    # make file names for temporary files
    FNames <- file.path(dir, paste0("TEMP_", names(queryTimeWindows), "_CERRA_", queryParams$variable, ".grib"))
    # remove empty elements from list as those confuse the CDS API
    queryParams <- queryParams[which(unlist(lapply(queryParams, FUN = function(x) {
        unique(x != "")
    })))]
    #' Make list of CDS Requests
    Requests_ls <- lapply(1:length(queryTimeWindows), FUN = function(requestID) {
        FName <- FNames[requestID]
        c(
            queryParams,
            list(
                "date" = paste0(
                    head(unique(format(queryTimeWindows[[requestID]], "%Y-%m-%d")), n = 1),
                    "/",
                    tail(unique(format(queryTimeWindows[[requestID]], "%Y-%m-%d")), n = 1)
                ),
                "target" = FName
            )
        )
    })
    #' Return list
    return(Requests_ls)
}

#' @title Execute CDS Requests
#' @description Loops over a list of CDS requests and stages, then listens for them sequentially.
#'
#' @param cdsRequests Optional, list. Each element holding a list object representing a CDS request. Created by \code{\link{Helper_CDS_MakeRequest}}.
#' @param apiUser Optional, character. CDS API User.
#' @param apiKey Optional, character. CDS API Key.
#'
#' @importFrom ecmwfr wf_delete
#' @importFrom ecmwfr wf_transfer
#' @importFrom ecmwfr wf_request
#' @importFrom terra rast
#'
#' @return Character. A vector of FileNames to load in subsequent scripts.
#'
#' @author Erik Kusch
#'
#' @seealso \code{\link{Helper_CDS_MakeRequest}}
#'
Helper_CDS_ExecuteRequests <- function(cdsRequests, apiUser, apiKey) {
    FilestoLoad <- lapply(seq_along(cdsRequests), FUN = function(Iter) {
        Request_ls <- cdsRequests[[Iter]]
        #--- making request execution updates to console
        Iterator <- paste0("[", Iter, "/", length(cdsRequests), "] ")
        FName <- Request_ls$target
        Dates <- gsub(pattern = "/", replacement = " - ", Request_ls$date)
        message(paste0(Iterator, basename(FName), " (UTC: ", Dates, ")"))
        #--- check if file is already present
        if (file.exists(FName)) {
            print("File already present on your disk. Skipping request staging.")
        } else { #--- Stage and listen for request
            cat("\r", sprintf("%-100s", "## Staging CDS request")) # overwrite entire line
            flush.console()
            Sys.sleep(1)
            # print("## Staging CDS request")
            API_request <- ecmwfr::wf_request(
                user = apiUser,
                request = Request_ls,
                transfer = FALSE,
                path = dirname(FName),
                verbose = FALSE
            )
            cat("\r", sprintf("%-100s", "## CDS request staged successfully")) # overwrite entire line
            flush.console()
            Sys.sleep(1)

            cat("\r", sprintf("%-100s", "## Resolving CDS request")) # overwrite entire line
            flush.console()
            FileDown <- list(state = "accepted")
            API_request <- API_request$update_status(verbose = FALSE)
            while (FileDown$state != "successful") {
                cat("\r", strrep(" ", 100), "\r")
                flush.console()
                ## console output that shows the status of the request on CDS
                if (FileDown$state == "accepted") {
                    for (rep_iter in 1:10) {
                        message_txt <- paste0("Waiting for CDS to start processing the request.", strrep(".", rep_iter))
                        cat("\r", sprintf("%-100s", message_txt)) # overwrite entire line
                        flush.console()
                        Sys.sleep(0.1)
                    }
                }
                if (FileDown$state == "running") {
                    for (rep_iter in 1:10) {
                        message_txt <- paste0("CDS is processing the request.", strrep(".", rep_iter))
                        cat("\r", sprintf("%-100s", message_txt)) # overwrite entire line
                        flush.console()
                        Sys.sleep(0.1)
                    }
                }

                API_request <- API_request$update_status(verbose = FALSE)
                FileDown$state <- API_request$get_status()

                if (API_request$is_failed()) {
                    stop("Query failed on CDS. Assess issues at https://cds.climate.copernicus.eu/cdsapp#!/yourrequests.")
                }

                if (FileDown$state == "successful") {
                    for (rep_iter in 1:10) {
                        message_txt <- paste0("CDS finished processing the request. Download starting soon.", strrep(".", rep_iter))
                        cat("\r", sprintf("%-100s", message_txt)) # overwrite entire line
                        flush.console()
                        Sys.sleep(0.1)
                    }
                    # Clear the final status line
                    cat("\r", strrep(" ", 100), "\r")
                    flush.console()

                    DownSuccess <- FALSE
                    DownAttempt <- 1
                    while (!DownSuccess) {
                        Download_CDS <- capture.output(
                            ecmwfr::wf_transfer(
                                url = API_request$get_url(),
                                user = apiUser,
                                verbose = TRUE,
                                path = dirname(FName),
                                filename = basename(API_request$get_request()$target)
                            ),
                            type = "message"
                        )
                        rm(Download_CDS)

                        ## check if file can be loaded
                        LoadTry <- tryCatch(rast(FName),
                            error = function(e) {
                                e
                            }
                        )
                        if (class(LoadTry)[1] == "simpleError") {
                            cat("\r", sprintf("%-100s", "Download terminated or finished but did not produce a readable file. Retrying shortly.")) # overwrite entire line
                            flush.console()
                            Sys.sleep(5)
                            unlink(FName)
                            DownAttempt <- DownAttempt + 1
                            if (DownAttempt == 10) {
                                stop("Donwload failed after 10 tries.")
                            }
                        } else {
                            DownSuccess <- TRUE
                            print("File successfully downloaded.")
                        }
                    }

                    ## purge request and check succes of doing so
                    checkdeletion <- capture.output(
                        wf_delete(
                            url = API_request$get_url(),
                            user = apiUser
                        ),
                        type = "message"
                    )
                    rm(checkdeletion)
                }
            }
        }
        return(FName)
    })
    return(FilestoLoad)
}
