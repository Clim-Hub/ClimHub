### CDS API Credentials ========================================================
#' Register CDS API Credentials
#'
#' Just checks if provided API user and Key have already been added to keychain and adds them if necessary.
#'
#' @param API_User Character. CDS API User
#' @param API_Key Character. CDS API Key
#'
#' @importFrom ecmwfr wf_get_key
#' @importFrom ecmwfr wf_set_key
#'
#' @return No R object. An addition to the keychain if necessary.
#'
#' @seealso \code{\link{Helper.CDS.ExecuteRequests}}.
#'
Helper.CDS.RegisterAPICredentials <- function(API_User, API_Key) {
    if (packageVersion("ecmwfr") < "2.0.0") {
        stop("Please install an ecmwfr version >= 2.0.0")
    } else {
        if (!grepl("@", API_User)) {
            stop("With the adoption of the new CDS (https://cds-beta.climate.copernicus.eu/), API_User must be you E-mail registered with the new CDS.")
        }
        KeyRegisterCheck <- tryCatch(ecmwfr::wf_get_key(user = API_User),
            error = function(e) {
                e
            }
        )
        if (any(class(KeyRegisterCheck) == "simpleError")) {
            ecmwfr::wf_set_key(
                user = API_User,
                key = as.character(API_Key)
            )
        }
    }
}

### QUERY SEPARATING INTO TIME WINDOWS =========================================
#' Creating time windows for CDS queries
#'
#' Make a list holding date ranges for which to make individual CDS queries
#'
#' @param Allowed_seq Sequence supported by queried data set
#' @param Queried_seq Sequence queried by user
#' @param TChunkSize Numeric. Maximum amount of layers to include in each query
#'
#' @importFrom stringr str_pad
#'
#' @return List. A list of dates for individual CDS queries as used by \code{\link{Helper.CDS.MakeRequest}}.
#'
Helper.CDS.MakeRequestWindows <- function(Allowed_seq, Queried_seq, TChunkSize) {
    ## check if data layers are available
    if (sum(sapply(Queried_seq, FUN = function(x) {
        x %in% Queried_seq
    })) != length(Queried_seq)) {
        stop("Some or all of your qeried time-steps are not available from this dataset. You may want to consult the temporal extent and resolution of your chosen data via the Meta.QuickFacts() function. Keep in mind that your specification of the TimeZone argument can alter what data gets queried in UTC format which the QuickFacts output reports.")
    }

    ## make request to nearest day/month/year depending on temporal resolution of underlying data
    Queried_dates_seq <- unique(format(Queried_seq, "%Y-%m-%d"))
    TimeDiff <- diff(head(Queried_seq, 2))
    Ensembles <- 1
    if (TimeDiff == 0) { # we now assume that ensemble memnber data is queried with 10 ensemble members
        TimeDiff <- diff(c(Queried_seq[1], Queried_seq[11]))
        Ensembles <- 10
    }
    TResolution <- units(TimeDiff)
    TStep <- as.numeric(TimeDiff)

    if (TResolution == "hours") { ## alter ChunkSize to target full days we can pull at once
        TChunkSize <- floor(TChunkSize / (24 / TStep)) / Ensembles
    } else {
        stop("Non-hour resolution downloads for CDS not supported yet")
    }

    ## make request windows
    QueryTimeWindows <- split(Queried_dates_seq, ceiling(seq_along(Queried_dates_seq) / TChunkSize))
    QueryTimeWindows <- lapply(QueryTimeWindows, FUN = function(x) {
        Queried_seq[as.Date(Queried_seq) %in% as.Date(x)]
    })

    ## set up names of list for temporary file names
    names(QueryTimeWindows) <- str_pad(names(QueryTimeWindows), width = 5, side = "left", pad = "0")

    return(QueryTimeWindows)
}

### FORMING CDS Requests =======================================================
#' Form CDS Requests
#'
#' Loops over time windows of defined size and creates a list of CDS requests.
#'
#' @param QueryTimeWindows List. List of date ranges created by \code{\link{Helper.CDS.MakeRequestWindows}}.
#' @param QueryParams_ls List. Query parameters to be added to request.
#' @param Dir Directory pointer. Where to store CDS request outcomes.
#'
#'
#' @return List. Each element holding a list object representing a CDS request.
#'
#' @seealso \code{\link{Helper.CDS.MakeRequestWindows}}, \code{\link{Helper.CDS.RegisterAPICredentials}}, \code{\link{Helper.CDS.ExecuteRequests}}.
#'
Helper.CDS.MakeRequests <- function(QueryTimeWindows, QueryParams_ls, Dir = getwd()) {
    # make file names for temporary files
    FNames <- file.path(Dir, paste0("TEMP_", names(QueryTimeWindows), "_CERRA_", QueryParams_ls$variable, ".grib"))
    # remove empty elements from list as those confuse the CDS API
    QueryParams_ls <- QueryParams_ls[which(unlist(lapply(QueryParams_ls, FUN = function(x) {
        unique(x != "")
    })))]
    #' Make list of CDS Requests
    Requests_ls <- lapply(1:length(QueryTimeWindows), FUN = function(requestID) {
        FName <- FNames[requestID]
        c(
            QueryParams_ls,
            list(
                "date" = paste0(
                    head(unique(format(QueryTimeWindows[[requestID]], "%Y-%m-%d")), n = 1),
                    "/",
                    tail(unique(format(QueryTimeWindows[[requestID]], "%Y-%m-%d")), n = 1)
                ),
                "target" = FName
            )
        )
    })
    #' Return list
    return(Requests_ls)
}

### EXECUTING CDS Requests =======================================================
#' Executing CDS Requests
#'
#' Loops over a list of CDS requests and stages, then listens for them sequentially.
#'
#' @param CDSRequests_ls List. Each element holding a list object representing a CDS request. Created by \code{\link{Helper.CDS.MakeRequest}}.
#' @param API_User Character. CDS API User
#' @param API_Key Character. CDS API Key
#'
#' @importFrom ecmwfr wf_delete
#' @importFrom ecmwfr wf_transfer
#' @importFrom ecmwfr wf_request
#' @importFrom terra rast
#'
#' @return Character. A vector of FileNames to load in subsequent scripts.
#'
#' @seealso \code{\link{Helper.CDS.MakeRequest}}
#'
Helper.CDS.ExecuteRequests <- function(CDSRequests_ls, API_User, API_Key) {
    FilestoLoad <- lapply(seq_along(CDSRequests_ls), FUN = function(Iter) {
        Request_ls <- CDSRequests_ls[[Iter]]
        #--- making request execution updates to console
        Iterator <- paste0("[", Iter, "/", length(CDSRequests_ls), "] ")
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
                user = API_User,
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
                                user = API_User,
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
                            user = API_User
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
