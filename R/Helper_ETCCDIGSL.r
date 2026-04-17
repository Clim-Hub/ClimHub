#' @title Calculate Growing Season Length (GSL)
#'
#' @description Calculates Growing Season Length as defined by ETCCDI with different treatment of northern and southern hemisphere. For the northern hemisphere, GSL is defined as the number of days between the first occurrence of 6 consecutive days with mean temperature above 5°C after Jan 1st and the first occurrence of 6 consecutive days with mean temperature below 5°C after Jul 1st. For the southern hemisphere, GSL is defined as the number of days between the first occurrence of 6 consecutive days with mean temperature above 5°C after Jul 1st and the first occurrence of 6 consecutive days with mean temperature below 5°C after Jan 1st of the following year. If no valid start or end date is found, GSL is set to NA for that year and grid cell.
#'
#' @param TM A CFVariable containing daily mean temperatures (or similar) to be used for GSL calculation.
#'
#' @importFrom ncdfCF as_CF
#'
#' @return A CFVariable.
#'
#' @author Erik Kusch
#'
#' @examples
#' \dontrun{
#' TX <- NC_Read("inst/extdata/KiN_tasmax_2050.nc")[["tasmax"]]
#' TN <- NC_Read("inst/extdata/KiN_tasmin_2050.nc")[["tasmin"]]
#' TM <- (TN + TX) / 2
#' GSL <- Helper_ETCCDIGSL(TM)
#' }
Helper_ETCCDIGSL <- function(TM) {
    threshold <- 273.15 + 5
    TM_array <- TM$raw()
    dn <- dimnames(TM_array)
    if (is.null(dn) || is.null(dn$lat)) {
        stop("Array must have dimnames with a 'lat' entry.")
    }
    lat <- as.numeric(dn$lat)
    if (anyNA(lat)) {
        stop("Latitude dimnames could not be converted to numeric.")
    }

    north_idx <- which(lat >= 0)
    south_idx <- which(lat < 0)
    GSL_ls <- list(
        north = TM_array[, north_idx, , drop = FALSE],
        south = TM_array[, south_idx, , drop = FALSE]
    )

    first_run_start <- function(x, idx, conditionFun, runLength = 6) {
        if (length(idx) < runLength) {
            return(NA_integer_)
        }
        v <- conditionFun(x[idx])
        v[is.na(v)] <- FALSE
        r <- rle(v)

        hit <- which(r$values & r$lengths >= runLength)
        if (length(hit) == 0) {
            return(NA_integer_)
        }

        relStart <- if (hit[1] == 1) 1L else (sum(r$lengths[seq_len(hit[1] - 1)]) + 1L)
        idx[relStart]
    }


    GSL_calcs <- lapply(names(GSL_ls), function(hemi) {
        arr <- GSL_ls[[hemi]]
        # Skip if no latitude cells
        if (is.null(arr) || length(dim(arr)) != 3 || dim(arr)[2] == 0) {
            return(NULL)
        }

        ## figure out dates and years
        dn <- dimnames(arr)
        if (is.null(dn) || is.null(dn$time)) stop("Time dimnames are required.")
        dates <- as.Date(dn$time)
        yrs <- sort(unique(as.integer(format(dates, "%Y"))))

        ## makke arrays for filling with information about start/end dates and GSL days
        nlon <- dim(arr)[which(names(dn) == "lon")]
        nlat <- dim(arr)[which(names(dn) == "lat")]
        ny <- length(yrs)
        startDate <- array(as.Date(NA),
            dim = c(nlon, nlat, ny),
            dimnames = list(lon = dn$lon, lat = dn$lat, time = as.character(yrs))
        )
        endDate <- array(as.Date(NA),
            dim = c(nlon, nlat, ny),
            dimnames = list(lon = dn$lon, lat = dn$lat, time = as.character(yrs))
        )
        gslDays <- array(NA_real_,
            dim = c(nlon, nlat, ny),
            dimnames = list(lon = dn$lon, lat = dn$lat, time = as.character(yrs))
        )

        ## iterate over years and grid cells to find start/end dates and GSL days
        for (yIdx in seq_along(yrs)) {
            y <- yrs[yIdx]

            ## define windows in which to look for runs
            if (hemi == "north") {
                # Start window (north): Jan 1 -> Dec 31 of year y
                startWin <- which(dates >= as.Date(sprintf("%d-01-01", y)) &
                    dates <= as.Date(sprintf("%d-12-31", y)))

                # End window (north): Jul 1 -> Dec 31 of year y
                endWin <- which(dates >= as.Date(sprintf("%d-07-01", y)) &
                    dates <= as.Date(sprintf("%d-12-31", y)))

                # Fallback if no 6-day <= threshold run
                endFallback <- as.Date(sprintf("%d-12-31", y))
            } else {
                # Start window (south): Jul 1 -> Dec 31 of year y
                startWin <- which(dates >= as.Date(sprintf("%d-07-01", y)) &
                    dates <= as.Date(sprintf("%d-12-31", y)))

                # End window (south): Jan 1 -> Jun 30 of year y+1
                # (June 31 is not a valid date, so Jun 30 is used)
                yNext <- y + 1L
                hasNextYear <- any(as.integer(format(dates, "%Y")) == yNext)

                if (hasNextYear) {
                    endWin <- which(dates >= as.Date(sprintf("%d-01-01", yNext)) &
                        dates <= as.Date(sprintf("%d-06-30", yNext)))
                    endFallback <- as.Date(sprintf("%d-06-30", yNext))
                } else {
                    endWin <- integer(0)
                    endFallback <- as.Date(NA)
                }
            }

            ## iterate over grid cells to find start/end dates and GSL days
            for (i in seq_len(nlon)) {
                for (j in seq_len(nlat)) {
                    ts <- arr[i, j, ]

                    sIdx <- first_run_start(ts, startWin, function(v) v > threshold)

                    if (is.na(sIdx)) {
                        # no valid season start -> leave NA
                        next
                    }

                    # Find end run; if not found, use fallback date (except missing next year case in south)
                    eIdx <- first_run_start(ts, endWin, function(v) v < threshold)

                    sDate <- dates[sIdx]
                    if (is.na(eIdx)) {
                        eDate <- endFallback
                    } else {
                        eDate <- dates[eIdx]
                    }

                    startDate[i, j, yIdx] <- sDate
                    endDate[i, j, yIdx] <- eDate

                    if (!is.na(eDate)) {
                        gslDays[i, j, yIdx] <- as.numeric(eDate - sDate)
                        # If you want inclusive count, use: as.numeric(eDate - sDate) + 1
                    }
                }
            }
        }

        # list(
        # startDate = startDate,
        # endDate = endDate,
        # gslDays =
        gslDays
        # ,
        # years = yrs
        # )
    })
    names(GSL_calcs) <- names(GSL_ls)

    ## fusing north and south-hemisphere arrays
    gsl_array <- abind::abind(GSL_calcs$south, GSL_calcs$north, along = 2)

    ## transforming back to CFVariable
    names(dimnames(gsl_array)) <- names(dimnames(TM_array))
    dimnames(gsl_array)[1:2] <- dimnames(TM_array)[1:2]
    dimnames(gsl_array)$time <- paste0(dimnames(gsl_array)$time, "-07-02T12:00:00")
    gsl_array_tll <- aperm(gsl_array, c(3, 1, 2))

    # optional: fix dimname order explicitly
    dn <- dimnames(gsl_array)
    dimnames(gsl_array_tll) <- list(
        time = dn$time,
        lon  = dn$lon,
        lat  = dn$lat
    )
    GSL <- as_CF("GSL", gsl_array_tll)

    ## report back
    return(GSL)
}
