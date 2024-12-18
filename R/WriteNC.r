### NETCDF WRITING WITH PROGRESS BAR ========================================
#' Write NetCDF files and their metadata with a progress bar for netcdf layers
#'
#' Write NetCDF files and their metadata with a progress bar for netcdf layers. Adapted from the .writeCDF method in the terra package: https://github.com/rspatial/terra/blob/master/R/ncdf.R
#'
#' @param spatraster SpatRaster
#' @param output_file Filename including directory
#' @param variable Character. Variable name to be saved as varname in NC output.
#' @param longname Character. Long variable name.
#' @param unit Character. Unit in which Variable is recorded.
#' @param attrs Named vector of metadata attributes
#' @param compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
#'
#' @importFrom terra is.lonlat
#' @importFrom terra xFromCol
#' @importFrom terra yFromRow
#' @importFrom terra crs
#' @importFrom terra ext
#' @importFrom terra res
#' @importFrom terra time
#' @importFrom terra nlyr
#' @importFrom terra ncol
#' @importFrom terra nrow
#' @importFrom ncdf4 ncdim_def
#' @importFrom ncdf4 ncvar_def
#' @importFrom ncdf4 nc_create
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncatt_put
#' @importFrom ncdf4 ncvar_put
#' @importFrom progress progress_bar
#' @importFrom utils packageVersion
#'
#' @return A SpatRaster with metadata written to the disk
#'
WriteNC <- function(spatraster, output_file, compression = 1, variable, longname, unit, attrs = NULL) {
    # Check if input is a SpatRaster
    if (!inherits(spatraster, "SpatRaster")) {
        stop("Input must be a SpatRaster object")
    }

    ## data precision and missing
    prec <- "float"
    valid_prec <- c("short", "integer", "float", "double", "byte")
    miss_vals <- c(-32768, -2147483647, -1.175494e38, -1.7976931348623157e308, 255)
    missval <- miss_vals[match(prec, valid_prec)]

    ## spatial
    if (terra::is.lonlat(spatraster, perhaps = TRUE, warn = FALSE)) {
        xname <- "longitude"
        yname <- "latitude"
        xunit <- "degrees_east"
        yunit <- "degrees_north"
    } else {
        xname <- "easting"
        yname <- "northing"
        xunit <- "meter" # probably
        yunit <- "meter" # probably
    }
    xdim <- ncdf4::ncdim_def(xname, xunit, terra::xFromCol(spatraster, 1:ncol(spatraster)))
    ydim <- ncdf4::ncdim_def(yname, yunit, terra::yFromRow(spatraster, 1:nrow(spatraster)))

    ## crs
    crs_def <- ncdf4::ncvar_def(
        name = "crs",
        units = "",
        dim = list(),
        missval = NULL,
        prec = "integer"
    ) # Add a CRS (coordinate reference system) variable.
    haveprj <- FALSE
    prj <- terra::crs(spatraster, proj = FALSE)
    # prj <- gsub("\n", "", prj)

    e <- terra::ext(spatraster)
    rs <- terra::res(spatraster)
    gt <- paste(trimws(formatC(as.vector(c(e$xmin, rs[1], 0, e$ymax, 0, -1 * rs[2])), 22)), collapse = " ")

    ## variable names
    vars <- variable
    lvar <- longname
    units <- unit

    ## time
    zname <- "time"
    if (length(terra::time(spatraster)) > 0) {
        # Extract time metadata
        zv <- terra::time(spatraster)
        cal <- "standard"
        # Ensure time values are numeric
        if (inherits(zv, "Date")) {
            # Convert Date objects to numeric days since 1970-01-01
            zv <- as.numeric(zv)
            zunit <- "days since 1970-01-01"
        } else if (inherits(zv, "POSIXct") || inherits(zv, "POSIXt")) {
            # Convert POSIXct objects to seconds since 1970-01-01
            zv <- as.numeric(zv)
            zunit <- "seconds since 1970-01-01"
        } else if (is.numeric(zv)) {
            # If already numeric, determine the unit based on the time step
            tstep <- attr(zv, "units") # Check for time units attribute, if available.
            if (tstep == "seconds") {
                zunit <- "seconds since 1970-01-01"
            } else if (tstep == "days") {
                zunit <- "days since 1970-01-01"
            } else if (tstep == "months") {
                zunit <- "months since 1970"
            } else if (tstep == "years") {
                zunit <- "years since 1970"
                zv <- zv - 1970 # Adjust numeric years to the correct origin.
            } else {
                zunit <- "unknown"
            }
        } else {
            stop("Time values must be numeric or convertible to numeric.")
        }
    } else {
        # If no time information, set default
        zv <- 1:nlyr(spatraster)
        zunit <- "unknown"
        cal <- NA
    }
    zdim <- ncdf4::ncdim_def(zname, zunit, zv, unlim = FALSE, create_dimvar = TRUE, calendar = cal)

    ## actual data
    # Define variable with compression
    var_def <- ncdf4::ncvar_def(
        name = vars,
        longname = lvar,
        units = units,
        dim = list(xdim, ydim, zdim),
        prec = prec,
        missval = missval,
        compression = compression,
        # shuffle = TRUE
    )

    # Create NetCDF file
    nc <- ncdf4::nc_create(output_file, vars = list(var_def, crs_def), force_v4 = TRUE)
    on.exit(ncdf4::nc_close(nc)) # Ensure the file is closed on function exit.

    # Progress bar
    pb <- progress_bar$new(
        format = "Writing layers to disk (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
        total = terra::nlyr(spatraster), # 100
        width = getOption("width"),
        clear = FALSE
    )
    progressIter <- 1:terra::nlyr(spatraster) # token reported in progress bar

    # Write data layer by layer
    for (WriteIter in 1:terra::nlyr(spatraster)) {
        # Extract the data for the WriteIter-th layer as a matrix
        layer_data <- as.matrix(spatraster[[WriteIter]])

        # Write the WriteIter-th layer to the NetCDF file
        ncdf4::ncvar_put(nc,
            varid = var_def,
            vals = layer_data,
            start = c(1, 1, WriteIter),
            count = c(terra::ncol(spatraster), terra::nrow(spatraster), 1)
        )

        # Update progress bar
        pb$tick(tokens = list(layer = progressIter[WriteIter]))
        Sys.sleep(0.05)
    }

    # Write CRS information
    if (prj != "") {
        haveprj <- TRUE
        ncdf4::ncatt_put(nc, crs_def, "crs_wkt", prj, prec = "text")
        proj4_string <- terra::crs(spatraster, proj = TRUE)
        if (proj4_string != "") {
            ncdf4::ncatt_put(nc, crs_def, "proj4", proj4_string, prec = "text")
        }
        epsg_code <- terra::crs(spatraster, describe = TRUE)[1, 3]
        if (!is.na(epsg_code)) {
            ncdf4::ncatt_put(nc, crs_def, "epsg_code", epsg_code, prec = "text")
        }
    }
    ncdf4::ncatt_put(nc, crs_def, "geotransform", gt, prec = "text")
    if (haveprj) {
        ncdf4::ncatt_put(nc, var_def, "grid_mapping", "crs", prec = "text")
    }

    ## general attributes
    ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.4", prec = "text")
    ncdf4::ncatt_put(nc, 0, "created_by", paste("R packages ncdf4, terra, and ClimHub (version ", packageVersion("ClimHub"), ")", sep = ""), prec = "text")
    ncdf4::ncatt_put(nc, 0, "date", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), prec = "text")

    ## further metadata
    if (!is.null(attrs)) {
        for (name in names(attrs)) {
            ncdf4::ncatt_put(nc, varid = 0, attname = name, attval = attrs[[name]])
        }
    }
}
