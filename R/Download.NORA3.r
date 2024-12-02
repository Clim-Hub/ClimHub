#' Downloading NORA3 3km NOrwegian Reanalysis Data from the Norwegian Meteorological Instite
#'
#' This function is used to obtain the NORA3 data product hosted through \href{https://thredds.met.no/thredds/projects/nora3.html}{thredds.met.no}. Specifically, this function makes available the following datasets:
#'  1. NORA3 files contained within \href{https://thredds.met.no/thredds/catalog/nora3/catalog.html}{nora3}.
#'
#' @param Variable Character. NORA3 contains the following:
#'  - "land_area_fraction"
#'  - "specific_humidity_ml"
#'  - "turbulent_kinetic_energy_ml"
#'  - "cloud_area_fraction_ml"
#'  - "toa_net_downward_shortwave_flux"
#'  - "surface_downwelling_shortwave_flux_in_air"
#'  - "toa_outgoing_longwave_flux"
#'  - "surface_downwelling_longwave_flux_in_air"
#'  - "atmosphere_boundary_layer_thickness"
#'  - "pressure_departure"
#'  - "surface_air_pressure"
#'  - "air_temperature_ml"
#'  - "surface_geopotential"
#'  - "x_wind_ml"
#'  - "y_wind_ml"
#'  - "air_pressure_at_sea_level"
#'  - "precipitation_amount_acc"
#'  - "SIC (Sea_ice_fraction)",
#'  - "SST (Sea Surface Temperature (SST))",
#'  - "TS (Surface temperature)",
#'  - "T2M (2m_Temperature)",
#'  - "Q2M (2m_Specific_Humidity)",
#'  - "HU2M (2m_Relative_Humidity)",
#'  - "ZON10M (10m_Zonal_wind)",
#'  - "MER10M (10m_Meridian_Wind)",
#'  - "H (Averaged_Sensible_Heat_Flux)",
#'  - "LE (Averaged_Total_Latent_Heat_Flux)",
#'  - "GFLUX (Averaged_Ground_Heat_Flux)",
#'  - "air_temperature_0m"
#'  - "surface_geopotential"
#'  - "liquid_water_content_of_surface_snow"
#'  - "downward_northward_momentum_flux_in_air"
#'  - "downward_eastward_momentum_flux_in_air"
#'  - "integral_of_toa_net_downward_shortwave_flux_wrt_time"
#'  - "integral_of_surface_net_downward_shortwave_flux_wrt_time"
#'  - "integral_of_toa_outgoing_longwave_flux_wrt_time"
#'  - "integral_of_surface_net_downward_longwave_flux_wrt_time"
#'  - "integral_of_surface_downward_latent_heat_evaporation_flux_wrt_time"
#'  - "integral_of_surface_downward_latent_heat_sublimation_flux_wrt_time"
#'  - "water_evaporation_amount"
#'  - "surface_snow_sublimation_amount_acc"
#'  - "integral_of_surface_downward_sensible_heat_flux_wrt_time"
#'  - "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time"
#'  - "integral_of_surface_downwelling_longwave_flux_in_air_wrt_time"
#'  - "air_temperature_2m"
#'  - "relative_humidity_2m"
#'  - "specific_humidity_2m"
#'  - "x_wind_10m"
#'  - "y_wind_10m"
#'  - "cloud_area_fraction"
#'  - "x_wind_gust_10m"
#'  - "y_wind_gust_10m"
#'  - "air_temperature_max"
#'  - "air_temperature_min"
#'  - "convective_cloud_area_fraction"
#'  - "high_type_cloud_area_fraction"
#'  - "medium_type_cloud_area_fraction"
#'  - "low_type_cloud_area_fraction"
#'  - "atmosphere_boundary_layer_thickness"
#'  - "hail_diagnostic"
#'  - "rainfall_amount"
#'  - "snowfall_amount"
#'  - "graupelfall_amount"
#'  - "x_wind_pl"
#'  - "y_wind_pl"
#'  - "air_temperature_pl"
#'  - "cloud_area_fraction_pl"
#'  - "geopotential_pl"
#'  - "relative_humidity_pl"
#'  - "upward_air_velocity_pl"
#'  - "air_pressure_at_sea_level"
#'  - "lwe_thickness_of_atmosphere_mass_content_of_water_vapor"
#'  - "surface_air_pressure"
#'  - "lifting_condensation_level"
#'  - "atmosphere_level_of_free_convection"
#'  - "atmosphere_level_of_neutral_buoyancy"
#'  - "wind_direction"
#'  - "wind_speed"
#'  - "precipitation_amount_acc"
#'  - "snowfall_amount_acc"
#'  - "x_wind_z"
#'  - "y_wind_z"
#' @param DateStart Character. "YYYY-MM-DD HH" date at which to start time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param DateStop Character. "YYYY-MM-DD HH" date at which to stop time series of downloaded data. Data is available daily at hours 00, 06, 12, and 18.
#' @param Leadtime Integer. Lead time of reanalysis, either 3, 4, 5, 6, 7, 8 or 9.
#' @param Cores Integer. How many cores to use for parallel downloads. Default NULL defines no parallelisation.
#' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
#' @param FileName Character. A file name for the produced file.
#' @param Compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
#' @param RemoveTemporary Logical. Whether to delete temporary files after completion.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom terra rast
#' @importFrom terra metags
#' @importFrom terra writeRaster
#' @importFrom utils download.file
#' @importFrom pbapply pblapply
#' @importFrom stringr str_pad
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom progress progress_bar
#'
#' @return A SpatRaster object containing the downloaded data, and a file in the specified directory. The SpatRaster contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#'  - *Citation* - A string which to use for in-line citation of the data product obtained}.
#'
#'
#' @examples
#' \dontrun{
#'
#' }

#' }
#' @export
Download.NORA3 <- function(Variable = "TS (Surface temperature)", # which variable
                           DateStart = "1961-08-01 00", DateStop = "2022-12-31 18", # time-window
                           Leadtime = 3, Cores = NULL,
                           Dir = getwd(), FileName, Compression = 9, # file storing
                           RemoveTemporary = TRUE) {
    ## Data files ========= # replace with metadata ASAP!
    NORA3_df <- data.frame(
        variable = c(
            "land_area_fraction",
            "specific_humidity_ml",
            "turbulent_kinetic_energy_ml",
            "cloud_area_fraction_ml",
            "toa_net_downward_shortwave_flux",
            "surface_downwelling_shortwave_flux_in_air",
            "toa_outgoing_longwave_flux",
            "surface_downwelling_longwave_flux_in_air",
            "atmosphere_boundary_layer_thickness",
            "pressure_departure",
            "surface_air_pressure",
            "air_temperature_ml",
            "surface_geopotential",
            "x_wind_ml",
            "y_wind_ml",
            "air_pressure_at_sea_level",
            "precipitation_amount_acc",
            "SIC (Sea_ice_fraction)",
            "SST (Sea Surface Temperature (SST))",
            "TS (Surface temperature)",
            "T2M (2m_Temperature)",
            "Q2M (2m_Specific_Humidity)",
            "HU2M (2m_Relative_Humidity)",
            "ZON10M (10m_Zonal_wind)",
            "MER10M (10m_Meridian_Wind)",
            "H (Averaged_Sensible_Heat_Flux)",
            "LE (Averaged_Total_Latent_Heat_Flux)",
            "GFLUX (Averaged_Ground_Heat_Flux)",
            "air_temperature_0m",
            "surface_geopotential",
            "liquid_water_content_of_surface_snow",
            "downward_northward_momentum_flux_in_air",
            "downward_eastward_momentum_flux_in_air",
            "integral_of_toa_net_downward_shortwave_flux_wrt_time",
            "integral_of_surface_net_downward_shortwave_flux_wrt_time",
            "integral_of_toa_outgoing_longwave_flux_wrt_time",
            "integral_of_surface_net_downward_longwave_flux_wrt_time",
            "integral_of_surface_downward_latent_heat_evaporation_flux_wrt_time",
            "integral_of_surface_downward_latent_heat_sublimation_flux_wrt_time",
            "water_evaporation_amount",
            "surface_snow_sublimation_amount_acc",
            "integral_of_surface_downward_sensible_heat_flux_wrt_time",
            "integral_of_surface_downwelling_shortwave_flux_in_air_wrt_time",
            "integral_of_surface_downwelling_longwave_flux_in_air_wrt_time",
            "air_temperature_2m",
            "relative_humidity_2m",
            "specific_humidity_2m",
            "x_wind_10m",
            "y_wind_10m",
            "cloud_area_fraction",
            "x_wind_gust_10m",
            "y_wind_gust_10m",
            "air_temperature_max",
            "air_temperature_min",
            "convective_cloud_area_fraction",
            "high_type_cloud_area_fraction",
            "medium_type_cloud_area_fraction",
            "low_type_cloud_area_fraction",
            "atmosphere_boundary_layer_thickness",
            "hail_diagnostic",
            "rainfall_amount",
            "snowfall_amount",
            "graupelfall_amount",
            "x_wind_pl",
            "y_wind_pl",
            "air_temperature_pl",
            "cloud_area_fraction_pl",
            "geopotential_pl",
            "relative_humidity_pl",
            "upward_air_velocity_pl",
            "air_pressure_at_sea_level",
            "lwe_thickness_of_atmosphere_mass_content_of_water_vapor",
            "surface_air_pressure",
            "lifting_condensation_level",
            "atmosphere_level_of_free_convection",
            "atmosphere_level_of_neutral_buoyancy",
            "wind_direction",
            "wind_speed",
            "precipitation_amount_acc",
            "snowfall_amount_acc",
            "x_wind_z",
            "y_wind_z"
        ),
        datafile = c(
            rep("", 17),
            rep("_sfx", 11),
            rep("_fp", 54)
        )
    )

    ## Catching Most Frequent Issues ============
    message("###### Checking Request Validity")
    ## FileName Specification
    FileName <- paste0(file_path_sans_ext(FileName), ".nc")

    ## check variable names
    # Unit <-

    ## check dates & times

    ## variable in which file
    FilePrefix <- NORA3_df$datafile[Variable == NORA3_df$variable]

    ## Metadata
    Citation <- paste("NORA3 (DOI: 10.5194/wes-6-1501-2021) data provided by the The Norwegian Meteorological institute obtained on", Sys.Date())
    names(Citation) <- "Citation"
    callargs <- mget(names(formals()), sys.frame(sys.nframe()))
    callargs[sapply(callargs, is.null)] <- "NULL"
    callargs[sapply(callargs, class) == "name"] <- ""
    names(callargs) <- paste("Call", names(callargs), sep = "-")
    Meta_vec <- c(Citation, unlist(callargs))

    ## File Check
    FCheck <- WriteRead.FileCheck(FName = FileName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
    if (!is.null(FCheck)) {
        FCheck <- WriteRead.NC(NC = FCheck, FName = file.path(Dir, FileName), Attrs = Meta_vec)
        return(FCheck)
    }

    ## temporary files names, we do this in UTC to avoid daylight savings shenanigans
    Start <- as.POSIXct(paste0(DateStart, ":00:00"), tz = "UTC")
    Stop <- as.POSIXct(paste0(DateStop, ":00:00"), tz = "UTC")
    Datetimes <- seq(
        from = Start,
        to = Stop,
        by = "6 hour"
    )
    Datetimes <- format(Datetimes, "%Y%m%d%H")
    FNames <- paste0("TEMP_", "fc", Datetimes, "_", stringr::str_pad(Leadtime, 3, "left", 0), FilePrefix, ".nc")

    ## parallelisation
    pb <- progress_bar$new(
        format = "Downloading (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
        total = length(FNames), # 100
        width = getOption("width"),
        clear = FALSE
    )
    progressIter <- 1:length(FNames) # token reported in progress bar
    if (!is.null(Cores) | Cores > 1) {
        cl <- makeCluster(Cores)
        doSNOW::registerDoSNOW(cl)
        progress <- function(n) {
            pb$tick(tokens = list(layer = progressIter[n]))
        }
        on.exit(stopCluster(cl))
        ForeachObjects <- c("Dir", "FNames")
    } else {
        cl <- NULL
    }

    ## Downloads ================================
    message("###### Data Download")
    Downls <- foreach(
        DownIter = 1:length(FNames),
        .packages = c(),
        .export = ForeachObjects,
        .options.snow = list(progress = progress)
    ) %dopar% { # parallel loop
        FName <- FNames[DownIter]
        Year <- substr(FName, 8, 11)
        Month <- substr(FName, 12, 13)
        Day <- substr(FName, 14, 15)
        Hour <- substr(FName, 16, 17)
        URL <- paste("https://thredds.met.no/thredds/fileServer/nora3", Year, Month, Day, Hour,
            gsub(FName, pattern = "TEMP_", replacement = ""),
            sep = "/"
        )
        if (!file.exists(file.path(Dir, FName))) {
            download.file(
                url = URL,
                destfile = file.path(Dir, FName),
                method = "wget",
                quiet = TRUE
            )
        }
        Sys.sleep(0.5)
        NULL
    } # end of parallel kriging loop

    ## Loading Data =================================
    message("###### Loading Downloaded Data from Disk")
    pb <- progress_bar$new(
        format = "Downloading (:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta",
        total = length(FNames), # 100
        width = getOption("width"),
        clear = FALSE
    )
    progressIter <- 1:length(FNames) # token reported in progress bar

    MetNo_rast <- as.list(rep(NA, length(FNames)))
    for (LoadIter in 1:length(FNames)) {
        MetNo_rast[[LoadIter]] <- terra::rast(file.path(Dir, FNames[LoadIter]))
        pb$tick(tokens = list(layer = progressIter[LoadIter]))
    }
    MetNo_rast <- do.call(c, MetNo_rast)

    ## Variable Extraction =================================
    message("###### Extracting Requested Variable")
    if (FilePrefix == "_sfx") {
        VarLyr <- which(
            startsWith(
                prefix = stringr::str_trim(stringr::str_extract(Variable, "^[^(]*"), side = "right"),
                x = names(MetNo_rast)
            )
        )
    } else {
        VarLyr <- grep(Variable, names(MetNo_rast))
    }
    MetNo_rast <- MetNo_rast[[VarLyr]]

    ## Exports =================================
    message("###### Data Export & Return")

    ### Assign additional information
    terra::metags(MetNo_rast) <- Meta_vec

    ### write file
    MetNo_rast <- WriteRead.NC(
        NC = MetNo_rast, FName = file.path(Dir, FileName),
        Variable = Variable, Unit = unique(terra::units(MetNo_rast)),
        Attrs = terra::metags(MetNo_rast), Write = TRUE, Compression = Compression
    )

    ### unlink temporary files
    if (RemoveTemporary) {
        unlink(file.path(Dir, FNames))
    }

    ### return object
    return(MetNo_rast)
}
