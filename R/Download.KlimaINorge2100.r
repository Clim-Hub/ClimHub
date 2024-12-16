#' Downloading Klima i Norge 2100 Data from the Norwegian Meteorological Instite
#'
#' This function is used to obtain the \href{https://www.miljodirektoratet.no/publikasjoner/2015/september-2015/klima-i-norge-2100/}{Klima_i_Norge_2100} data product hosted through \href{https://thredds.met.no/thredds/catalog/KSS/catalog.html}{thredds.met.no}. Specifically, this function makes available the following datasets:
#'  1. \href{https://publikasjoner.nve.no/rapport/2016/rapport2016_59.pdf}{Gridded 1 x 1 km climate and hydrological projections for Norway} data contained within \href{https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge_2100/utgave2015/catalog.html}{utgave 2015}.
#'
#' @param Variable Character. An overview of Klima i Norge variables can be obtained with `Meta.Variables(dataset = "KlimaiNorge2100")`.
#' @param DateStart Character. "YYYY-MM-DD" date at which to start time series of downloaded data. Data is available daily at hourly intervals.
#' @param DateStop Character. "YYYY-MM-DD" date at which to stop time series of downloaded data. Data is available daily at hourly intervals.
#' @param Model Character. An overview of climate models from which data can be obtained can be obtained with `Meta.QuickFacts("KlimaiNorge2100")$models`.
#' @param Scenario Character. An overview of climate models from which data can be obtained can be obtained with `Meta.QuickFacts("KlimaiNorge2100")$scenarios`. Note that this choice only affects data post-dating 2004-12-31.
#' @param Cores Integer. How many cores to use for parallel downloads. Default NULL defines no parallelisation.
#' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
#' @param FileName Character. A file name for the produced file.
#' @param Compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
#' @param RemoveTemporary Logical. Whether to delete temporary files after completion.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom terra metags
#' @importFrom terra crs
#' @importFrom terra names
#' @importFrom terra time
#' @importFrom stringr str_pad
#'
#' @return A SpatRaster object containing the downloaded data, and a file in the specified directory. The SpatRaster contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
#'  - *Citation* - A string which to use for in-line citation of the data product obtained.
#'  - *Call_* - A set of strings matching arguments supplied to the download function call
#'
#'
#' @examples
#' \dontrun{
#'
#' }
#' @export
Download.KlimaiNorge2100 <- function(
    Variable, # which variable
    DateStart, DateStop, # time-window
    Model, Scenario,
    Cores = 1,
    Dir = getwd(), FileName, Compression = 9, # file storing
    RemoveTemporary = TRUE) {





}





# #' @param DataSet Character. Which dataset to query data from. Supported strings:
# #'  - "utgave2015" (for annual 1x1km historic and projection data)
# #'  - "nyindekser2015" (for 1x1km climate indices)
# #'  - "masks" (for grids of regions and counties across Norway)
# #' @param Variable Character. Ignored when Dataset != "utgave2015". Klima_i_Norge_2100 contains the following:
# #'  - "TX" ... Daily_maximum_air_temperature
# #'  - "TN" ... Daily_minimum_air_temperature
# #'  - "TM" ... Daily_mean_air_temperature
# #'  - "SWE" ... Snow_water_equivalent_amount
# #'  - "SMD" ... Soil_moisture_deficit_amount_per_day
# #'  - "RUN" ... runoff_amount_per_day
# #'  - "RR" ... precipitation_amount_per_day
# #'  - "GRW" ... groundwater_amount
# #'  - "ORO" ... orography
# #' @param Model Character. Ignored when Dataset != "utgave2015". Klima_i_Norge_2100 contains the following:
# #'  - CNRM_CCLM
# #'  - CNRM_RCA
# #'  - EC-EARTH_CCLM
# #'  - EC-EARTH_HIRHAM
# #'  - EC-EARTH_RACMO
# #'  - EC-EARTH_RCA
# #'  - HADGEM_RCA
# #'  - IPSL_RCA
# #'  - MPI_CCLM
# #'  - MPI_RCA.
# #' @param Scenario Character. Ignored when Dataset != "utgave2015". Klima_i_Norge_2100 contains the following:
# #'  - rcp85 ... RCP85 (2005 - 2100)
# #'  - "rcp45" ... RCP45 (2005 - 2100)
# #'  - "hist" ... Historical conditions (1971 - 2005)
# #' @param File Character. Required when Dataset != "utgave2015". Name of .nc file hosted on thredds.
# #' @param DateStart Numeric. Ignored when Dataset != "utgave2015". Year ('YYYY') at which to start time series of downloaded data.
# #' @param DateStop Numeric. Ignored when Dataset != "utgave2015". Year ('YYYY') at which to stop time series of downloaded data.
# #' @param Dir Character/Directory Pointer. Directory specifying where to download data to.
# #' @param FileName Character. A file name for the produced file.
# #' @param Compression Integer between 1 to 9. Applied to final .nc file that the function writes to hard drive. Same as compression argument in terra::writeCDF().
# #'
# #' @importFrom tools file_path_sans_ext
# #' @importFrom terra rast
# #' @importFrom terra metags
# #' @importFrom terra writeRaster
# #' @importFrom utils download.file
# #' @importFrom pbapply pblapply
# #'
# #' @return A SpatRaster object containing the downloaded data, and a file in the specified directory. The SpatRaster contains metadata/attributes as a named vector that can be retrieved with terra::metags(...):
# #'  - *Citation* - A string which to use for in-line citation of the data product obtained}.
# #'
# #'
# #' @examples
# #' \dontrun{
# #' UtgaveTest <- Download.KlimaiNorge2100(
# #'   Variable = "TM", # which variable
# #'   DataSet = "utgave2015", # data set
# #'   Model = "MPI_RCA", # climate model
# #'   Scenario = "hist", # scenario
# #'   File = NULL, # single file specification
# #'   DateStart = 1971, DateStop = 2005, # time-window
# #'   Dir = getwd(), FileName = "TM-HIST_MPI_RCA" # file storing
# #' )
# #' terra::plot(UtgaveTest[[1:2]])
# #'
# #' UtgaveORO <- Download.KlimaiNorge2100(
# #'   Variable = "ORO",
# #'   DataSet = "utgave2015",
# #'   FileName = "ORO"
# #' )
# #'
# #' SingleFile <- Download.KlimaiNorge2100(
# #'   DataSet = "nyeindekser2015",
# #'   File = "snowMap1x1.nc",
# #'   FileName = "SnowIndex"
# #' )
# #' }
# #' @export
# Download.KlimaiNorge2100 <- function(Variable = "TM", # which variable
#                                      DataSet = "utgave2015", # data set
#                                      Model = "MPI_RCA", # climate model
#                                      Scenario = "hist", # scenario
#                                      File = NULL, # single file specification
#                                      DateStart = 1971, DateStop = 2005, # time-window
#                                      Dir = getwd(), FileName, Compression = 9 # file storing
# ) {
#   ## Catching Most Frequent Issues ============
#   message("Checking Request Validity")
#   ## FileName Specification
#   FileName <- paste0(file_path_sans_ext(FileName), ".nc")

#   ## check dates
#   if ((DateStop > 2005 & Scenario == "hist") | (DateStart < 1971 & Scenario == "hist")) {
#     stop("You cannot obtain historical data before 1971 or after 2005.")
#   }
#   if ((DateStop > 2100 & Scenario != "hist") | (DateStart < 2006 & Scenario != "hist")) {
#     stop("You cannot obtain projection data before 2006 or after 2100.")
#   }
#   ## force single-file download for ORO data
#   if (DataSet == "utgave2015") {
#     File <- NULL
#     if (Variable == "ORO") {
#       File <- "ORO/KSSgrid_DEM_1km_masl.nc"
#     }
#   }
#   ## Metadata
#   Meta_vec <- paste("Klima i Norge 2100 (ISSN: 2387-3027) data provided by the The Norwegian Meteorological institute obtained on", Sys.Date())
#   names(Meta_vec) <- "Citation"

#   ## File Check
#   FCheck <- WriteRead.FileCheck(FName = FileName, Dir = Dir, loadFun = terra::rast, load = TRUE, verbose = TRUE)
#   if (!is.null(FCheck)) {
#     FCheck <- WriteRead.NC(NC = FCheck, FName = file.path(Dir, FileName), Attrs = Meta_vec)
#     return(FCheck)
#   }

#   ## Downloads ================================
#   message("###### Data Download")
#   ## time series download vs single-file download
#   if (DataSet == "utgave2015" & is.null(File)) {
#     FNames <- paste("TEMP", Scenario, Model, Variable, "daily", DateStart:DateStop, "v4.nc", sep = "_")
#     print(paste("Staging", length(FNames), "download(s)."))
#     Files_ls <- pblapply(FNames, FUN = function(FName) {
#       URL <- paste("https://thredds.met.no/thredds/fileServer/KSS/Klima_i_Norge_2100/utgave2015", Variable, Model, Scenario,
#         gsub(FName, pattern = "TEMP_", replacement = ""),
#         sep = "/"
#       )
#       download.file(
#         url = URL,
#         destfile = file.path(Dir, FName),
#         method = "wget",
#         quiet = TRUE
#       )
#       terra::rast(file.path(Dir, FName))
#     })
#     MetNo_rast <- do.call(c, Files_ls)
#   } else {
#     FNames <- File
#     URL <- paste("https://thredds.met.no/thredds/fileServer/KSS/Klima_i_Norge_2100", DataSet, File, sep = "/")
#     download.file(
#       url = URL,
#       destfile = file.path(Dir, basename(File)),
#       method = "wget"
#     )
#     MetNo_rast <- terra::rast(file.path(Dir, basename(File)))
#   }

#   ## Exports =================================
#   message("Data Export & Return")

#   ### Assign additional information
#   terra::metags(MetNo_rast) <- Meta_vec

#   ### write file
#   MetNo_rast <- WriteRead.NC(
#     NC = MetNo_rast, FName = file.path(Dir, FileName),
#     Variable = Variable,
#     Attrs = terra::metags(MetNo_rast), Write = TRUE, Compression = Compression
#   )

#   ### unlink temporary files
#   unlink(file.path(Dir, FNames))

#   ### return object
#   return(MetNo_rast)
# }
