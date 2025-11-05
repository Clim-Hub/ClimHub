# Load Package ------------------
setwd("C:/Users/erikkus/OneDrive - CICERO senter for klimaforskning/Documents/ClimHub/GitHub")

document()
build()
load_all()


# Make Klima i Norge raw file ------------------
KiN_rast <- Access_KlimaiNorge2100(
  variable = "Mean Air Temperature",
  dateStart = "2025-01-01",
  dateStop = "2025-01-10",
  model = "CNRM_CCLM",
  scenario = "rcp85",
  cores = 1,
  fileName = file.path(getwd(), "inst/extdata", "KiN_rast.nc"),
  compression = 9,
  removeTemporary = TRUE
)
# usethis::use_data(KiN_rast)

# Jotunheimen boundary as spatialfeatureobject ------------------
Jotunheimen_sf <- sf::st_read("data-raw/Shape/Shape-polygon.shp")
usethis::use_data(Jotunheimen_sf)

# Peaks as spatialfeatureobject ------------------
Peaks_df <- read.csv("data-raw/NorPeaks.csv")[, -1:-3]
Peaks_df$Longitude <- as.numeric(stringr::str_trim(Peaks_df$Longitude))
Nor2K_sf <- sf::st_as_sf(Peaks_df, coords = c("Longitude", "Latitude"), crs = sf::st_crs(Jotunheimen_sf))
usethis::use_data(Nor2K_sf, overwrite = TRUE)

# Metrics.ETCCDI objects ------------------
## Raw Data -----
TX_rast <- Access_KlimaiNorge2100(
  variable = "Maximum Air Temperature",
  dateStart = "2050-01-01",
  dateStop = "2051-12-31",
  scenario = "rcp85",
  model = "CNRM_CCLM",
  fileName = file.path(getwd(), "inst/extdata", "KiN_TX.nc"),
  writeFile = FALSE
)

TN_rast <- Access_KlimaiNorge2100(
  variable = "Minimum Air Temperature",
  dateStart = "2050-01-01",
  dateStop = "2051-12-31",
  scenario = "rcp85",
  model = "CNRM_CCLM",
  fileName = file.path(getwd(), "inst/extdata", "KiN_TN.nc"),
  writeFile = FALSE
)

BP_TM_rast <- Access_KlimaiNorge2100(
  variable = "Mean Air Temperature",
  dateStart = "1971-01-01",
  dateStop = "2000-12-31",
  model = "CNRM_CCLM",
  fileName = file.path(getwd(), "inst/extdata", "KiN_BP_TM.nc"),
  writeFile = FALSE
)

RR_rast <- Access_KlimaiNorge2100(
  variable = "Precipitation",
  dateStart = "2050-01-01",
  dateStop = "2051-12-31",
  scenario = "rcp85",
  model = "CNRM_CCLM",
  fileName = file.path(getwd(), "inst/extdata", "KiN_RR.nc"),
  writeFile = FALSE
)
RR_rast <- RR_rast / 10

BP_RR_rast <- Access_KlimaiNorge2100(
  variable = "Precipitation",
  dateStart = "1971-01-01",
  dateStop = "2000-12-31",
  model = "CNRM_CCLM",
  fileName = file.path(getwd(), "inst/extdata", "KiN_BP_RR.nc"),
  writeFile = FALSE
)

## Cropping -----
Jotunheimen_sf <- sf::st_read("data-raw/Shape/Shape-polygon.shp")
datals <- list(
  TX = TX_rast, TN = TN_rast, RR = RR_rast, 
  BP_TM = BP_TM_rast, BP_RR = BP_RR_rast
)

x <- lapply(names(datals), function(name) {
  # name <- "TBP"
  print(name)
  FNAME <- file.path(getwd(), "inst/extdata", paste0("Jotunheimen_", name, ".nc"))
  if(file.exists(FNAME)){
    ret <- terra::rast(FNAME)
  }else{
    x <- datals[[name]]
    y <- Spatial_Reproject(x, Jotunheimen_sf)
    z <- Spatial_CropMask(rast = y, shape = Jotunheimen_sf)
  if(unique(terra::units(x)) == "K"){
    z <- z/100
  }else{
    terra::units(x) <- "mm"
  }
  # terra::plot(z[[1]])
    ret <- NC_Write(
      spatRaster = z,
      fileName = FNAME,
      varName = unique(terra::varnames(x)),
      longName = unique(terra::longnames(x)),
      unit = unique(terra::units(x)),
      meta = setNames(terra::metags(x)[, 2], terra::metags(x)[, 1]),
      compression = 9
    )
  }
  ret
})

## Percentiles -----
stop("Make percentiles for BASEPeriods")
## make Reference into percentiles
library(terra)
BASEPeriod <- terra::rast("inst/extdata/Jotunheimen_BASEPeriod.nc")
percentiles <- app(BASEPeriod, fun = function(x) {
  quantile(x, probs = c(0.1, 0.9), na.rm = TRUE)
})
# Optional: give meaningful layer names
names(percentiles) <- c("p10", "p90")
terra::time(percentiles) <- rep(time(BASEPeriod[[1]]), 2)

BASEPeriod_rast <- NC_Write(
  spatRaster = percentiles / 100,
  fileName = file.path(getwd(), "inst/extdata", "Jotunheimen_BP_TM.nc"),
  varName = "Air Temperature Percentiles",
  longName = "Air Temperature Percentiles for 1971-2000",
  unit = "K",
  meta = setNames(terra::metags(BASEPeriod_rast)[, 2], terra::metags(BASEPeriod_rast)[, 1]),
  compression = 9
)

BASEPeriod_rast <- terra::rast("inst/extdata/Jotunheimen_BASEPeriod.nc")


unlink(list.files(pattern = "TEMP_", full.names = TRUE))

### Sufficiently large file for testing of NC_Write ------------------
NORA3 <- Access_NORA3(
    variable = "TS (Surface temperature)", # which variable
    dateStart = "1971-01-01 00", dateStop = "1971-05-31 18", # time-window
    leadTimeHour = 3, cores = 1,
    fileName = "NORA3.nc", compression = NA, # file storing
    removeTemporary = TRUE
)
