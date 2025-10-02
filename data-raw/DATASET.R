# Load Package ------------------
setwd("C:/Users/erikkus/OneDrive - CICERO senter for klimaforskning/Documents/ClimHub/GitHub")

document()
build()
load_all()


# Make Klima i Norge raw file ------------------
KiN_rast <- Download.KlimaiNorge2100(
  Variable = "Mean Air Temperature",
  DateStart = "2025-01-01",
  DateStop = "2025-01-10",
  Model = "CNRM_CCLM",
  Scenario = "rcp85",
  Cores = 1,
  Dir = file.path(getwd(), "inst/extdata"),
  FileName = "KiN_rast",
  Compression = 9,
  RemoveTemporary = TRUE
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
TX_rast <- Download.KlimaiNorge2100(
  Variable = "Maximum Air Temperature",
  DateStart = "2050-01-01",
  DateStop = "2051-12-31", ,
  Scenario = "rcp85",
  Model = "CNRM_CCLM",
  FileName = "KiN_TX",
  WriteFile = FALSE
)

TN_rast <- Download.KlimaiNorge2100(
  Variable = "Minimum Air Temperature",
  DateStart = "2050-01-01",
  DateStop = "2051-12-31",
  Scenario = "rcp85",
  Model = "CNRM_CCLM",
  FileName = "KiN_TN",
  WriteFile = FALSE
)

BP_TM_rast <- Download.KlimaiNorge2100(
  Variable = "Mean Air Temperature",
  DateStart = "1971-01-01",
  DateStop = "2000-12-31",
  Model = "CNRM_CCLM",
  FileName = "KiN_BP_TM",
  WriteFile = FALSE
)

RR_rast <- Download.KlimaiNorge2100(
  Variable = "Precipitation",
  DateStart = "2050-01-01",
  DateStop = "2051-12-31",
  Scenario = "rcp85",
  Model = "CNRM_CCLM",
  FileName = "KiN_RR",
  WriteFile = FALSE
)
RR_rast <- RR_rast / 10

BP_RR_rast <- Download.KlimaiNorge2100(
  Variable = "Precipitation",
  DateStart = "1971-01-01",
  DateStop = "2000-12-31",
  Model = "CNRM_CCLM",
  FileName = "KiN_BP_RR",
  WriteFile = FALSE
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
  y <- Spatial.Reproject(x, Jotunheimen_sf)
  z <- Spatial.CropMask(Rast = y, Shape = Jotunheimen_sf)
  if(unique(terra::units(x)) == "K"){
    z <- z/100
  }else{
    terra::units(x) <- "mm"
  }
  # terra::plot(z[[1]])
  ret <- WriteRead.NC(
      NC = z,
      FName = FNAME,
      Variable = unique(terra::varnames(x)),
      LongVar = unique(terra::longnames(x)),
      Unit = unique(terra::units(x)),
      Attrs = setNames(terra::metags(x)[, 2], terra::metags(x)[, 1]),
      Write = TRUE,
      Compression = 9
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

BASEPeriod_rast <- WriteRead.NC(
  NC = percentiles / 100,
  FName = file.path(getwd(), "inst/extdata", "Jotunheimen_BP_TM.nc"),
  Variable = "Air Temperature Percentiles",
  LongVar = "Air Temperature Percentiles for 1971-2000",
  Unit = "K",
  Attrs = setNames(terra::metags(BASEPeriod_rast)[, 2], terra::metags(BASEPeriod_rast)[, 1]),
  Write = TRUE,
  Compression = 9
)

BASEPeriod_rast <- terra::rast("inst/extdata/Jotunheimen_BASEPeriod.nc")


unlink(list.files(pattern = "TEMP_", full.names = TRUE))
