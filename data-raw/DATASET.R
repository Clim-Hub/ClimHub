# Load Package ------------------
setwd("C:/Users/erikkus/OneDrive - CICERO senter for klimaforskning/Documents/ClimHub/GitHub")

document()
build()
load_all()


# Make Klima i Norge raw file ------------------
KiN_rast <- Download.KlimaiNorge2100(
  Variable = "Mean Air Temperature",
  DateStart = "2025-01-01",
  DateStop = "2025-01-31",
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
