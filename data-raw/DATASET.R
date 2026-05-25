# Load Package ------------------
# document()
# # build()
load_all()

# Make NORA3 raw file ------------------
NORA3_rast <- Access_NORA3(
  variable = "T2M", dateStart = "2001-08-01 00", dateStop = "2001-08-01 00",
  leadTimeHour = 3,
  fileName = file.path(getwd(), "inst/extdata", "NORA3.nc")
)

# # Jotunheimen boundary as spatialfeatureobject ------------------
# Jotunheimen_sf <- sf::st_read("data-raw/Shape/Shape-polygon.shp")
# usethis::use_data(Jotunheimen_sf)

# # Peaks as spatialfeatureobject ------------------
# Peaks_df <- read.csv("data-raw/NorPeaks.csv")[, -1:-3]
# Peaks_df$Longitude <- as.numeric(stringr::str_trim(Peaks_df$Longitude))
# Nor2K_sf <- sf::st_as_sf(Peaks_df, coords = c("Longitude", "Latitude"), crs = sf::st_crs(Jotunheimen_sf))
# usethis::use_data(Nor2K_sf, overwrite = TRUE)
