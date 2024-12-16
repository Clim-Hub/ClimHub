# Load the required library
library(jsonlite)

# Example Meta_df data frame
Meta_df <- data.frame(
    name = c("Maximum Air Temperature", "Minimum Air Temperature", "Mean Air Temperature", "Snow Water Equivalent", "Soil Moisture Deficit", "Runoff", "Precipitation", "Groundwater", "Orography"),
    unit = c("K", "K", "K", "kg/m^2", "kg/m^2", "kg/m^2", "kg/m^2", "kg/m^2", "m"),
    prefix = c("TX", "TN", "TM", "SWE", "SMD", "RUN", "RR", "GRW", "ORO"),
    issues = c("", "", "Values recorded *10", "", "Values recorded *10", "Values recorded *10", "Values recorded *10", "", "No Time Component")
)

# Define the metadata for the dataset
dataset_metadata <- list(
    name = "Klima i Norge 2100",
    url = "https://thredds.met.no/thredds/catalog/KSS/Klima_i_Norge_2100/utgave2015/catalog.html",
    license = "CC-BY-4.0 and NLOD 2.0",
    citation = "Klima i Norge 2100 Data from The Norwegian Meteorological Institute (MET Norway)",
    doi = "",
    issn = "",
    isbn = "978-82-410-1512-0",
    access_method = "Direct Download",
    access_function = "Download.KlimaiNorge2100",
    scenarios = c("rcp85", "rcp45"),
    models = c("CNRM_CCLM", "CNRM_RCA", "EC-EARTH_CCLM", "EC-EARTH_HIRHAM", "EC-EARTH_RACMO", "EC-EARTH_RCA", "HADGEM_RCA", "IPSL_RCA", "MPI_CCLM", "MPI_RCA"),
    time = list(
        extent = c("1971-01-01", "2100-12-31"),
        resolution = 1,
        unit = "day"
    ),
    space = list(
        extent = c(-1.32644172144617, 32.5377018248157, 57.7603587380021, 72.0981239565917),
        resolution = 0.01535093,
        unit = "°",
        crs = "Custom CRS"
    )
)

# Create the 'variables' list from Meta_df
variables_list <- apply(Meta_df, 1, function(row) {
    as.list(row)
})

# Combine everything into the final JSON structure
final_json <- list(
    dataset = c(dataset_metadata, list(variables = variables_list))
)

# Convert to JSON and pretty-print
json_output <- toJSON(final_json, pretty = TRUE, auto_unbox = TRUE)
cat(json_output)

# Optionally, save to a file
write(json_output, "KlimaiNorge2100.json")
