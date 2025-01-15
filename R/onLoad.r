.onAttach <- function(lib, pkg) {
    msg <- paste0("This is ClimHub (version ", packageVersion("ClimHub"), "). Please note that this package is in early development.")
    packageStartupMessage(msg)
}
# usethis::use_citation()
# Please cite it as Kusch, E., & Davy, R. (2022). KrigR-a tool for downloading and statistically downscaling climate reanalysis data. Environmental Research Letters, 17(2). https://doi.org/10.1088/1748-9326/ac48b3"
