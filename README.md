<!-- badges: start -->

<!-- change here for badges when ready -->

<!-- [![DOI](https://zenodo.org/badge/116978043.svg)](https://zenodo.org/badge/latestdoi/116978043)
[![codecov](https://codecov.io/gh/derek-corcoran-barrios/NetworkExtinction/branch/master/graph/badge.svg?token=BqPVAVQVBv)](https://codecov.io/gh/derek-corcoran-barrios/NetworkExtinction)
[![CRAN status](https://www.r-pkg.org/badges/version/NetworkExtinction)](https://CRAN.R-project.org/package=NetworkExtinction)
[![R-CMD-check](https://github.com/derek-corcoran-barrios/NetworkExtinction/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/derek-corcoran-barrios/NetworkExtinction/actions/workflows/R-CMD-check.yaml) -->


<!-- badges: end -->

<div style="border: 1px solid #f5c6cb; background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px; text-align: center;">
  ⚠️ <strong>The package is currently still in early development.</strong>
</div>

# ClimHub

<img align="right" width="300" src="inst/figures/CodeBadge.png" />

`ClimHub` is an R Package for downloading and processing a variety of climate data products. The package interfaces with select data products provided by:
1. The [Climate Data Store (CDS)](https://cds.climate.copernicus.eu/#!/home) hosted by the [Copernicus Climate Change Service (C3S)](https://cds.climate.copernicus.eu/about-c3s) of the [European Centre for Medium-Range Weather Forecasts (ECMWF)](https://www.ecmwf.int/)
1. The [Norwegian Meteorological Institute](https://www.met.no/en)

`ClimHub` contains functionality centred on four distinct groups of `R` functions:

<img src="inst/figures/PackageWorkflow.png" style="width: 100%;" />

1. **Data Discovery.**
   - Indexing and metadata querying of climate data accessible via `ClimHub`
   <!-- 2. Matching of user-needs with climate data accessible via `ClimHub` -->
2. **Data Access.**
   - Download of met.no data products via direct file transfer
   - Download of ECMWF CDS data products via API calls
3. **Data Processing.**
   - Spatial Operations:
     - Cropping and masking
     - Reprojection
     <!-- - Interpolation -->
   - Temporal Operations
     - Aggregation
     - Decumulation
   - Calculation of Aggregate Metrics/Indices
4. **Data Dissemination.**
   - Data provenance via metadata
   <!-- - Visualisation -->

# How to Cite
`ClimHub` has not been published yet. Please cite it for the time being as you would any other GitHub page.

# How to Contribute
I will explain how to make feature requests or report bugs here as soon as I have prepared issue templates for these purposes.

# Installation
`ClimHub` is not yet on CRAN. The package can currently be used either as a local R installation from GitHub or through a prebuilt Docker image.

## Install in R
The simplest installation route is directly from GitHub:

```r
install.packages("pak")
pak::pkg_install("Clim-Hub/ClimHub")
library(ClimHub)
```

An alternative using `remotes` is:

```r
install.packages("remotes")
remotes::install_github("Clim-Hub/ClimHub")
library(ClimHub)
```

To access ECMWF CDS data products, users require personal CDS API-access tokens which can be obtained [here](https://accounts.ecmwf.int/auth/realms/ecmwf/login-actions/registration?client_id=cds&tab_id=VkbipqjwuIQ).

## Use via Docker
Container images are published to GitHub Container Registry under `ghcr.io/clim-hub/climhub`. Tags follow the package version in `DESCRIPTION`, followed by the architecture (e.g., `arm64` or `amd64`) depending on your Docker runtime.

Aviable versions can be browsed [here](https://github.com/orgs/Clim-Hub/packages).


Pull a specific version:

```bash
docker pull ghcr.io/clim-hub/climhub:<version>-<arch>
```

Run a quick package-load check:

```bash
docker run --rm ghcr.io/clim-hub/climhub:<version>-<arch> \
  Rscript -e "library(ClimHub); packageVersion('ClimHub')"
```

Run a package function inside the container:

```bash
docker run --rm ghcr.io/clim-hub/climhub:<version>-<arch> \
  Rscript -e "library(ClimHub); print(Discovery_Library())"
```

Open an interactive R session:

```bash
docker run --rm -it ghcr.io/clim-hub/climhub:<version>-<arch> R
```

Build the image locally from the repository root:

```bash
docker buildx build --platform linux/arm64 -t climhub:arm64 --load .
docker buildx build --platform linux/amd64 -t climhub:amd64 --load .
```

## Publishing new container versions
This step is only needed when maintaining the published Docker images.

Read the version directly from `DESCRIPTION`:

```bash
VERSION=$(sed -n 's/^Version: //p' DESCRIPTION)
```

Log in to GitHub Container Registry:

```bash
docker login ghcr.io -u YOUR_GITHUB_USERNAME
```

Push the architecture-specific images:

```bash
docker tag climhub:arm64 ghcr.io/clim-hub/climhub:${VERSION}-arm64
docker push ghcr.io/clim-hub/climhub:${VERSION}-arm64

docker tag climhub:amd64 ghcr.io/clim-hub/climhub:${VERSION}-amd64
docker push ghcr.io/clim-hub/climhub:${VERSION}-amd64
```

Create the multi-platform manifest under the plain version tag:

```bash
docker buildx imagetools create \
  -t ghcr.io/clim-hub/climhub:${VERSION} \
  ghcr.io/clim-hub/climhub:${VERSION}-arm64 \
  ghcr.io/clim-hub/climhub:${VERSION}-amd64
```

Optionally update `latest`:

```bash
docker buildx imagetools create \
  -t ghcr.io/clim-hub/climhub:latest \
  ghcr.io/clim-hub/climhub:${VERSION}-arm64 \
  ghcr.io/clim-hub/climhub:${VERSION}-amd64
```

Verify the published manifest:

```bash
docker buildx imagetools inspect ghcr.io/clim-hub/climhub:${VERSION}
```

# Walkthrough
A walkthrough of the basic functionality of `ClimHub` will be developed and added here when data visualisation and automated code coverage and testing is in place. 
