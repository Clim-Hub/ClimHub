FROM rocker/r-ver:4.5.1
LABEL org.opencontainers.image.source="https://github.com/Clim-Hub/ClimHub"

RUN apt-get update && apt-get install -y --no-install-recommends \
    git \
    make \
    g++ \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    gdal-bin \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libnetcdf-dev \
    netcdf-bin \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -q -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('remotes','pak'))"

WORKDIR /opt/climhub
COPY . /opt/climhub

RUN R -q -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); pak::pkg_install('ggplot2'); pak::pkg_install('.'); library(ClimHub); packageVersion('ClimHub')"

CMD ["R"]
