FROM ghcr.io/hivtools/naomi-base@sha256:cd851dd027a154812ea0d4626f18c69042821d0cfd7e90221270d6f811d91a18 AS build

COPY . /src
COPY scripts /usr/local/bin
COPY docker/Rprofile.site /usr/local/lib/R/etc/Rprofile.site

## Remove the large test data not needed for runtime
RUN install_packages remotes \
    && install_remote mrc-ide/naomi mrc-ide/naomi.options \
    && rm -r /usr/local/lib/R/site-library/naomi/extdata

WORKDIR /src
RUN R CMD INSTALL /src \
    && rm -r /src

FROM rocker/r-ver:4.5 AS run

RUN apt-get update && apt-get -y install --no-install-recommends \
        libcurl4-openssl-dev \
        libgdal-dev \
        libhiredis-dev \
        libjq-dev \
        libprotobuf-dev \
        libssh-dev \
        libsodium-dev \
        libudunits2-dev \
        libv8-dev \
        libxml2-dev \
        protobuf-compiler \
        texinfo \
        texlive \
        texlive-fonts-extra \
        texlive-latex-extra \
        wget \
        xz-utils \
        zlib1g-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=build /usr/local/bin /usr/local/bin

RUN Rscript -e 'install.packages("remotes"); remotes::install_version("qs", version = "0.25.5", upgrade = "never")'

ENV HINTR_QUEUE_ID=hintr

WORKDIR /

ENTRYPOINT ["/usr/local/bin/run_migration_2.10.21"]
