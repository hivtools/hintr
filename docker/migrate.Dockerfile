FROM ghcr.io/hivtools/naomi-base:latest AS build

COPY . /src
COPY scripts /usr/local/bin
COPY docker/Rprofile.site /usr/local/lib/R/etc/Rprofile.site

## Remove the large test data not needed for runtime
RUN install_packages remotes \
    && install_remote mrc-ide/naomi mrc-ide/naomi.options \
    && rm -r /usr/local/lib/R/site-library/naomi/extdata

FROM rocker/r-ver:4.0 AS run

## Install just the runtime-dependencies
RUN apt-get update && apt-get -y install --no-install-recommends \
        libudunits2-0 \
        libhiredis1.1.0 \
        libjq1 \
        libsodium23 \
        libssl3 \
	libnode-dev \
        gdal-bin \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/* \
        && rm -rf /tmp/*
        

COPY --from=build /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=build /usr/local/bin /usr/local/bin

RUN install_remote \
        qsbase/qs

ENV HINTR_QUEUE_ID=hintr

WORKDIR /

ENTRYPOINT ["/usr/local/bin/run_migration_2.10.21"]