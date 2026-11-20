FROM ghcr.io/hivtools/naomi-base:latest AS build

COPY . /src
COPY docker/bin /usr/local/bin/

## otelsdk sends the OpenTelemetry traces. It is only in Suggests, as it
## needs C++ to build from source, so install it here. At run time it needs
## libprotobuf (see the run stage).
## Remove the large test data not needed for runtime
RUN install_packages remotes otelsdk \
    && install_remote mrc-ide/naomi mrc-ide/naomi.options \
    && rm -r /usr/local/lib/R/site-library/naomi/extdata

WORKDIR /src
RUN ./scripts/build_test_data \
    && find /src/inst/output -type f ! -name "malawi_model_output.qs2" -delete \
    && R CMD INSTALL /src \
    && rm -r /src

FROM rocker/r-ver:4 AS run

## Install just the runtime-dependencies
RUN apt-get update && apt-get -y install --no-install-recommends \
        libudunits2-0 \
        libhiredis1.1.0 \
        libjq1 \
        libsodium23 \
        libssl3 \
        libprotobuf32t64 \
	libnode-dev \
        gdal-bin \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/* \
        && rm -rf /tmp/*

COPY --from=build /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=build /usr/local/bin /usr/local/bin
COPY --from=build /usr/bin/pandoc /usr/bin/pandoc

EXPOSE 8888
ENV HINTR_QUEUE_ID=hintr

## The name of the service in OpenTelemetry traces. It has no effect unless
## tracing is on (OTEL_TRACES_EXPORTER). It is set here, not in R, as otel
## reads it when the first span is made, which can be while packages load.
ENV OTEL_SERVICE_NAME=hintr

## Model run will try to parallelise over as many threads as are available
## potentially slowing the application, manually limit threads to 1
ENV OMP_NUM_THREADS=1

WORKDIR /

ENTRYPOINT ["/usr/local/bin/hintr_api"]
