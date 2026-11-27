## The build stage only depends on naomi and the test inputs, not on hintr's
## source, so CI can cache it. It only rebuilds when naomi-base, naomi or the
## inputs change.
FROM ghcr.io/hivtools/naomi-base:latest AS build

COPY docker/bin /usr/local/bin/

## CI passes the commits to install, so that a new naomi commit invalidates
## the cache. Defaults install the latest.
ARG NAOMI_REF=HEAD
ARG NAOMI_OPTIONS_REF=HEAD

## otelsdk sends the OpenTelemetry traces. It is only in Suggests, as it
## needs C++ to build from source, so install it here. At run time it needs
## libprotobuf (see the run stage).
## Remove the large test data not needed for runtime
RUN install_packages remotes otelsdk \
    && install_remote mrc-ide/naomi@${NAOMI_REF} \
        mrc-ide/naomi.options@${NAOMI_OPTIONS_REF} \
    && rm -r /usr/local/lib/R/site-library/naomi/extdata

## The mock model returns this model output. Copy in only what building it
## needs so this layer stays cached when hintr's source changes.
WORKDIR /src
COPY scripts/build_test_data scripts/run_model scripts/*_payload.json \
    /src/scripts/
COPY tests/testthat/testdata /src/tests/testthat/testdata
RUN ./scripts/build_test_data

## Shrink the library copied to the runtime image. C++ headers are only used
## when compiling (BH alone is ~150MB), vignettes and help aren't used by the
## API, and naomi's TMB model is compiled with debug symbols (69MB -> 2MB).
RUN cd /usr/local/lib/R/site-library \
    && rm -rf */include */doc */help */html \
    && find . -name "*.so" -exec strip --strip-debug {} +

FROM rocker/r-ver:4 AS runtime

## Install just the runtime-dependencies. Use the GDAL library rather than
## gdal-bin, which pulls in python and numpy for its command line tools. V8's
## binary bundles its own engine so doesn't need libnode. libuv is for fs.
## To check nothing is missing, run in the image:
##   find /usr/local/lib/R -name "*.so" -exec ldd {} + | grep "not found"
## (libR, tcltk and RcppParallel's tbbmalloc_proxy are expected)
RUN apt-get update && apt-get -y install --no-install-recommends \
        libuv1t64 \
        libudunits2-0 \
        libhiredis1.1.0 \
        libjq1 \
        libsodium23 \
        libssl3 \
        libprotobuf32t64 \
        libgdal38 \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/* \
        && rm -rf /tmp/*

COPY --from=build /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=build /usr/local/bin /usr/local/bin
COPY --from=build /usr/bin/pandoc /usr/bin/pandoc

## hintr has no compiled code so it installs here without build tools, which
## keeps the cached build stage independent of hintr's source. The source
## and model output are bind mounted (writes are discarded) rather than
## copied, so they don't add layers to the image.
RUN --mount=type=bind,target=/src,rw \
    --mount=type=bind,from=build,source=/src/inst/output,target=/output \
    mkdir -p /src/inst/output \
    && cp /output/malawi_model_output.qs2 /src/inst/output/ \
    && R CMD INSTALL /src

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

## The worker is the same image with a different entrypoint (build with
## --target worker). The server is the last stage so it is the default.
FROM runtime AS worker
ENTRYPOINT ["/usr/local/bin/hintr_worker"]

FROM runtime AS server
ENTRYPOINT ["/usr/local/bin/hintr_api"]
