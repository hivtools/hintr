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
## libprotobuf, which the runtime stage finds automatically.
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

## List the system packages the runtime image needs: the packages that own
## the shared libraries which R, the R packages and pandoc link to. This keeps
## up to date as packages are added, rather than maintaining a list by hand.
RUN find /usr/local/lib/R /usr/bin/pandoc -type f \
        \( -name "*.so*" -o -path "*/bin/exec/*" -o -name pandoc \) \
        -exec env LD_LIBRARY_PATH=/usr/local/lib/R/lib ldd {} + 2>/dev/null \
    | awk '$2 == "=>" && $3 ~ /^\// {print $3}' | sort -u \
    | xargs realpath | xargs dpkg -S 2>/dev/null \
    | cut -d: -f1 | sort -u > /runtime-packages.txt \
    && cat /runtime-packages.txt

## A plain Ubuntu base rather than rocker/r-ver, which has compilers, a JDK
## and dev headers for building packages (~700MB we don't need at runtime).
## R itself is copied from the build stage so it always matches the packages.
## Must be the same Ubuntu release as naomi-base for the package names.
FROM ubuntu:26.04 AS runtime

ENV R_HOME=/usr/local/lib/R \
    TZ=Etc/UTC \
    LANG=en_US.UTF-8

## Libraries found above, plus files R uses that aren't linked libraries:
## certificates for https, timezone data, the locale, zip/unzip (R_ZIPCMD,
## R_UNZIPCMD) and fonts. The plots in the reports use the default sans font,
## rocker has Nimbus Sans from fonts-urw-base35 so use it to keep them the same.
RUN --mount=type=bind,from=build,source=/runtime-packages.txt,target=/tmp/runtime-packages.txt \
    apt-get update \
    && apt-get -y install --no-install-recommends \
        $(cat /tmp/runtime-packages.txt) \
        ca-certificates \
        fonts-urw-base35 \
        locales \
        tzdata \
        unzip \
        zip \
    && sed -i 's/^# *en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen \
    && locale-gen \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local/lib/R /usr/local/lib/R
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
