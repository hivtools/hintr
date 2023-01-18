FROM mrcide/naomi-base:latest

RUN apt-get update && apt-get -y install --no-install-recommends \
        libhiredis-dev \
        libjq-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

RUN install_packages --repo=https://mrc-ide.r-universe.dev \
        callr \
        docopt \
        geojsonio \
        glue \
        jsonlite \
        jsonvalidate \
        lgr \
        mockr \
        naomi.options \
        plumber \
        porcelain \
        ps \
        remotes \
        rlang \
        rrq \
        testthat.buildkite

COPY docker/bin /usr/local/bin/

# If tracking development versions of packages, install via
# github. However, limt this (and prefer updating the drat) as this
# can hit rate limits as we cannot use a PAT when building the
# container.
RUN install_remote \
    mrc-ide/naomi.options

COPY . /src
RUN R CMD INSTALL /src/naomi \
    && cd /src && ./scripts/build_test_data \
    && R CMD INSTALL /src

EXPOSE 8888
ENV HINTR_QUEUE_ID=hintr
## Model run will try to parallelise over as many threads as are available
## potentially slowing the application, manually limit threads to 1
ENV OMP_NUM_THREADS=1

ENTRYPOINT ["/usr/local/bin/hintr_api"]
