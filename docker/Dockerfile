FROM mrcide/naomi-base:latest

RUN apt-get update && apt-get -y install --no-install-recommends \
        libhiredis-dev \
        libjq-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

RUN install_packages --repo=https://mrc-ide.github.io/drat \
        callr \
        docopt \
        geojsonio \
        glue \
        jsonlite \
        methods \
        plumber \
        ps \
        remotes \
        rlang

COPY docker/bin /usr/local/bin/

RUN install_remote \
        mrc-ide/rrq \
        reside-ic/traduire \
        ropensci/jsonvalidate

RUN Rscript -e "remotes::install_github('cran/httpuv@v1.6.1', upgrade = FALSE)"

COPY . /src
RUN R CMD INSTALL /src/naomi
RUN cd /src && ./scripts/build_test_data
RUN R CMD INSTALL /src

EXPOSE 8888
ENV HINTR_QUEUE_ID=hintr
## Model run will try to parallelise over as many threads as are available
## potentially slowing the application, manually limit threads to 1
ENV OMP_NUM_THREADS=1

ENTRYPOINT ["/usr/local/bin/hintr_api"]
