FROM rocker/r-ver:3.6.0

RUN apt-get update && apt-get -y install \
        libcurl4-openssl-dev \
        libgdal-dev \
        libhiredis-dev \
        libjq-dev \
        libprotobuf-dev \
        libssl-dev \
        libudunits2-dev \
        libv8-dev \
        libxml2-dev \
        protobuf-compiler \
        zlib1g-dev

COPY docker/bin /usr/local/bin/

RUN install_packages --repo=https://mrc-ide.github.io/drat \
        docopt \
        glue \
        geojsonio \
        jsonlite \
        plumber \
        ps \
        remotes \
        specio \
        testthat

RUN install_packages --repo=https://inla.r-inla-download.org/R/stable \
        INLA

RUN install_remote \
        mrc-ide/eppasm \
        mrc-ide/naomi@mrc-570 \
        mrc-ide/rrq

COPY . /src
RUN R CMD INSTALL /src

EXPOSE 8888
ENV HINTR_QUEUE_ID=hintr

ENTRYPOINT ["/usr/local/bin/hintr_api"]
