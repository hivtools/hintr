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
RUN sed  -i'' '/mran.microsoft.com/d' /usr/local/lib/R/etc/Rprofile.site

RUN install_packages --repo=https://mrc-ide.github.io/drat \
        Matrix \
        TMB \
        assertthat \
        data.tree \
        docopt \
        dplyr \
        forcats \
        geojsonio \
        ggplot2 \
        glue \
        gridExtra \
        here \
        jsonlite \
        magrittr \
        methods \
        mvtnorm \
        plumber \
        ps \
        remotes \
        rlang \
        sf \
        spdep \
        specio \
        testthat \
        tidyr \
        zip \
        zoo

RUN install_packages --repo=https://inla.r-inla-download.org/R/stable \
        INLA

RUN install_remote \
        mrc-ide/eppasm \
        mrc-ide/rrq \
        mrc-ide/traduire

COPY . /src
RUN R CMD INSTALL /src/naomi
RUN cd /src && ./scripts/build_test_data
RUN R CMD INSTALL /src

EXPOSE 8888
ENV HINTR_QUEUE_ID=hintr

ENTRYPOINT ["/usr/local/bin/hintr_api"]
