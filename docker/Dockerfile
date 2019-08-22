FROM rocker/r-ver:3.6.0

RUN apt-get update && apt-get -y install \
        libcurl4-openssl-dev \
        libhiredis-dev \
        libssl-dev \
        libjq-dev \
        libudunits2-dev \
        libxml2-dev \
        zlib1g-dev

COPY docker/bin /usr/local/bin/

RUN install_packages --repo=https://mrc-ide.github.io/drat \
        jsonlite \
        plumber \
        ps \
        remotes \
        rrq \
        specio \
        testthat

COPY . /src
RUN R CMD INSTALL /src

EXPOSE 8888

ENTRYPOINT ["/src/inst/entrypoint.R"]
