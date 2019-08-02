FROM rocker/r-ver:3.6.0

RUN apt-get update && apt-get -y install \
        libcurl4-openssl-dev \
        libhiredis-dev \
        libssl-dev \
        libxml2-dev \
        zlib1g-dev

COPY docker/bin /usr/local/bin/

RUN install_packages --repo=https://mrc-ide.github.io/drat \
        jsonlite \
        plumber \
        ps \
        rrq \
        testthat

RUN install_github_packages \
        mrc-ide/specio

COPY . /src
RUN R CMD INSTALL /src

ENTRYPOINT ["/src/inst/entrypoint.R"]
