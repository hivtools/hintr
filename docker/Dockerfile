FROM mrcide/naomi-base:latest

COPY docker/bin /usr/local/bin/

RUN install_packages --repo=https://inla.r-inla-download.org/R/stable \
        INLA

COPY . /src
RUN R CMD INSTALL /src/naomi
RUN cd /src && ./scripts/build_test_data
RUN R CMD INSTALL /src

EXPOSE 8888
ENV HINTR_QUEUE_ID=hintr

ENTRYPOINT ["/usr/local/bin/hintr_api"]
