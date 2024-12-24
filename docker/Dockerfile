FROM ghcr.io/hivtools/naomi-base:minimal

COPY . /src
COPY docker/Rprofile.site /usr/local/lib/R/etc/Rprofile.site
COPY docker/bin /usr/local/bin/

RUN cd /src \
    && ./scripts/build_test_data \
    && find /src/inst/output -type f ! -name "malawi_model_output.qs" -delete \
    && R CMD INSTALL /src \
    && rm -r /src

EXPOSE 8888
ENV HINTR_QUEUE_ID=hintr

## Model run will try to parallelise over as many threads as are available
## potentially slowing the application, manually limit threads to 1
ENV OMP_NUM_THREADS=1

WORKDIR /

ENTRYPOINT ["/usr/local/bin/hintr_api"]
