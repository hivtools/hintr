<!-- DO NOT EDIT - EDIT README.md.in INSTEAD! -->
# hintr

<!-- badges: start -->
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build status](https://badge.buildkite.com/c9753af77df495db4ac498034dc51413c343d6837535e36ccb.svg)](https://buildkite.com/mrc-ide/hintr)
[![codecov.io](https://codecov.io/github/mrc-ide/hintr/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/hintr?branch=master)
<!-- badges: end -->

R API for Naomi app

App to show district level estimates of HIV indicators

## Running in docker

Docker images are built on travis, if on master branch run via:
```
docker run --rm -d --network=host --name hintr_redis redis
docker run --rm -d --network=host --mount type=volume,src=upload_volume,dst=/uploads \
  -e USE_MOCK_MODEL=true --name hintr mrcide/hintr:master
```

For a more complete example of running on a network see [docker test script](https://github.com/mrc-ide/hintr/blob/master/docker/test).

Test that container is working by using

```
$ curl http://localhost:8888
```

```json
"Welcome to hintr"
```
Validate PJNZ

```
$ curl -X POST -H 'Content-Type: application/json' \
    --data @example/docker_payload.json \
    http://localhost:8888/validate/baseline-individual
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "hash": "12345",
        "type": "pjnz",
        "data": {
            "country": "Botswana",
            "iso3": "BWA"
        },
        "filename": "Botswana2018.PJNZ",
        "filters": null
    }
}
```
Validate shape file and return serialised data

```
$ curl -X POST -H 'Content-Type: application/json' \
    --data @example/docker_validate_shape_payload.json \
    http://localhost:8888/validate/baseline-individual
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "hash": "12345",
        "type": "shape",
        "data": {
            "type": "FeatureCollection",
            "crs": {
                "type": "name",
                "properties": {
                    "name": "urn:ogc:def:crs:OGC:1.3:CRS84"
                }
            },
            "features": [
                {
                    "type": "Feature",
                    "properties": {
... truncated 144134 lines of output
```
Validate population data

```
$ curl -X POST -H 'Content-Type: application/json' \
    --data \
    @example/docker_validate_population_payload.json \
    http://localhost:8888/validate/baseline-individual
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "hash": "12345",
        "type": "population",
        "data": null,
        "filename": "population.csv",
        "filters": null
    }
}
```
Validate baseline data

```
$ curl -X POST -H 'Content-Type: application/json' \
    --data \
    @example/docker_validate_baseline_payload.json \
    http://localhost:8888/validate/baseline-combined
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "consistent": true
    }
}
```
Validate programme ART data

```
$ curl -X POST -H 'Content-Type: application/json' \
    --data \
    @example/docker_validate_programme_payload.json \
    http://localhost:8888/validate/survey-and-programme
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "hash": "12345",
        "type": "programme",
        "data": [
            {
                "area_id": "MWI_4_20",
                "sex": "both",
                "age_group": "00-14",
                "year": 2011,
                "current_art": 464.1
            },
            {
                "area_id": "MWI_4_20",
                "sex": "both",
                "age_group": "00-14",
... truncated 3615 lines of output
```
Validate ANC data

```
$ curl -X POST -H 'Content-Type: application/json' \
    --data @example/docker_validate_anc_payload.json \
    http://localhost:8888/validate/survey-and-programme
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "hash": "12345",
        "type": "anc",
        "data": [
            {
                "area_id": "MWI_4_1",
                "age_group": "15-49",
                "year": 2011,
                "anc_clients": 4325,
                "ancrt_hiv_status": 2346,
                "ancrt_known_pos": 24,
                "ancrt_already_art": 0,
                "ancrt_tested": 2322,
                "ancrt_test_pos": 50,
                "prevalence": 0.0315,
... truncated 3367 lines of output
```
Validate survey data

```
$ curl -X POST -H 'Content-Type: application/json' \
    --data \
    @example/docker_validate_survey_payload.json \
    http://localhost:8888/validate/survey-and-programme
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "hash": "12345",
        "type": "survey",
        "data": [
            {
                "indicator": "prev",
                "survey_id": "MWI2004DHS",
                "survey_year": 2004,
                "area_id": "MWI",
                "sex": "both",
                "age_group": "15-19",
                "n_cluster": 392,
                "n_obs": 1000,
                "est": 0.021,
                "se": 0.0049,
... truncated 281411 lines of output
```
Get model run options

```
$ curl -X POST -H 'Content-Type: application/json' \
    --data @example/docker_model_run_options.json \
    http://localhost:8888/model/options
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "controlSections": [
            {
                "label": "General",
                "description": "Select general model options:",
                "controlGroups": [
                    {
                        "label": "Area scope",
                        "controls": [
                            {
                                "name": "area_scope",
                                "type": "multiselect",
                                "options": [
                                    {
                                        "id": "MWI",
... truncated 1113 lines of output
```
Run a model

```
$ curl -X POST -H 'Content-Type: application/json' \
    --data @example/model_submit_payload.json \
    http://localhost:8888/model/submit
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "id": "76bed22993c3012ac90347343b8d7f45"
    }
}
```
Query status of model run

```
$ curl http://localhost:8888/model/status/{id}
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "done": false,
        "status": "RUNNING",
        "success": null,
        "queue": 0,
        "progress": [

        ],
        "id": "76bed22993c3012ac90347343b8d7f45"
    }
}
```
Get the result of a model run

```
$ curl http://localhost:8888/model/result/{id}
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "data": [
            {
                "area_id": "MWI",
                "sex": "both",
                "age_group": "15-49",
                "calendar_quarter": "CY2016Q1",
                "indicator_id": 1,
                "mode": 7631061.6527,
                "mean": 7631061.6527,
                "lower": 7631061.6527,
                "upper": 7631061.6527
            },
            {
                "area_id": "MWI",
... truncated 462426 lines of output
```
Headers for summary download

```
$ curl -I http://localhost:8888/download/summary/{id}
```

```json
HTTP/1.1 200 OK
Date: Wed, 20 Nov 2019 14:13:30 GMT
Content-Type: application/octet-stream
Date: Wed, 20 Nov 2019 02:13:30 PM GMT
Connection: close
Content-Length: 2552891

```
Get the summary download

```
$ curl http://localhost:8888/download/summary/{id}
```

```json
Hidden 11915 bytes of output
```
Headers for spectrum digest download

```
$ curl -I http://localhost:8888/download/spectrum/{id}
```

```json
HTTP/1.1 200 OK
Date: Wed, 20 Nov 2019 14:13:30 GMT
Content-Type: application/octet-stream
Date: Wed, 20 Nov 2019 02:13:30 PM GMT
Connection: close
Content-Length: 2552891

```
Get the spectrum digest download

```
$ curl http://localhost:8888/download/spectrum/{id}
```

```json
Hidden 11915 bytes of output
```
Get plotting metadata for Malawi

```
$ curl http://localhost:8888/meta/plotting/Malawi
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "anc": {
            "choropleth": {
                "indicators": [
                    {
                        "indicator": "art_coverage",
                        "value_column": "art_coverage",
                        "indicator_column": "",
                        "indicator_value": "",
                        "name": "ART coverage",
                        "min": 0,
                        "max": 1,
                        "colour": "interpolateViridis",
                        "invert_scale": false
                    },
... truncated 247 lines of output
```
Get information about hintr versions

```
$ curl http://localhost:8888/hintr/version
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "hintr": "0.0.15",
        "naomi": "0.0.17",
        "rrq": "0.2.1"
    }
}
```
Get information about hintr's workers

```
$ curl http://localhost:8888/hintr/worker/status
```

```json
{
    "status": "success",
    "errors": [

    ],
    "data": {
        "supersecretive_sanderling_1": "IDLE",
        "supersecretive_sanderling_2": "IDLE"
    }
}
```

Docker container can be cleaned up using
```
docker rm -f hintr hintr_redis
```

### Input data

Input data should be written to the shared `upload_volume`. When requesting validation pass the absolute path to the file in the request JSON e.g.

```
{
  "type": "pjnz",
  "path": "/uploads/Botswana.pjnz"
}
```

## Validating JSON against schema

To turn on validation of requests and responses you need to set the environmental variable VALIDATE_JSON_SCHEMAS to true. You can do that by writing to a `.Renviron` file, on linux `echo -e "VALIDATE_JSON_SCHEMAS=true" >> .Renviron`.


## Running tests

To run tests locally:

1. Install all dependencies with `devtools::install_deps(".")`. You may be prompted to install some operating system
    packages; these should be available via your package manager but for `protoc` you may need the following instructions:
   https://askubuntu.com/questions/1072683/how-can-i-install-protoc-on-ubuntu-16-04
1. Some packages need to be installed from GitHub:
    * `devtools::install_github("ropensci/jsonvalidate")`
    * `devtools::install_github("mrc-ide/eppasm")`
    * `devtools::install_github("mrc-ide/naomi")`
    * `devtools::install_github("mrc-ide/rrq")`
1. Install the hintr package:
   ```
   R CMD INSTALL .
   ```
1. If running all tests, including those that require redis, start a redis docker container
    ```
    docker run --rm -d --network=host --name hintr_redis redis
    ```

Finally tests can be run with `devtools::test()`.
