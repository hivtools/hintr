# hintr

<!-- badges: start -->
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis build status](https://travis-ci.org/mrc-ide/hintr.svg?branch=master)](https://travis-ci.org/mrc-ide/hintr)
[![codecov.io](https://codecov.io/github/mrc-ide/hintr/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/hintr?branch=master)
<!-- badges: end -->

R API for Naomi app

App to show district level estimates of HIV indicators

## Running in docker

Docker images are built on travis, if on master branch run via:
```
docker run --rm -d --network=host --name hintr_redis redis
docker run --rm -d --network=host --mount type=volume,src=upload_volume,dst=/uploads \
  --name hintr mrcide/hintr:latest
```

Test that container is working by using:
```
curl http://localhost:8888
```

Validate PJNZ:
``` 
curl -X POST -H 'Content-Type: application/json' \
     --data @example/docker_payload.json http://localhost:8888/validate
#> {"status":"success","errors":{},"data":{"filename":"Botswana2018.PJNZ","type":"pjnz","data":{"country":"Botswana"}}}
```

Validate shape file and return serialised data:
``` 
curl -X POST -H 'Content-Type: application/json' \
     --data @example/docker_validate_shape_payload.json http://localhost:8888/validate
```

Validate population data:
```
curl -X POST -H 'Content-Type: application/json' \
     --data @example/docker_validate_population_payload.json http://localhost:8888/validate
#> {"status":"success","errors":{},"data":{"filename":"population.csv","type":"population","data":null}
```

Validate baseline data:
```
curl -X POST -H 'Content-Type: application/json' \
     --data @example/docker_validate_baseline_payload.json http://localhost:8888/validate_baseline
#> {"status":"success","errors":[],"data":{"complete":true,"consistent":true}}
```

Validate programme ART data:
```
curl -X POST -H 'Content-Type: application/json' \
     --data @example/docker_validate_programme_payload.json http://localhost:8888/validate
```

Validate ANC data:
```
curl -X POST -H 'Content-Type: application/json' \
     --data @example/docker_validate_anc_payload.json http://localhost:8888/validate
```

Validate survey data:
```
curl -X POST -H 'Content-Type: application/json' \
     --data @example/docker_validate_survey_payload.json http://localhost:8888/validate
```

Run a model:
```
curl -X POST -H 'Content-Type: application/json' \
     --data @example/model_submit_payload.json http://localhost:8888/model/submit
#> {"status":"success","errors":{},"data":{"id":"4d99b972cdcbebc96835c102857a808c"}}
```

Query status of model run:
```
curl http://localhost:8888/model/status/4d99b972cdcbebc96835c102857a808c
```

Get the result of a model run:
```
curl http://localhost:8888/model/result/4d99b972cdcbebc96835c102857a808c
```

Docker container can be cleaned up using
```
docker rm -f hintr
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
1. Install the latest version of `jsonvalidate` from GitHub with
     `devtools::install_github("ropensci/jsonvalidate")`
1. Install the hintr package:
   ```
   R CMD INSTALL .
   ```
1. If running all tests, including those that require redis, start a redis docker container
    ```
    docker run --rm -d --network=host --name hintr_redis redis
    ```

Finally tests can be run with `devtools::test()`.
