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
docker run --rm -d --network=host --name hintr mrcide/hintr:latest
```

Test that container is working by using:
```
curl http://localhost:8888
```

Validate input data:
```
curl -X POST -H 'Content-Type: application/json' \
     --data @example/docker_payload.json http://localhost:8888/validate
#> {\"status\":\"success\",\"errors\":{},\"data\":\"Botswana\"}
```

Queue a model run:
```
curl -X POST -H 'Content-Type: application/json' \
     --data @example/docker_model_run_payload.json http://localhost:8888/run_model
#> {"status":"success","errors":{},"data":{"job_id":"e5bdf08b6938ee68ab2c4ee3838c5a05"}}
```

Query model run status:
```
curl -X POST -H 'Content-Type: application/json' \
     --data '{"job_id": "e5bdf08b6938ee68ab2c4ee3838c5a05"}' http://localhost:8888/run_status
```

Result before completion:
```
#> {"status":"success","errors":{},"data":{"job_id":"74ebd6eed130e8123048eff45fa0176c","complete":false,"progress":"50%","timeRemaining":"10s"}}
```

Result after completion:
```
#> {"status":"success","errors":{},"data":{"job_id":"c3308621bf1007c6d9f2f164e13beac0","complete":true,"result":2}}
```

Docker container can be cleaned up using
```
docker rm -f hintr
```

## Validating JSON against schema

To turn on validation of requests and responses you need to set the environmental variable VALIDATE_JSON_SCHEMAS to true. You can do that by writing to a `.Renviron` file, on linux `echo -e "VALIDATE_JSON_SCHEMAS=true" >> .Renviron`.


## Running tests which use redis

To run tests including those which rely on a redis instance being available you need to start a redis docker container
```
docker run --rm -d --network=host --name hintr_redis redis
```
