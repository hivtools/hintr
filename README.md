# hintr

<!-- badges: start -->
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis build status](https://travis-ci.org/mrc-ide/hintr.svg?branch=master)](https://travis-ci.org/mrc-ide/hintr)
[![codecov.io](https://codecov.io/github/mrc-ide/hintr/coverage.svg?branch=master)](https://codecov.io/github/mrc-ide/hintr?branch=master)
<!-- badges: end -->

R API for Naomi app

App to show district level estimates of HIV indicators

## Running in docker

Docker images are built on travis, if on master branch run via

```
docker run --rm -d -p 8888:8888 mrcide/hintr:latest
```

Test that container is working by using
```
curl http://localhost:8888
```

Validate input data:
```
curl -X POST -H 'Content-Type: application/json' \
     --data @example/payload.json http://localhost:8888/validate
```
