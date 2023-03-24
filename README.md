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

Docker images are built on buildkite, run via
```
docker run --rm -d -p 8888:8888 --name hintr-validation mrcide/hintr-validation:validation-api
```

Test that container is working by using

```
$ curl -s http://localhost:8888
```

```json
{
    "status": "success",
    "errors": null,
    "data": "Welcome to hintr"
}
```

Validate PJNZ

```
$ curl -s -X POST -H 'Content-Type: application/json' \
--data @inst/payload/validate_pjnz_payload.json \
http://localhost:8888/validate/baseline-individual
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "hash": "12345",
        "type": "pjnz",
        "data": {
            "country": "Malawi",
            "iso3": "MWI"
        },
        "filename": "Malawi2019.PJNZ",
        "fromADR": false,
        "resource_url": "https://adr.unaids.org/file/123.csv",
        "filters": null
    }
}
```
Validate shape file and return serialised data

```
$ curl -s -X POST -H 'Content-Type: application/json' \
--data @inst/payload/validate_shape_payload.json \
http://localhost:8888/validate/baseline-individual
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "hash": "12345",
        "type": "shape",
        "data": {
            "type": "FeatureCollection",
            "name": "demo_areas",
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
                        "area_id": "MWI",
... truncated 144125 lines of output
```
Validate population data

```
$ curl -s -X POST -H 'Content-Type: application/json' \
--data @inst/payload/validate_population_payload.json \
http://localhost:8888/validate/baseline-individual
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "hash": "12345",
        "type": "population",
        "data": null,
        "filename": "original.csv",
        "fromADR": false,
        "resource_url": null,
        "filters": null
    }
}
```
Validate baseline data

```
$ curl -s -X POST -H 'Content-Type: application/json' \
--data @inst/payload/validate_baseline_payload.json \
http://localhost:8888/validate/baseline-combined
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "consistent": true
    }
}
```
Validate programme ART data

```
$ curl -s -X POST -H 'Content-Type: application/json' \
--data @inst/payload/validate_programme_payload.json \
http://localhost:8888/validate/survey-and-programme
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "hash": "12345",
        "type": "programme",
        "data": [
            {
                "area_id": "MWI_4_1_demo",
                "area_name": "Chitipa",
                "sex": "both",
                "age_group": "Y000_014",
                "year": 2011,
                "calendar_quarter": "CY2011Q4",
                "art_current": 127,
                "art_new": 9,
                "vl_tested_12mos": null,
                "vl_suppressed_12mos": null
            },
            {
... truncated 6180 lines of output
```
Validate ANC data

```
$ curl -s -X POST -H 'Content-Type: application/json' \
--data @inst/payload/validate_anc_payload.json \
http://localhost:8888/validate/survey-and-programme
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "hash": "12345",
        "type": "anc",
        "data": [
            {
                "area_id": "MWI_4_1_demo",
                "age_group": "Y015_049",
                "year": 2011,
                "anc_clients": 4330,
                "anc_known_pos": 24,
                "anc_already_art": 0,
                "anc_tested": 2105,
                "anc_tested_pos": 50,
                "anc_known_neg": 217,
                "births_facility": 3279,
                "anc_prevalence": 0.0315,
                "anc_art_coverage": 0
... truncated 3626 lines of output
```
Validate survey data

```
$ curl -s -X POST -H 'Content-Type: application/json' \
--data @inst/payload/validate_survey_payload.json \
http://localhost:8888/validate/survey-and-programme
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "hash": "12345",
        "type": "survey",
        "data": [
            {
                "indicator": "prevalence",
                "survey_id": "DEMO2004DHS",
                "survey_mid_calendar_quarter": "CY2004Q4",
                "area_id": "MWI",
                "area_name": "Malawi - Demo",
                "res_type": "all",
                "sex": "both",
                "age_group": "Y015_049",
                "n_clusters": 512,
                "n_observations": 5136,
                "n_eff_kish": 3125.6878,
                "estimate": 0.1183,
... truncated 367974 lines of output
```
