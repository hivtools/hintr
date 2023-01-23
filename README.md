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
Get model run options

```
$ curl -s -X POST -H 'Content-Type: application/json' \
--data @inst/payload/model_run_options_payload.json \
http://localhost:8888/model/options
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "controlSections": [
            {
                "label": "General",
                "description": "Select general model options:",
                "controlGroups": [
                    {
                        "label": "Trigger mock model error",
                        "controls": [
                            {
                                "name": "mock_model_trigger_error",
                                "type": "select",
                                "required": true,
                                "helpText": "Set TRUE to force the model fit to error",
                                "options": [
                                    {
                                        "id": "true",
... truncated 1826 lines of output
```
Run a model

```
$ curl -s --data '{
    "data": {
        "pjnz": {
            "path": "testdata/Malawi2019.PJNZ",
            "filename": "Malawi2019.PJNZ",
            "hash": "12345",
            "fromADR": false
        },
        "shape": {
            "path": "testdata/malawi.geojson",
            "filename": "malawi.geojson",
            "hash": "12345",
            "fromADR": false
        },
        "population": {
            "path": "testdata/population.csv",
            "filename": "population.csv",
            "hash": "12345",
            "fromADR": false
        },
        "survey": {
            "path": "testdata/survey.csv",
            "filename": "survey.csv",
            "hash": "12345",
            "fromADR": false
        },
        "programme": {
            "path": "testdata/programme.csv",
            "filename": "programme.csv",
            "hash": "12345",
            "fromADR": false
        },
        "anc": {
            "path": "testdata/anc.csv",
            "filename": "anc.csv",
            "hash": "12345",
            "fromADR": false
        }
    },
    "options": {
        "area_scope": "MWI",
        "area_level": 4,
        "calendar_quarter_t1": "CY2016Q1",
        "calendar_quarter_t2": "CY2018Q3",
        "calendar_quarter_t3": "CY2019Q2",
        "survey_prevalence": [
            "DEMO2016PHIA",
            "DEMO2015DHS"
        ],
        "survey_art_coverage": "DEMO2016PHIA",
        "survey_recently_infected": "DEMO2016PHIA",
        "include_art_t1": "true",
        "include_art_t2": "true",
        "anc_clients_year2": 2018,
        "anc_clients_year2_num_months": "9",
        "anc_prevalence_year1": 2016,
        "anc_prevalence_year2": 2018,
        "anc_art_coverage_year1": 2016,
        "anc_art_coverage_year2": 2018,
        "spectrum_population_calibration": "none",
        "spectrum_plhiv_calibration_level": "none",
        "spectrum_plhiv_calibration_strat": "sex_age_coarse",
        "spectrum_artnum_calibration_level": "none",
        "spectrum_artnum_calibration_strat": "sex_age_coarse",
        "spectrum_infections_calibration_level": "none",
        "spectrum_infections_calibration_strat": "sex_age_coarse",
        "spectrum_aware_calibration_level": "none",
        "spectrum_aware_calibration_strat": "sex_age_coarse",
        "calibrate_method": "logistic",
        "artattend_log_gamma_offset": -4,
        "artattend": false,
        "output_aware_plhiv": "true",
        "rng_seed": 17,
        "no_of_samples": 500,
        "max_iter": 250
    },
    "version": {
        "hintr": "1.1.9",
        "naomi": "2.8.12",
        "rrq": "0.5.7",
        "traduire": "0.0.6"
    }
}' \
-X POST -H 'Content-Type: application/json' \
http://localhost:8888/model/submit
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "id": "957800d3055d679195ffe18aaf3391ab"
    }
}
```
Query status of model run

```
$ curl -s http://localhost:8888/model/status/957800d3055d679195ffe18aaf3391ab
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "done": false,
        "status": "RUNNING",
        "success": null,
        "queue": 0,
        "progress": [

        ],
        "id": "957800d3055d679195ffe18aaf3391ab"
    }
}
```
Get the result of a model run

```
$ curl -s http://localhost:8888/model/result/957800d3055d679195ffe18aaf3391ab
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "id": "957800d3055d679195ffe18aaf3391ab",
        "complete": true,
        "warnings": [
            {
                "text": "Zero population input for 8 population groups. Replaced with population 0.1.",
                "locations": [
                    "model_fit"
                ]
            }
        ]
    }
}
```
Calibrate a model

```
$ curl -s --data '{
    "options": {
        "spectrum_plhiv_calibration_level": "national",
        "spectrum_plhiv_calibration_strat": "sex_age_group",
        "spectrum_artnum_calibration_level": "national",
        "spectrum_artnum_calibration_strat": "sex_age_coarse",
        "spectrum_infections_calibration_level": "national",
        "spectrum_infections_calibration_strat": "sex_age_coarse",
        "spectrum_aware_calibration_level": "national",
        "spectrum_aware_calibration_strat": "sex_age_coarse",
        "calibrate_method": "logistic"
    },
    "version": {
        "hintr": "1.1.9",
        "naomi": "2.8.12",
        "rrq": "0.5.7",
        "traduire": "0.0.6"
    }
}' \
-X POST -H 'Content-Type: application/json' \
http://localhost:8888/calibrate/submit/957800d3055d679195ffe18aaf3391ab
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "id": "d5d08f4783975d8092e049adec43726b"
    }
}
```
Query status of calibrate run

```
$ curl -s http://localhost:8888/calibrate/status/d5d08f4783975d8092e049adec43726b
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "done": false,
        "status": "RUNNING",
        "success": null,
        "queue": 0,
        "progress": [

        ],
        "id": "d5d08f4783975d8092e049adec43726b"
    }
}
```
Get the result of a calibrate run

```
$ curl -s http://localhost:8888/calibrate/result/d5d08f4783975d8092e049adec43726b
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "data": [
            {
                "area_id": "MWI",
                "sex": "both",
                "age_group": "Y015_049",
                "calendar_quarter": "CY2016Q1",
                "indicator": "population",
                "mode": 7631061.6527,
                "mean": 7631061.6527,
                "lower": 7631061.6527,
                "upper": 7631061.6527
            },
            {
                "area_id": "MWI",
                "sex": "both",
                "age_group": "Y015_064",
... truncated 3403007 lines of output
```
Initialise download generation, type spectrum, coarse_output, summary or comparison

```
$ curl -s --data '{
    "notes": {
        "project_notes": {
            "name": "My project 123",
            "updated": "2022/05/17 12:34:21",
            "note": "These are my project notes"
        },
        "version_notes": [
            {
                "name": "Version 2",
                "updated": "2022/05/17 12:34:21",
                "note": "Notes specific to this version"
            },
            {
                "name": "Version 1",
                "updated": "2022/05/14 09:12:54",
                "note": "Notes from the first version"
            }
        ]
    },
    "state": {
        "datasets": {
            "pjnz": {
                "path": "72A9B1F58AAA743ADA64C6AE985CF228.pjnz",
                "filename": "demo_mwi2019.pjnz"
            },
            "population": {
                "path": "651105353D7153381ED29363DF0D772F.csv",
                "filename": "demo_population_agesex.csv"
            },
            "shape": {
                "path": "EBE533976BFAF0CABCA2C2E1B611B9C7.geojson",
                "filename": "demo_areas.geojson"
            },
            "survey": {
                "path": "F669CA9AA38A3993B5A9E9D3EB717C7D.csv",
                "filename": "demo_survey_hiv_indicators.csv"
            },
            "programme": {
                "path": "8301300AB39BE177FA593571B9DD94C4.csv",
                "filename": "demo_art_number.csv"
            },
            "anc": {
                "path": "E6323AAEB045D31E4A267398F669CF20.csv",
                "filename": "demo_anc_testing.csv"
            }
        },
        "model_fit": {
            "options": {
                "area_scope": "MWI",
                "area_level": 4,
                "calendar_quarter_t1": "CY2016Q1",
                "calendar_quarter_t2": "CY2018Q3",
                "calendar_quarter_t3": "CY2019Q2",
                "survey_prevalence": [
                    "DEMO2016PHIA",
                    "DEMO2015DHS"
                ],
                "survey_art_coverage": "DEMO2016PHIA",
                "survey_recently_infected": "DEMO2016PHIA",
                "include_art_t1": "true",
                "include_art_t2": "true",
                "anc_clients_year2": 2018,
                "anc_clients_year2_num_months": "9",
                "anc_prevalence_year1": 2016,
                "anc_prevalence_year2": 2018,
                "anc_art_coverage_year1": 2016,
                "anc_art_coverage_year2": 2018,
                "spectrum_population_calibration": "none",
                "spectrum_plhiv_calibration_level": "none",
                "spectrum_plhiv_calibration_strat": "sex_age_coarse",
                "spectrum_artnum_calibration_level": "none",
                "spectrum_artnum_calibration_strat": "sex_age_coarse",
                "spectrum_infections_calibration_level": "none",
                "spectrum_infections_calibration_strat": "sex_age_coarse",
                "spectrum_aware_calibration_level": "none",
                "spectrum_aware_calibration_strat": "sex_age_coarse",
                "calibrate_method": "logistic",
                "artattend_log_gamma_offset": -4,
                "artattend": false,
                "output_aware_plhiv": "true",
                "rng_seed": 17,
                "no_of_samples": 500,
                "max_iter": 250
            },
            "id": "17d40b32f8e649349e047561a6831144"
        },
        "calibrate": {
            "options": {
                "spectrum_plhiv_calibration_level": "national",
                "spectrum_plhiv_calibration_strat": "sex_age_group",
                "spectrum_artnum_calibration_level": "national",
                "spectrum_artnum_calibration_strat": "sex_age_coarse",
                "spectrum_infections_calibration_level": "national",
                "spectrum_infections_calibration_strat": "sex_age_coarse",
                "spectrum_aware_calibration_level": "national",
                "spectrum_aware_calibration_strat": "sex_age_coarse",
                "calibrate_method": "logistic"
            },
            "id": "6e457f5a9f0413708624b7b0384e5fd0"
        },
        "version": {
            "hintr": "1.1.9",
            "naomi": "2.8.12",
            "rrq": "0.5.7",
            "traduire": "0.0.6"
        }
    }
}' \
-X POST -H 'Content-Type: application/json' \
http://localhost:8888/download/submit/spectrum/d5d08f4783975d8092e049adec43726b
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "id": "930202bce97ebe49df170f0e59512357"
    }
}
```
Query status of download generation

```
$ curl -s http://localhost:8888/download/status/930202bce97ebe49df170f0e59512357
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "done": false,
        "status": "RUNNING",
        "success": null,
        "queue": 0,
        "progress": [

        ],
        "id": "930202bce97ebe49df170f0e59512357"
    }
}
```
Headers for summary download result

```
$ curl -s -I http://localhost:8888/download/result/930202bce97ebe49df170f0e59512357
```

```json
HTTP/1.1 200 OK
Date: Mon, 23 Jan 2023 18:44:36 GMT
Content-Type: application/octet-stream
Content-Disposition: attachment; filename="MWI_naomi-output_20230123-1844.zip"
X-Porcelain-Validated: false
Content-Length: 18810039

```
Get the summary download result

```
$ curl -s http://localhost:8888/download/result/930202bce97ebe49df170f0e59512357
```

```json
Hidden 74802 bytes of output
```
Get plotting metadata for Malawi

```
$ curl -s http://localhost:8888/meta/plotting/Malawi
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "anc": {
            "choropleth": {
                "indicators": [
                    {
                        "indicator": "anc_prevalence",
                        "value_column": "anc_prevalence",
                        "indicator_column": "",
                        "indicator_value": "",
                        "name": "ANC HIV prevalence",
                        "min": 0,
                        "max": 0.5,
                        "colour": "interpolateOranges",
                        "invert_scale": false,
                        "scale": 1,
                        "accuracy": null,
                        "format": "0.0%"
... truncated 798 lines of output
```
Get information about hintr versions

```
$ curl -s http://localhost:8888/hintr/version
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "hintr": "1.1.9",
        "naomi": "2.8.12",
        "rrq": "0.5.7",
        "traduire": "0.0.6"
    }
}
```
Get information about hintr's workers

```
$ curl -s http://localhost:8888/hintr/worker/status
```

```json
{
    "status": "success",
    "errors": null,
    "data": {
        "capsizable_pipit_1": "IDLE",
        "capsizable_pipit_2": "IDLE"
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

## Using sensitive data

To run tests which use sensitive data you need to clone the private [naomi-data repo](https://github.com/mrc-ide/naomi-data) into `tests/testthat/testdata/sensitive`.

```
git clone git@github.com:mrc-ide/naomi-data.git tests/testthat/testdata/sensitive
```

## Adding prerun model results

Details here will depend on the deploy (and that will be the place to look for the running version).

Use `hintr::prerun_push`, specifying the *relative* filenames of the output, spectrum and summary files.

First, run a model using `naomi::hintr_run_model` into some directory, say `mydir`

Then import the data into the *production copy of naomi* with

```
hintr::prerun_push("mydir",
                   output = "malawi_output.qs",
                   spectrum = "malawi_spectrum_download.zip",
                   summary = "malawi_summary_download.zip")
```

Make a note of the hash that is returned - you'll need that if you want to delete the data.

**You must be on the VPN for this to work.**
