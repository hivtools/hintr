# hintr 0.0.18

* Add model options validate endpoint

# hintr 0.0.17

* Not found (404) responses now conform to the error schema (mrc-596)

# hintr 0.0.16

* Download endpoints return Content-Disposition headers

# hintr 0.0.15

* Add HEAD endpoints for summary and spectrum downloads

# hintr 0.0.14

* Return plotting metadata for barchart with the model result response

# hintr 0.0.13

* Caching enabled for geojson reading

# hintr 0.0.12

* Update model download endpoint from indicators to summary
* Get downloads from model run
* Replace model run code with a mock that returns expected format
* Allow running up hintr docker container with env var USE_MOCK_MODEL to mock a model response

# hintr 0.0.11

* Return iso3 code from PJNZ upload
* Use iso3 to locate plotting metadata instead of country name

# hintr 0.0.10

* Iterate model run option endpoint to return complete options, to always return an ID and label for each option and to return regions as a hierarchy

# hintr 0.0.9

* Add stub endpoints for downloading key indicators and spectrum digest

# hintr 0.0.8

* Add endpoint for retrieving model run options

# hintr 0.0.7

* New `GET /hintr/version` and `GET /hintr/worker/status` endpoints, the first in a series of hintr informational endpoints

# hintr 0.0.6

* Fix plotting metadata for output dataset
* Return indicator filters for input data
* Make input and output data responses more consistent

# hintr 0.0.5

* Add endpoint for retrieving plotting metadata

# hintr 0.0.4

* Rename validate/input endpoint to validate/baseline-individual and rename validate/baseline to validate/baseline-combined for clarity

# hintr 0.0.3

* Validate survey and programme data using a different endpoint from the validation of baseline data

# hintr 0.0.2

* Add baseline validation endpoint and update validation endpoint names

# hintr 0.0.1

* Initialise package
