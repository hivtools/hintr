# hintr 0.0.36

* Return uncertainty range metadata from model result for choropleth

# hintr 0.0.34

* Accept semicolon delimited data as csv inputs (common in countries with non-Anglo number formats)

# hintr 0.0.33

* Language preference is passed through to naomi during model runs.

# hintr 0.0.32

* New debug endpoint for locally getting a copy of failing data.

# hintr 0.0.31

* Extend time options back to Q1 2010 to support household survey datasets back to 2010 (Burkina Faso).

# hintr 0.0.30

* Remove "survey_year" from required columns in survey datasets.

# hintr 0.0.28

* Add default values for area level and calendar quarter to generate estimates for

# hintr 0.0.27

* Support for stopping jobs (mrc-732)

# hintr 0.0.25

* Add receiving_art output indicator

# hintr 0.0.24

* Return better error message from geojson reading

# hintr 0.0.23

* No longer require ancrt_hiv_status
* Internationalisation support (mrc-788)

# hintr 0.0.22

* Update for changes to model options in naomi v0.0.21
* Support prerun model results (mrc-1155)

# hintr 0.0.21

* Errors in the hint model run are returned with stack traces (mrc-714)

# hintr 0.0.20

* Accept and validate zip of PJNZ files

# hintr 0.0.19

* Return version info from model run options response
* Require version info in model run submit endpoint
* Update filter ordering and naming

# hintr 0.0.18

* Add model options validate endpoint
* Update input data structures including
   * Move quarter_id to use calendar_quarter instead in format CY2016Q3 for population and output data
   * Move quarter_id to use year for ANC, ART, 
   * Move age_group_id to age_group format e.g. 00-04, 15+, 05-09 etc. in ANC, ART, population, programme, survey and output data
   * Update model run options
   * Update region IDS to be formatted as <ISO3>_<area_level>_<area_number> e.g. MWI_2_1 for first region in level 2. Top level region remains as just ISO3 code e.g. MWI
   * Update filters and metadata to reflect data changes
* Add advanced model run options

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
