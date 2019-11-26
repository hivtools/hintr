### Test data

Most test data is coming from naomi repo. https://github.com/mrc-ide/naomi

It is in the format that we are expecting users to upload it to the front end.

In particular we have

* PJNZ - Malawi 2019 and Botswana 2018 supplied by Jeff
* malawi.geojson - Built from boundary data, hierarchy info and  level meta data from https://github.com/mrc-ide/naomi/tree/master/inst/extdata/areas using code from vignette https://github.com/mrc-ide/naomi/blob/master/vignettes/model-workflow.R#L45
* malawi_missing_regions.geojson - The Malawi data but subset to only include some regions, used for testing validation errors
* population.csv - Direct from naomi repo test data https://github.com/mrc-ide/naomi/blob/master/inst/extdata/population/population_agesex.csv
* programme.csv - Direct from naomi repo https://github.com/mrc-ide/naomi/blob/master/inst/extdata/programme/art_number.csv
* malformed_programme.csv - programme.csv edited to cause a validation error
* anc.csv - Direct from naomi repo https://github.com/mrc-ide/naomi/blob/master/inst/extdata/programme/anc_testing.csv
* malformed_anc.csv - anc.csv edited to cause a validation error
* survey.csv - Direct from naomi repo https://github.com/mrc-ide/naomi/blob/master/inst/extdata/survey/survey_hiv_indicators.csv
* malformed_survey.csv - survey.csv edited to cause a validation error

### Updates

Any updates to the form of the expected upload data will be dictated by Jeff and the ADR (AIDS data repository). What we want to move towards is an integration between data existing in the ADR and Naomi, so users can select what ADR data they want to include in a run and it will be made a available without any manual uploading of data. For the time being this is a manual process (download from ADR and upload to Naomi). There are some known changes which will be coming:

* subnational PJNZ files - we're expecting to open up the PJNZ upload to also accept a zip of PJNZ files. Or to take multiple uploads. This is for countries who have moved to subnational files.

