setwd("tests/testthat")
devtools::load_all()
library(testthat)

test_redis_available()
test_mock_model_available()

data <- list(
  pjnz = "path/to/pjnz",
  shape = "path",
  population = "path",
  survey = "path",
  programme = "path",
  anc = "path"
)
options = list()
req <- list(postBody = '
              {
              "data": {
              "pjnz": "path/to/file",
              "shape": "path/to/file",
              "population": "path/to/file",
              "survey": "path/to/file",
              "programme": "path/to/file",
              "anc": "path/to/file"
              },
              "options": {}
              }')

## Create mock response
res <- MockPlumberResponse$new()

## Call the endpoint
queue <- Queue$new()
model_submit <- endpoint_model_submit(queue)
response <- model_submit(req, res, data, options)
response <- jsonlite::parse_json(response)
expect_equal(response$status, "success")
expect_true("id" %in% names(response$data))
expect_equal(res$status, 200)

testthat::try_again(4, {
  Sys.sleep(2)
  download_summary <- endpoint_download_summary(queue)
  download <- download_summary(NULL, res, response$data$id)
  expect_type(download$bytes, "raw")
  expect_length(download$bytes, file.size(system_file("output",
                                                      "malawi_summary_download.zip")))
  expect_equal(download$id, response$data$id)
})
