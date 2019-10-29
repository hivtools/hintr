context("endpoints-download")

test_that("indicator download returns bytes", {
  test_redis_available()

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

  Sys.sleep(5)
  download_summary <- endpoint_download_summary(queue)
  bytes <- download_summary(NULL, res, response$data$id)
  expect_type(bytes, "raw")
  expect_length(bytes,
                file.size(file.path("testdata", "malawi_summary_download.zip")))
})

test_that("spectrum download returns bytes", {
  test_redis_available()

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

  Sys.sleep(5)
  download_spectrum <- endpoint_download_spectrum(queue)
  bytes <- download_spectrum(NULL, res, response$data$id)
  expect_type(bytes, "raw")
  expect_length(bytes,
                file.size(file.path("testdata", "malawi_spectrum_download.zip")))
})
