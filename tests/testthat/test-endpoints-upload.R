test_that("can upload input files", {
  inputs_dir <- tempfile()
  dir.create(inputs_dir)
  q <- Queue$new(workers = 0, inputs_dir = inputs_dir)

  file <- "col1,col2\nval1,val2"
  endpoint <- endpoint_upload_input(q)
  res <- endpoint$run(charToRaw(file), "survey_data.csv")

  expect_equal(res$status_code, 200)
  expect_null(res$errors)
  expect_equal(res$data$filename, scalar("survey_data.csv"))
  expect_match(res$data$path, paste0(inputs_dir, "/[A-Z0-9]{32}.csv"))

  ## file has been uploaded
  expect_length(list.files(inputs_dir), 1)

  ## Uploading again
  res2 <- endpoint$run(charToRaw(file), "survey_data.csv")

  expect_equal(res, res2)

  ## File has not been uploaded
  expect_length(list.files(inputs_dir), 1)
})


test_that("api can upload input files", {
  inputs_dir <- tempfile()
  dir.create(inputs_dir)
  q <- Queue$new(workers = 0, inputs_dir = inputs_dir)
  api <- api_build(q)

  file <- "col1,col2\nval1,val2"
  res <- api$request("POST", "/internal/upload/input/survey_data.csv",
                     body = charToRaw(file))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_null(body$error)
  expect_equal(body$data$filename, "survey_data.csv")
  expect_match(body$data$path, paste0(inputs_dir, "/[A-Z0-9]{32}.csv"))

  ## file has been uploaded
  expect_length(list.files(inputs_dir), 1)

  ## Uploading again
  res2 <- api$request("POST", "/internal/upload/input/survey_data.csv",
                      body = charToRaw(file))

  expect_equal(res, res2)

  ## File has not been uploaded
  expect_length(list.files(inputs_dir), 1)
})

test_that("can upload output files", {
  results_dir <- tempfile()
  dir.create(results_dir)
  q <- Queue$new(workers = 0, results_dir = results_dir)

  file <- "col1,col2\nval1,val2"
  endpoint <- endpoint_upload_output(q)
  res <- endpoint$run(charToRaw(file), "survey_data.csv")

  expect_equal(res$status_code, 200)
  expect_null(res$errors)
  expect_equal(res$data$filename, scalar("survey_data.csv"))
  expect_match(res$data$path, paste0(results_dir, "/[A-Z0-9]{32}.csv"))

  ## file has been uploaded
  expect_length(list.files(results_dir), 1)

  ## Uploading again
  res2 <- endpoint$run(charToRaw(file), "survey_data.csv")

  expect_equal(res, res2)

  ## File has not been uploaded
  expect_length(list.files(results_dir), 1)
})


test_that("api can upload output files", {
  results_dir <- tempfile()
  dir.create(results_dir)
  q <- Queue$new(workers = 0, results_dir = results_dir)
  api <- api_build(q)

  file <- "col1,col2\nval1,val2"
  res <- api$request("POST", "/internal/upload/result/survey_data.csv",
                     body = charToRaw(file))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_null(body$error)
  expect_equal(body$data$filename, "survey_data.csv")
  expect_match(body$data$path, paste0(results_dir, "/[A-Z0-9]{32}.csv"))

  ## file has been uploaded
  expect_length(list.files(results_dir), 1)

  ## Uploading again
  res2 <- api$request("POST", "/internal/upload/result/survey_data.csv",
                      body = charToRaw(file))

  expect_equal(res, res2)

  ## File has not been uploaded
  expect_length(list.files(results_dir), 1)
})

