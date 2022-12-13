test_that("can upload input files", {
  uploads_dir <- tempfile()
  dir.create(uploads_dir)
  q <- Queue$new(workers = 0, uploads_dir = uploads_dir)

  file <- "col1,col2\nval1,val2"
  endpoint <- endpoint_upload_input(q)
  res <- endpoint$run(charToRaw(file), "survey_data.csv")

  expect_equal(res$status_code, 200)
  expect_null(res$error)
  expect_equal(res$data$filename, scalar("survey_data.csv"))
  expect_match(res$data$hash, "[A-Z0-9]{32}")
  expect_match(res$data$path, paste0(uploads_dir, "/[A-Z0-9]{32}.csv"))

  ## file has been uploaded
  expect_length(list.files(uploads_dir), 1)

  ## Uploading again
  res2 <- endpoint$run(charToRaw(file), "survey_data.csv")

  expect_equal(res, res2)

  ## File has not been uploaded
  expect_length(list.files(uploads_dir), 1)
})


test_that("api can upload input files", {
  uploads_dir <- tempfile()
  dir.create(uploads_dir)
  q <- Queue$new(workers = 0, uploads_dir = uploads_dir)
  api <- api_build(q)

  file <- "col1,col2\nval1,val2"
  res <- api$request("POST", "/upload/input/survey_data.csv",
                     body = charToRaw(file))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_null(body$error)
  expect_equal(body$data$filename, "survey_data.csv")
  expect_match(body$data$hash, "[A-Z0-9]{32}")
  expect_match(body$data$path, paste0(uploads_dir, "/[A-Z0-9]{32}.csv"))

  ## file has been uploaded
  expect_length(list.files(uploads_dir), 1)

  ## Uploading again
  res2 <- api$request("POST", "/upload/input/survey_data.csv",
                      body = charToRaw(file))

  expect_equal(res, res2)

  ## File has not been uploaded
  expect_length(list.files(uploads_dir), 1)
})
