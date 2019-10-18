context("endpoints-download")

test_that("indicator download returns bytes", {
  test_redis_available()

  res <- MockPlumberResponse$new()

  queue <- Queue$new()
  download_indicators <- endpoint_download_indicators(queue)

  bytes <- download_indicators(NULL, res, "id123")
  expect_type(bytes, "raw")
  expect_length(bytes, file.size(system_file("output", "malawi.zip")))
})

test_that("spectrum download returns bytes", {
  test_redis_available()

  res <- MockPlumberResponse$new()

  queue <- Queue$new()
  download_spectrum <- endpoint_download_spectrum(queue)

  bytes <- download_spectrum(NULL, res, "id123")
  expect_type(bytes, "raw")
  expect_length(bytes, file.size(system_file("output", "malawi.zip")))
})
