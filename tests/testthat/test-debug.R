test_that("download_debug prevents overwriting", {
  tmp <- tempfile()
  id <- "abc"
  dir.create(file.path(tmp, id), FALSE, TRUE)
  expect_error(
    download_debug(id, dest = tmp),
    "Path 'abc' already exists at destination")
})
