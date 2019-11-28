context("prerun models")

test_that("import failures", {
  obj <- PrerunModels$new(tempfile())
  p <- tempfile()
  expect_error(obj$import(p), "Import directory .+ does not exist")
  dir.create(p)
  expect_error(obj$import(p),
               "Path 'output.rds' for 'output' does not exist")
  file.create(file.path(p, "output.rds"))
  expect_error(obj$import(p),
               "Path 'spectrum.zip' for 'spectrum' does not exist")
  file.create(file.path(p, "spectrum.zip"))
  expect_error(obj$import(p),
               "Path 'summary.zip' for 'summary' does not exist")
  file.create(file.path(p, "summary.zip"))
  expect_error(obj$import(p, "path/to/output.zip"),
               "Path for 'output' must be just the filename, no slashes")
})


test_that("import base data", {
  tmp <- tempfile()
  obj <- PrerunModels$new(tmp)
  expect_equal(obj$list(), character(0))

  p <- system_file("output")

  inputs <- read_info_inputs(file.path(p, "malawi_summary_download.zip"))
  expect_false(obj$exists(inputs))

  h <- obj$import(p, "malawi_output.rds",
                  "malawi_spectrum_download.zip",
                  "malawi_summary_download.zip")
  expect_equal(obj$list(), h)
  expect_true(obj$exists(inputs))
  expect_equal(obj$get(inputs),
               list(output_path = file.path(tmp, h, "output.rds"),
                    spectrum_path = file.path(tmp, h, "spectrum.zip"),
                    summary_path = file.path(tmp, h, "summary.zip")))
  expect_true(all(vapply(obj$get(inputs), file.exists, TRUE)))
})
