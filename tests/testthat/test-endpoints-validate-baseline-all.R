test_that("endpoint_validate_baseline_combined correctly validates data", {
  input <- validate_baseline_all_input(file.path("testdata", "Malawi2019.PJNZ"),
                                       file.path("testdata", "malawi.geojson"),
                                       file.path("testdata", "population.csv"))
  response <- validate_baseline_combined(input)
  expect_equal(response$consistent, scalar(TRUE))
})

test_that("endpoint_validate_baseline_combined returns error on invalid data", {
  input <- validate_baseline_all_input(
    file.path("testdata", "Botswana2018.PJNZ"),
    file.path("testdata", "malawi.geojson"),
    file.path("testdata", "population.csv"))
  error <- expect_error(validate_baseline_combined(input))
  expect_equal(error$data[[1]]$error, scalar("INVALID_BASELINE"))
  expect_equal(
    error$data[[1]]$detail,
    scalar("Countries aren't consistent got BWA from pjnz and MWI from shape."))
  expect_equal(error$status_code, 400)
})

test_that("validation works with muliple PJNZ files", {
  skip_if_sensitive_data_missing()
  input <- validate_baseline_all_input(
    file.path("testdata", "sensitive", "ZMB", "data",
              "zmb_all_pjnz_extract.zip"),
    file.path("testdata", "sensitive", "ZMB", "data",
              "zmb_areas.geojson"),
    NULL)
  response <- validate_baseline_combined(input)
  expect_equal(response$consistent, scalar(TRUE))
})

test_that("validation errors if spectrum region codes mismatch", {
  skip_if_sensitive_data_missing()
  input <- validate_baseline_all_input(
    file.path("testdata", "sensitive", "ZMB", "data",
              "zmb_all_pjnz_extract.zip"),
    file.path("testdata", "malawi.geojson"),
    NULL)
  mockery::stub(do_validate_baseline, "read_iso3", "ZMB", depth = 2)
  error <- expect_error(validate_baseline_combined(input))
  expect_equal(error$data[[1]]$error, scalar("INVALID_BASELINE"))
  expect_match(error$data[[1]]$detail, paste0(
    "Different spectrum region codes in PJNZ and shape file.\n",
    "10 codes in PJNZ missing from shape file: 10, 11, 12, 13, 14, 15, 16, 17,",
    " 18, 19\n", "1 code in shape file missing from PJNZ: 0"))
  expect_equal(error$status_code, 400)
})

