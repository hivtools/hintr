context("validate-inputs")

test_that("baseline inputs can be validated and return data", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  ## TODO: Expand validation to include other input files
  expect_equal(do_validate_baseline(pjnz, NULL, NULL), "Botswana")
})

test_that("country can be read from PJNZ file", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  expect_equal(read_country(pjnz), "Botswana")
})
