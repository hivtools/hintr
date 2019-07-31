context("validate-inputs")

test_that("inputs can be validated and return data", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  expect_true(validate_inputs(pjnz, NULL, NULL, NULL, NULL, NULL))
})

test_that("country can be read from PJNZ file", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  expect_equal(read_country(pjnz), "Botswana")
})
