context("validate-inputs")

test_that("baseline inputs can be validated and return data", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  ## TODO: Expand validation to include other input files
  expect_equal(do_validate_pjnz(pjnz), "Botswana")

  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    expect_error(do_validate_pjnz(pjnz), "Invalid country")
  })
})

test_that("country can be read from PJNZ file", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  expect_equal(read_country(pjnz), "Botswana")
})
