context("read-data")

test_that("geojson read applies geojson class", {
  path <- file.path("testdata", "malawi.geojson")
  json <- hintr_geojson_read(path)
  expect_s3_class(json, "geojson")
})

test_that("can read surveys", {
  survey_path <- file.path("testdata", "survey.csv")
  surveys <- read_surveys(survey_path)
  expect_equal(surveys,
               c("MWI2016PHIA", "MWI2015DHS", "MWI2010DHS", "MWI2004DHS"))
})

test_that("can read quarters", {
  programme_path <- file.path("testdata", "programme.csv")
  programme_quarters <- read_quarters(programme_path)
  expect_equal(programme_quarters, seq.int(445, 476))

  anc_path <- file.path("testdata", "anc.csv")
  anc_quarters <- read_quarters(anc_path)
  expect_equal(anc_quarters, seq.int(447, 475))
})

test_that("can read geojson regions", {
  shape <- file.path("testdata", "malawi.geojson")
  regions <- read_geojson_regions(shape)
  expect_length(regions, 69)
  expect_equal(regions[1:5], c("MWI", "MWI.1", "MWI.2", "MWI.3", "MWI.1.1"))
})

