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

test_that("can read geojson level labels", {
  shape <- file.path("testdata", "malawi.geojson")
  levels <- read_level_labels(shape)
  expect_length(levels, 5)
  expect_equal(levels, c("Country", "Region", "Zone", "District",
                         "District + Metro"))
})

test_that("can read regions from csv file", {
  programme <- file.path("testdata", "programme.csv")
  regions <- read_csv_regions(programme)
  expect_length(regions, 32)
  expect_equal(regions[1:3], c("MWI.3.4.18.20", "MWI.3.5.23.26",
                               "MWI.3.5.23.29"))
})

test_that("can read iso3 from PJNZ", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  iso3 <- read_pjnz_iso3(pjnz)
  expect_equal(iso3, "BWA")
})

test_that("can read iso3 from geojson", {
  shape <- file.path("testdata", "malawi.geojson")
  iso3 <- read_geojson_iso3(shape)
  expect_equal(iso3, "MWI")
})

test_that("can read country from PJNZ", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  expect_equal(read_country(pjnz), "Botswana")
})
