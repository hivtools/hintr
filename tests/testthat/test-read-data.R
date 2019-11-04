context("read-data")

test_that("geojson read applies geojson class", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  json <- hintr_geojson_read(shape)
  expect_s3_class(json, "geojson")
})

test_that("can read geojson regions", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  regions <- read_geojson_regions(shape)
  expect_length(regions, 69)
  expect_equal(regions[1:5], c("MWI", "MWI.1", "MWI.2", "MWI.3", "MWI.1.1"))
})

test_that("can read regions from csv file", {
  programme <- file_object(file.path("testdata", "programme.csv"))
  regions <- read_csv_regions(programme)
  expect_length(regions, 32)
  expect_equal(regions[1:3], c("MWI.3.4.18.20", "MWI.3.5.23.26",
                               "MWI.3.5.23.29"))
})

test_that("can read iso3 from PJNZ", {
  pjnz <- file_object(file.path("testdata", "Botswana2018.PJNZ"))
  iso3 <- read_pjnz_iso3(pjnz)
  expect_equal(iso3, "BWA")
})

test_that("can read iso3 from geojson", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  iso3 <- read_geojson_iso3(shape)
  expect_equal(iso3, "MWI")
})

test_that("can read country from PJNZ", {
  pjnz <- file_object(file.path("testdata", "Botswana2018.PJNZ"))
  expect_equal(read_country(pjnz), "Botswana")
})

test_that("geojson read applies can use cache", {
  cache <- new_cache()
  path <- file_object(file.path("testdata", "malawi.geojson"))
  value <- hintr_geojson_read(path, cache)
  expect_equal(cache$list("geojson"), path$hash)
  expect_identical(hintr_geojson_read(path, cache), value)
})
