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
  expect_equal(regions[1:5], c("MWI", "MWI_1_1", "MWI_1_2", "MWI_1_3",
                               "MWI_2_1"))
})

test_that("can read regions from csv file", {
  programme <- file_object(file.path("testdata", "programme.csv"))
  regions <- read_csv_regions(programme)
  expect_length(regions, 32)
  expect_equal(regions[1:3], c("MWI_4_20", "MWI_4_26", "MWI_4_29"))
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
  expect_equal(read_country(pjnz$path), "Botswana")
})

test_that("geojson read applies can use cache", {
  cache <- new_cache()
  path <- file_object(file.path("testdata", "malawi.geojson"))
  value <- hintr_geojson_read(path, cache)
  expect_equal(cache$list("geojson"), path$hash)
  expect_identical(hintr_geojson_read(path, cache), value)
})

test_that("can read spectrum region code from geojson", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  region_codes <- read_geojson_spectrum_region_codes(shape)
  expect_equal(region_codes, 0)

  skip_if_sensitive_data_missing()
  shape <- file_object(file.path("testdata", "sensitive", "ZMB", "data",
                     "zmb_areas.geojson"))
  region_codes <- read_geojson_spectrum_region_codes(shape)
  expect_equal(region_codes, c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19))
})

test_that("read csv removes empty rows", {
  path <- tempfile()
  writeLines(c("character,value", "a,1", ",", "b,2", "c,3", "d,4", ",", ","),
             path)
  expect_equal(nrow(read_csv(path)), 4)
})

test_that("read csv can read semicolon delimited files", {
  path <- tempfile()
  data <- data.frame(a = 1:4, b = 1:4)
  dir.create(path)
  path1 <- file.path(path, "input1.csv")
  path2 <- file.path(path, "input2.csv")
  write.csv(data, path1)
  write.csv2(data, path2)
  expect_equal(read_csv(path1), read_csv(path2))
})
