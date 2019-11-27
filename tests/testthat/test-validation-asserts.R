context("validation-asserts")

test_that("assert fails if more than once country in json", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  json <- hintr_geojson_read(shape)
  expect_true(assert_single_country(json, "shape"))

  ## Change a country for purpose of testing
  json$features[[1]]$properties$area_id <- "AGO"
  expect_error(assert_single_country(json, "shape"),
               "Shape file contains regions for more than one country. Got countries AGO, MWI.")

  df <- data_frame(
    area_id = paste0(rep("MWI", 10), ".", as.character(seq_len(10))))
  expect_true(assert_single_country(df, "population"))

  df[11, "area_id"] <- "AGO.1.2.3"
  expect_error(assert_single_country(df, "population"),
               "Population file contains regions for more than one country. Got countries MWI, AGO.")

  expect_error(assert_single_country(character(0), "test"),
               "Test file contains no regions. Check file has an area_id column.")
})

test_that("assert fails if a feature is missing an area id", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  json <- hintr_geojson_read(shape)
  expect_true(assert_property_exists("area_id", json))
  expect_true(assert_property_exists("spectrum_region_code", json))
  expect_true(assert_properties_exist(json,
                                      c("area_id", "spectrum_region_code")))

  ## Remove an ID for testing
  json$features[[1]]$properties$area_id <- NULL
  expect_error(assert_property_exists("area_id", json),
               "Shape file does not contain property area_id for each region. Missing ID for 1 feature.")

  json$features[[1]]$properties$spectrum_region_code <- NULL
  expect_error(assert_property_exists("spectrum_region_code", json),
               "Shape file does not contain property spectrum_region_code for each region. Missing ID for 1 feature.")
})

test_that("assert_column_names checks column names are as expected", {
  expect_true(assert_column_names(c("col1", "col2"), c("col1", "col2")))
  expect_error(assert_column_names(c("col1"), c("col1", "col2")),
               "Data missing column col2")
  expect_true(assert_column_names(c("col1", "col2"), c("col1")))
})

test_that("assert_consistent_country checks for consistent countries", {
  expect_true(assert_consistent_country("test", "source1", "test", "source2"))
  expect_true(assert_consistent_country("Test", "source1", "test", "source2"))
  expect_true(assert_consistent_country(NULL, "source1", NULL, "source2"))
  expect_true(assert_consistent_country("test", "source1", NULL, "source2"))
  expect_true(assert_consistent_country(NULL, "source1", "test", "source2"))
  expect_error(assert_consistent_country("test", "source1", "test2", "source2"),
    "Countries aren't consistent got test from source1 and test2 from source2.")
})

test_that("assert_consistent_regions checks for consistent regions", {
  shape_regions <- c("Blantyre", "Chikwawa", "Chiradzulu", "Machinga")
  test_regions <- c("Blantyre", "Chikwawa", "Chiradzulu")
  expect_true(assert_consistent_regions(shape_regions, test_regions, "population"))
  expect_true(assert_consistent_regions(shape_regions, shape_regions, "population"))
  test_regions <- c("Rumphi", "Balaka", "Blantyre", "Chikwawa", "Chiradzulu")
  expect_error(assert_consistent_regions(shape_regions, test_regions, "population"),
    "Regions aren't consistent population file contains 2 regions missing from shape file including:
Rumphi, Balaka")
})

test_that("assert_file_exists throws error if file doesn't exist", {
  expect_true(assert_file_exists(file.path("testdata", "Malawi2019.PJNZ")))
  expect_error(assert_file_exists("file.path"),
               "File at path file.path does not exist. Create it, or fix the path.")
})

test_that("assert_single_parent_region fails if more than one parent region", {
  data <- list(
    features = list(
      list(
        properties = list(
          area_id = "MWI"
        )
      ),
      list(
        properties = list(
          area_id = "MWI_1"
        )
      ),
      list(
        properties = list(
          area_id = "MWI_2"
        )
      ),
      list(
        properties = list(
          area_id = "MWI_1_1"
        )
      )
    )
  )

  expect_true(assert_single_parent_region(data))

  data <- list(
    features = list(
      list(
        properties = list(
          area_id = "MWI"
        )
      ),
      list(
        properties = list(
          area_id = "MWI"
        )
      ),
      list(
        properties = list(
          area_id = "MWI_2"
        )
      ),
      list(
        properties = list(
          area_id = "MWI_1_1"
        )
      )
    )
  )
  expect_error(assert_single_parent_region(data),
    "Should have located one parent regions but found regions MWI, MWI.")
})

test_that("can test region codes are consistent", {
  pjnz_codes <- c(1, 2)
  shape_codes <- c(1, 2)
  expect_true(assert_consistent_region_codes(pjnz_codes, shape_codes))

  pjnz_codes <- c(1, 2, 3)
  expect_error(assert_consistent_region_codes(pjnz_codes, shape_codes),
               "PJNZ files contain spectrum region codes missing from shape file: 3")

  shape_codes <- c(1, 2, 3, 4)
  expect_error(assert_consistent_region_codes(pjnz_codes, shape_codes),
               "Shape file contains spectrum region codes missing from PJNZ files: 4")

  pjnz_codes <- c(1, 2, 3, 5)
  expect_error(assert_consistent_region_codes(pjnz_codes, shape_codes),
               "Shape file contains spectrum region codes missing from PJNZ files: 4
PJNZ files contain spectrum region codes missing from shape file: 5")
})
