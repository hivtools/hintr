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
               "Different spectrum region codes in PJNZ and shape file.
1 code in PJNZ missing from shape file: 3
0 codes in shape file missing from PJNZ:")

  shape_codes <- c(1, 2, 3, 4)
  expect_error(assert_consistent_region_codes(pjnz_codes, shape_codes),
               "Different spectrum region codes in PJNZ and shape file.
0 codes in PJNZ missing from shape file: \n1 code in shape file missing from PJNZ: 4")

  pjnz_codes <- c(1, 2, 3, 5)
  expect_error(assert_consistent_region_codes(pjnz_codes, shape_codes),
               "Different spectrum region codes in PJNZ and shape file.
1 code in PJNZ missing from shape file: 5
1 code in shape file missing from PJNZ: 4")
})

test_that("can check file extensions", {
  expect_true(assert_file_extension("testdata/anc.csv", "csv"))
  expect_true(assert_file_extension("testdata/Botswana2018.PJNZ",
                                    c("PJNZ", "zip")))
  expect_true(assert_file_extension("testdata/Botswana2018.pjnz",
                                    c("PJNZ", "zip")))
  expect_error(assert_file_extension("testdata/anc.csv",
                                     c("PJNZ", "zip")),
               "File must be of type PJNZ, zip, got type csv.")
})

test_that("can check region file spectrum codes are valid", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  json <- hintr_geojson_read(shape)
  expect_true(assert_region_codes_valid(json))

  mock_contains_property <- mockery::mock(c(FALSE, TRUE, TRUE))
  with_mock("hintr:::features_contain_property" = mock_contains_property, {
    expect_true(assert_region_codes_valid(json))
  })

  mock_contains_property <- mockery::mock(c(FALSE, FALSE, TRUE))
  with_mock("hintr:::features_contain_property" = mock_contains_property, {
    expect_error(assert_region_codes_valid(json),
                 "Shape file contains 2 regions with missing spectrum region code, code can only be missing for country level region.")
  })
})

test_that("can check a column for expected values", {
  data <- data_frame(
    age_group = c("00-04", "05-09", "10-14"),
    sex = c("male", "female", "female")
  )

  expect_true(assert_expected_values(data, "age_group",
                                     c("00-04", "05-09", "10-14")))
  expect_true(assert_expected_values(data, "sex", c("male", "female")))

  expect_error(assert_expected_values(data, "age_group", c("00-04")),
               "Unexpected values in column age_group: 05-09, 10-14")
  expect_error(assert_expected_values(data, "sex", c("male")),
               "Unexpected values in column sex: female")

  expect_error(assert_expected_values(data, "missing_col", "test"),
               "Data does not contain required column: missing_col")

  expect_true(assert_expected_values(data, "age_group",
                                     c("00-04", "05-09", "10-14"),
                                     all_values = TRUE))
  expect_error(assert_expected_values(data, "age_group",
                                      c("00-04", "05-09", "10-14", "15-19"),
                                      all_values = TRUE),
               "Column age_group is missing required values: 15-19")

})

test_that("can check column values for expected patterns", {
  data <- data_frame(
    calendar_quarter = c("CY1998Q2", "CY2012Q3", "CY2020Q2"),
    year= c("2019", "1999", "2025")
  )

  expect_true(assert_calendar_quarter_column(data))
  expect_true(assert_year_column(data))

  data$calendar_quarter <- c("1998", "CY2012Q3", "CY2020Q9")
  data$year <- c("2010Q2", "2018", "Y2020")
  expect_error(assert_calendar_quarter_column(data),
               "Values in column calendar_quarter do not match required format: 1998, CY2020Q9")
  expect_error(assert_year_column(data),
               "Values in column year do not match required format: 2010Q2, Y2020")
  expect_error(assert_column_matches(data, "missing_column", ".*"),
               "Data does not contain required column: missing_column")

})

test_that("can check data comes from a single source", {
  data <- data.frame(source=c("NSO", "NSO", "NSO"))

  expect_true(assert_single_source(data))

  data$source <- c("NSO", "WorldPop", "NSO")
  expect_error(assert_single_source(data),
               "Data should be from a single source. Multiple sources present: NSO, WorldPop")
})

test_that("can check the validity of ANC data", {
  data <- data_frame(ancrt_test_pos=c(2,3,6), ancrt_known_pos=c(4,5,6),
                     ancrt_already_art=c(1,1,5), ancrt_tested=c(2,4,9))

  expect_true(assert_anc_client_numbers(data))

  data$ancrt_tested <- c(2,2,9)
  expect_error(assert_anc_client_numbers(data),
                 "The number of people who tested positive is greater than the number of people tested")
  data$ancrt_already_art <- c(20,1,5)
  data$ancrt_tested <- c(2,4,9)
  expect_error(assert_anc_client_numbers(data),
                 "The number of people already on ART is greater than the number positive \\(those known to be positive \\+ those who tested positive\\)")
})

test_that("can check that certain combinations of column values are unique", {
  data <- data_frame(area_id = rep("XXX_1_1",3), calendar_quarter = rep("CY2000Q2",3),
                     age_group = rep("00-04",3), sex = c("male", "female", "both"))

  cols_for_unique <- c("area_id", "calendar_quarter", "age_group", "sex")

  expect_true(assert_unique_combinations(data, cols_for_unique))

  cols_for_unique <- c("area_id", "calendar_quarter", "age_group")
  expect_error(assert_unique_combinations(data, cols_for_unique),
               "Unique combinations are required for columns: area_id, calendar_quarter, age_group")
})

test_that("can check that a column contains only positive numeric values", {
  data <- data_frame(population=c(1, 2, 2, 3),
                     current_art=c("none", 2, 0, 5),
                     test_pos=c(-1, -3, 2, 4))

  expect_true(assert_column_positive_numeric(data, "population"))
  expect_error(assert_column_positive_numeric(data, "current_art"),
               "Column current_art is required to be numeric. Non-numeric values were found.")
  expect_error(assert_column_positive_numeric(data, "test_pos"),
               "Column test_pos requires positive numeric values. Negative numeric values were found.")
  expect_error(assert_column_positive_numeric(data, c("population", "current_art")),
                                              "Column current_art is required to be numeric. Non-numeric values were found.")
        })

test_that("can check for non NA values", {
  data <- data_frame(
    calendar_quarter = c("CY1998Q2", "CY2012Q3", "CY2020Q2"),
    year= c("2019", "1999", NA)
  )
  expect_true(assert_no_na(data, "calendar_quarter"))
  expect_error(assert_no_na(data, "year"),
               "Found NA values in column year. NA values not allowed.")
})
