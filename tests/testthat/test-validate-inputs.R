context("validate-inputs")

test_that("PJNZ can be validated and return data", {
  pjnz <- file_object(file.path("testdata", "Botswana2018.PJNZ"))
  expect_equal(do_validate_pjnz(pjnz),
               list(data = list(country = scalar("Botswana"),
                                iso3 = scalar("BWA")),
                    filters = scalar(NA)))
  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    expect_error(do_validate_pjnz(pjnz), "Invalid country")
  })
})

test_that("country can be read", {
  pjnz <- file_object(file.path("testdata", "Botswana2018.PJNZ"))
  expect_equal(read_country(pjnz), "Botswana")
})

test_that("assert fails if more than once country in json", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  json <- hintr_geojson_read(shape)
  expect_true(assert_single_country(json, "shape"))

  ## Change a country for purpose of testing
  json$features[[1]]$properties$area_id <- "AGO"
  expect_error(assert_single_country(json, "shape"),
               "Shape file contains regions for more than one country. Got countries AGO, MWI.")
})

test_that("assert fails if a feature is missing an area id", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  json <- hintr_geojson_read(shape)
  expect_true(assert_area_id_exists(json))

  ## Remove an ID for testing
  json$features[[1]]$properties$area_id <- NULL
  expect_error(assert_area_id_exists(json),
               "Shape file does not contain an area ID for each region. Missing ID for 1 feature.")
})

test_that("do_validate_shape validates shape and returns geojson as list", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  json <- do_validate_shape(shape)
  expect_s3_class(json$data, "json")
  expect_equal(names(json$filters), c("regions", "level_labels"))

  expect_length(json$filters$level_labels, 5)
  expect_equal(names(json$filters$level_labels[[1]]),
               c("id", "area_level_label", "display"))
  expect_equal(json$filters$level_labels[[1]]$id, scalar(0))
  expect_equal(json$filters$level_labels[[1]]$area_level_label,
               scalar("Country"))
  expect_equal(json$filters$level_labels[[1]]$display, scalar(TRUE))

  expect_equal(names(json$filters$regions), c("id", "label", "children"))
  expect_equal(json$filters$regions$id, scalar("MWI"))
  expect_equal(json$filters$regions$label, scalar("Malawi"))
  expect_length(json$filters$regions$children, 3)
  expect_equal(json$filters$regions$children[[1]]$label, scalar("Northern"))
  expect_equal(json$filters$regions$children[[2]]$label, scalar("Central"))
  expect_equal(json$filters$regions$children[[3]]$label, scalar("Southern"))
})

test_that("do_validate_population validates population file", {
  population <- file_object(file.path("testdata", "population.csv"))
  pop <- do_validate_population(population)
  ## No actual data to return but has been validated
  expect_true(is.na(pop$data))
  expect_true(is.na(pop$filters))
})

test_that("do_validate_programme validates programme file", {
  programme <- file_object(file.path("testdata", "programme.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_programme(programme, shape)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data$data) > 1400)
  expect_equal(typeof(data$data$current_art), "double")

  expect_equal(names(data$filters), c("age", "quarter", "indicators"))
  expected_age_filters <- list(
    list(id = scalar("20"),
         label = scalar("15+")),
    list(id = scalar("24"),
         label = scalar("0-14"))
  )
  expect_equal(data$filters$age, expected_age_filters)
  expect_quarter_filters <- list(
    list(id = scalar("20"),
         label = scalar("15+")),
    list(id = scalar("24"),
         label = scalar("0-14"))
  )
  expect_length(data$filters$quarter, 32)
  expect_equal(data$filters$quarter[[1]]$id, scalar("445"))
  expect_equal(data$filters$quarter[[1]]$label, scalar("Jan-Mar 2011"))

  expect_length(data$filters$indicators, 1)
  expect_equal(data$filters$indicators[[1]]$id, scalar("current_art"))
  expect_equal(data$filters$indicators[[1]]$label, scalar("ART number"))
})

test_that("do_validate_anc validates ANC file and gets data for plotting", {
  anc <- file_object(file.path("testdata", "anc.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_anc(anc, shape)

  expect_true(nrow(data$data) > 800)
  expect_equal(typeof(data$data$ancrt_hiv_status), "integer")
  expect_true(all(c("prevalence", "art_coverage") %in% colnames(data$data)))

  expect_equal(names(data$filters), c("quarter", "indicators"))
  expected_filters <- list(
    list(id = scalar("18"),
         label = scalar("15-49")))
  expect_length(data$filters$quarter, 29)
  expect_equal(data$filters$quarter[[1]]$id, scalar("447"))
  expect_equal(data$filters$quarter[[1]]$label, scalar("Jul-Sep 2011"))

  expect_length(data$filters$indicators, 2)
  expect_equal(data$filters$indicators[[1]]$id, scalar("prevalence"))
  expect_equal(data$filters$indicators[[1]]$label, scalar("Prevalence"))
  expect_equal(data$filters$indicators[[2]]$id, scalar("art_coverage"))
  expect_equal(data$filters$indicators[[2]]$label, scalar("ART coverage"))
})

test_that("do_validate_survey validates survey file", {
  survey <- file_object(file.path("testdata", "survey.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_survey(survey, shape)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data$data) > 20000)
  expect_equal(typeof(data$data$est), "double")
  expect_equal(names(data$filters), c("age", "surveys", "indicators"))
  expected_ages <- list(id = scalar("18"),
                        label = scalar("15-49"))
  expected_survey <- list(
    list(id = scalar("MWI2016PHIA"),
         label = scalar("MWI2016PHIA")),
    list(id = scalar("MWI2015DHS"),
         label = scalar("MWI2015DHS")),
    list(id = scalar("MWI2010DHS"),
         label = scalar("MWI2010DHS")),
    list(id = scalar("MWI2004DHS"),
         label = scalar("MWI2004DHS"))
  )
  expect_equal(data$filters$age[[1]], expected_ages)
  expect_length(data$filters$age, 21)
  expect_equal(data$filters$surveys, expected_survey)

  expect_length(data$filters$indicators, 4)
  expect_equal(data$filters$indicators[[1]]$id, scalar("prevalence"))
  expect_equal(data$filters$indicators[[1]]$label, scalar("Prevalence"))
  expect_equal(data$filters$indicators[[2]]$id, scalar("art_coverage"))
  expect_equal(data$filters$indicators[[2]]$label, scalar("ART coverage"))
  expect_equal(data$filters$indicators[[3]]$id, scalar("vls"))
  expect_equal(data$filters$indicators[[3]]$label,
               scalar("Viral load suppression"))
  expect_equal(data$filters$indicators[[4]]$id, scalar("recent"))
  expect_equal(data$filters$indicators[[4]]$label,
               scalar("Proportion recently infected"))
})

test_that("do_validate_programme returns useful error from shapefile comparison", {
  programme <- file_object(file.path("testdata", "programme.csv"))
  shape <- file_object(file.path("testdata", "malawi_missing_regions.geojson"))
  expect_error(do_validate_programme(programme, shape),
    "Regions aren't consistent programme file contains 32 regions missing from shape file including:\n\\w+")
})

test_that("do_validate_anc returns useful error from shapefile comparison", {
  anc <- file_object(file.path("testdata", "anc.csv"))
  shape <- file_object(file.path("testdata", "malawi_missing_regions.geojson"))
  expect_error(do_validate_anc(anc, shape),
    "Regions aren't consistent ANC file contains 32 regions missing from shape file including:\n\\w+")
})

test_that("do_validate_survey returns useful error from shapefile comparison", {
  survey <- file_object(file.path("testdata", "survey.csv"))
  shape <- file_object(file.path("testdata", "malawi_missing_regions.geojson"))
  expect_error(do_validate_survey(survey, shape),
    "Regions aren't consistent survey file contains 64 regions missing from shape file including:\n\\w+")
})

test_that("converting from numeric to iso3 works", {
  expect_equal(iso_numeric_to_alpha_3(454), "MWI")
  expect_equal(iso_numeric_to_alpha_3(084), "BLZ")
})

test_that("can read iso3", {
  pjnz <- file_object(file.path("testdata", "Malawi2019.PJNZ"))
  expect_equal(read_iso3(pjnz, "pjnz"), "MWI")
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  expect_equal(read_iso3(shape, "shape"), "MWI")
})

test_that("can read regions", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  expect_true(all(grepl(
    "^MWI[\\.\\d]*$", read_regions(shape, "shape"), perl = TRUE)))
  population <- file_object(file.path("testdata", "population.csv"))
  expect_true(all(grepl(
    "^MWI[\\.\\d]*$", read_regions(population, "population"), perl = TRUE)))
})

test_that("baseline data can be validated as a collection", {
  pjnz <- file_object(file.path("testdata", "Malawi2019.PJNZ"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  population <- file_object(file.path("testdata", "population.csv"))
  response <- do_validate_baseline(pjnz, shape, population)
  expect_true(response$consistent)
  expect_true(response$complete)

  response <- do_validate_baseline(NULL, shape, population)
  expect_true(response$consistent)
  expect_false(response$complete)

  response <- do_validate_baseline(pjnz, NULL, population)
  expect_true(response$consistent)
  expect_false(response$complete)

  response <- do_validate_baseline(pjnz, shape, NULL)
  expect_true(response$consistent)
  expect_false(response$complete)

  botswana_pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  expect_error(do_validate_baseline(botswana_pjnz, shape, NULL))

  response <- do_validate_baseline(botswana_pjnz, NULL, population)
  expect_true(response$consistent)
  expect_false(response$complete)
})
