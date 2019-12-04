context("validate-inputs")

test_that("PJNZ can be validated and return data", {
  pjnz <- file_object(file.path("testdata", "Botswana2018.PJNZ"))
  expect_equal(do_validate_pjnz(pjnz),
               list(data = list(country = scalar("Botswana"),
                                iso3 = scalar("BWA")),
                    filters = scalar(NA)))
})

test_that("country can be read", {
  pjnz <- file_object(file.path("testdata", "Botswana2018.PJNZ"))
  expect_equal(read_country(pjnz$path), "Botswana")
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
  expect_true(nrow(data$data) > 200)
  expect_equal(typeof(data$data$current_art), "double")

  expect_equal(names(data$filters), c("age", "year", "indicators"))
  expected_age_filters <- list(
    list(id = scalar("15+"),
         label = scalar("15+")),
    list(id = scalar("00-14"),
         label = scalar("0-14"))
  )
  expect_equal(data$filters$age, expected_age_filters)
  expect_length(data$filters$year, 8)
  expect_equal(data$filters$year[[1]]$id, scalar("2018"))
  expect_equal(data$filters$year[[1]]$label, scalar("2018"))

  expect_length(data$filters$indicators, 1)
  expect_equal(data$filters$indicators[[1]]$id, scalar("current_art"))
  expect_equal(data$filters$indicators[[1]]$label, scalar("ART number"))
})

test_that("do_validate_anc validates ANC file and gets data for plotting", {
  anc <- file_object(file.path("testdata", "anc.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_anc(anc, shape)

  expect_true(nrow(data$data) > 200)
  expect_equal(typeof(data$data$area_id), "character")
  expect_true(all(c("prevalence", "art_coverage") %in% colnames(data$data)))

  expect_equal(names(data$filters), c("year", "indicators"))
  expect_length(data$filters$year, 8)
  expect_equal(data$filters$year[[1]]$id, scalar("2018"))
  expect_equal(data$filters$year[[1]]$label, scalar("2018"))

  expect_length(data$filters$indicators, 2)
  expect_equal(data$filters$indicators[[1]]$id, scalar("prevalence"))
  expect_equal(data$filters$indicators[[1]]$label, scalar("Prevalence"))
  expect_equal(data$filters$indicators[[2]]$id, scalar("art_coverage"))
  expect_equal(data$filters$indicators[[2]]$label, scalar("ART coverage"))
})

test_that("do_validate_anc doesn't require ancrt_hiv_status column", {
  anc <- file_object(file.path("testdata", "anc_without_hiv_status.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_anc(anc, shape)

  expect_true(nrow(data$data) > 200)
  expect_equal(typeof(data$data$area_id), "character")
  expect_true(all(c("prevalence", "art_coverage") %in% colnames(data$data)))

  expect_equal(names(data$filters), c("year", "indicators"))
  expect_length(data$filters$year, 8)
  expect_equal(data$filters$year[[1]]$id, scalar("2018"))
  expect_equal(data$filters$year[[1]]$label, scalar("2018"))

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
  expected_ages <- list(id = scalar("15-49"),
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
  expect_equal(data$filters$indicators[[3]]$id, scalar("recent"))
  expect_equal(data$filters$indicators[[3]]$label,
               scalar("Proportion recently infected"))
  expect_equal(data$filters$indicators[[4]]$id, scalar("vls"))
  expect_equal(data$filters$indicators[[4]]$label,
               scalar("Viral load suppression"))
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

test_that("do_validate_anc passes for zambia", {
  skip_if_sensitive_data_missing()
  anc <- file_object(file.path("testdata", "sensitive", "ZMB", "data", "zmb_anc_testing.csv"))
  shape <- file_object(file.path("testdata", "sensitive", "ZMB", "data",
                     "zmb_areas.geojson"))
  expect_error(do_validate_anc(anc, shape),
               "The number of people already on ART is greater than the number positive (those known to be positive + those who tested positive)",
               fixed = TRUE)
})

test_that("do_validate_survey returns useful error from shapefile comparison", {
  survey <- file_object(file.path("testdata", "survey.csv"))
  shape <- file_object(file.path("testdata", "malawi_missing_regions.geojson"))
  expect_error(do_validate_survey(survey, shape),
    "Regions aren't consistent survey file contains 68 regions missing from shape file including:\n\\w+")
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
    "^MWI[_\\d]*$", read_regions(shape, "shape"), perl = TRUE)))
  population <- file_object(file.path("testdata", "population.csv"))
  expect_true(all(grepl(
    "^MWI[_\\d]*$", read_regions(population, "population"), perl = TRUE)))
})

test_that("baseline data can be validated as a collection", {
  pjnz <- file_object(file.path("testdata", "Malawi2019.PJNZ"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  population <- file_object(file.path("testdata", "population.csv"))
  response <- do_validate_baseline(pjnz, shape, population)
  expect_true(response$consistent)

  response <- do_validate_baseline(NULL, shape, population)
  expect_true(response$consistent)

  response <- do_validate_baseline(pjnz, NULL, population)
  expect_true(response$consistent)

  response <- do_validate_baseline(pjnz, shape, NULL)
  expect_true(response$consistent)

  botswana_pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  expect_error(do_validate_baseline(botswana_pjnz, shape, NULL))

  response <- do_validate_baseline(botswana_pjnz, NULL, population)
  expect_true(response$consistent)
})
