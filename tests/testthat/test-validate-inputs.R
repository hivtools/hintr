test_that("PJNZ can be validated and return data", {
  pjnz <- file_object(file.path("testdata", "Botswana2018.PJNZ"))
  expect_equal(do_validate_pjnz(pjnz),
               list(data = list(country = scalar("Botswana"),
                                iso3 = scalar("BWA")),
                    filters = json_verbatim("null")))
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
  expect_equal(json$filters$level_labels[[1]]$id, scalar(0L))
  expect_equal(json$filters$level_labels[[1]]$area_level_label,
               scalar("Country"))
  expect_equal(json$filters$level_labels[[1]]$display, scalar(TRUE))

  expect_equal(names(json$filters$regions), c("id", "label", "children"))
  expect_equal(json$filters$regions$id, scalar("MWI"))
  expect_equal(json$filters$regions$label, scalar("Malawi - Demo"))
  expect_length(json$filters$regions$children, 3)
  expect_equal(json$filters$regions$children[[1]]$label, scalar("Northern"))
  expect_equal(json$filters$regions$children[[2]]$label, scalar("Central"))
  expect_equal(json$filters$regions$children[[3]]$label, scalar("Southern"))
})

test_that("do_validate_shape returns if display property is missing", {
  shape <- file_object(file.path("testdata", "malawi_missing_display.geojson"))
  json <- do_validate_shape(shape)
  expect_s3_class(json$data, "json")
  expect_equal(json$filters$level_labels[[1]]$display, scalar(TRUE))
})

test_that("do_validate_population validates population file", {
  population <- file_object(file.path("testdata", "population.csv"))
  pop <- do_validate_population(population)

  expect_setequal(colnames(pop$data),
                  c("area_id", "area_name", "calendar_quarter",
                    "sex", "age_group", "population"))
  expect_true(nrow(pop$data) > 100)
  ## Data is rounded i.e. no decimal point
  expect_false(grepl("\\.", as.character(pop$data$population[[1]])))
  expect_population_metadata(pop$metadata)
})

test_that("empty rows are ignored in validation", {
  ## We saw in workshops people uploading csv files with several empty rows
  ## The rows just contained , delimiters see mrc-1151
  path <- tempfile(fileext = ".csv")
  writeLines(c("area_id,source,calendar_quarter,sex,age_group,population,asfr",
"MWI_4_1,Census 2018,CY2008Q2,female,Y000_004,16155.885821168984,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y005_009,14445.68972796804,",
"MWI_4_1,Census 2018,CY2008Q2,male,Y010_014,12442.49314841828,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y015_019,11256.444365995902,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y020_024,7753.0536033182025,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y025_029,5942.4272195206995,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y030_034,5399.44719147703,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y035_039,4678.4498127542165,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y040_044,3514.76195260583,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y045_049,2685.274469895612,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y050_054,1973.1838662610244,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y055_059,1497.1350625645346,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y060_064,1482.1704491131122,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y065_069,1276.117945350401,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y070_074,1040.0770066208468,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y075_079,641.0301937241122,",
"MWI_4_1,Census 2018,CY2008Q2,female,Y080_999,802.0852823887416,",
",,,,,,",
",,,,,,"), path)
  population <- file_object(path)
  pop <- do_validate_population(population)

  # Check these empty rows have been ignored
  expect_equal(nrow(pop$data), 17)
})

test_that("do_validate_programme validates programme file", {
  programme <- file_object(file.path("testdata", "programme.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_programme(programme, shape)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data$data) > 200)
  expect_type(data$data$art_current, "double")
  expect_length(data$warnings, 0)
})

test_that("do_validate_anc validates ANC file and gets data for plotting", {
  anc <- file_object(file.path("testdata", "anc.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_anc(anc, shape)

  expect_true(nrow(data$data) > 200)
  expect_type(data$data$area_id, "character")
  expect_true(all(c("anc_prevalence", "anc_art_coverage") %in% colnames(data$data)))
})

test_that("do_validate_anc can include anc_hiv_status column", {
  anc <- read_csv(file.path("testdata", "anc.csv"))
  anc$anc_hiv_status <- runif(nrow(anc))
  t <- tempfile(fileext = ".csv")
  write.csv(anc, t)
  anc_file <- file_object(t)
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_anc(anc_file, shape)

  expect_true(nrow(data$data) > 200)
  expect_type(data$data$area_id, "character")
  expect_true(all(c("anc_prevalence", "anc_art_coverage") %in% colnames(data$data)))
})

test_that("do_validate_anc adds default anc_known_neg column", {
  anc <- file_object(file.path("testdata", "anc.csv"))
  x <- read.csv(anc$path)
  x$anc_known_neg <- NULL
  t <- tempfile(fileext = ".csv")
  write.csv(x, t, row.names = FALSE)
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_anc(file_object(t), shape)

  expect_true(nrow(data$data) > 200)
  expect_type(data$data$area_id, "character")
  expect_true("anc_known_neg" %in% colnames(data$data))
  expect_equal(data$data$anc_known_neg, rep(0, nrow(data$data)))
})

test_that("do_validate_survey validates survey file", {
  survey <- file_object(file.path("testdata", "survey.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_survey(survey, shape)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data$data) > 20000)
  expect_type(data$data$estimate, "double")
  expect_length(data$warnings, 0)
})

test_that("do_validate_vmmc validates vmmc file", {
  vmmc <- file_object(file.path("testdata", "vmmc.xlsx"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  data <- do_validate_vmmc(vmmc, shape)
  ## Some arbitrary test that the data has actually been returned
  expect_equal(data, list(data = json_verbatim("null"),
                          warnings = list()))
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
    "Regions aren't consistent survey file contains 68 regions missing from shape file including:\n\\w+")
})

test_that("converting from numeric to iso3 works", {
  expect_equal(iso_numeric_to_alpha_3(454), "MWI")
  expect_equal(iso_numeric_to_alpha_3(084), "BLZ")
})

test_that("can read iso3", {
  pjnz <- file_object(file.path("testdata", "Malawi2024.PJNZ"))
  expect_equal(read_iso3(pjnz, "pjnz"), "MWI")
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  expect_equal(read_iso3(shape, "shape"), "MWI")
})

test_that("can read regions", {
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  expect_true(all(grepl(
    "^MWI[_\\d]*(demo)*$", read_regions(shape, "shape")$area_id, perl = TRUE)))
  population <- file_object(file.path("testdata", "population.csv"))
  expect_true(all(grepl(
    "^MWI[_\\d]*(demo)*$", read_regions(population, "population"),
    perl = TRUE)))
})

test_that("baseline data can be validated as a collection", {
  pjnz <- file_object(file.path("testdata", "Malawi2024.PJNZ"))
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

test_that("useful error returned when csv file read fails", {
  not_a_csv <- file_object(tempfile(fileext = ".csv"))
  saveRDS("123", not_a_csv$path)
  shape <- file_object(file.path("testdata", "malawi.geojson"))

  expect_error(
    do_validate_population(not_a_csv),
    "Failed to read file. Please review input file and check it is a valid csv.")

  expect_error(
    do_validate_survey(not_a_csv),
    "Failed to read file. Please review input file and check it is a valid csv.")

  expect_error(
    do_validate_programme(not_a_csv),
    "Failed to read file. Please review input file and check it is a valid csv.")

  suppressWarnings(expect_error(
    do_validate_anc(not_a_csv, shape),
    "Failed to read file. Please review input file and check it is a valid csv."))
})
