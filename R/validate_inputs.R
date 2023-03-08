do_validate_pjnz <- function(pjnz) {
  assert_file_extension(pjnz, c("PJNZ", "zip"))
  pjnz_paths <- get_pjnz_paths(pjnz)
  countries <- lapply(pjnz_paths, read_country)
  if (length(unique(countries)) != 1) {
    stop(t_("INPUTS_PJNZ_ZIP_MIXED",
            list(countries = collapse(unique(countries)))))
  }
  pjnz_spectrum_region_codes <-
    vapply(pjnz_paths, read_spectrum_region_code, numeric(1))
  zero_codes <- pjnz_spectrum_region_codes == 0
  if (length(which(zero_codes)) > 1) {
    stop(t_("INPUTS_PJNZ_ZIP_REGION0",
            list(count = length(which(zero_codes)),
                 err = collapse(basename(names(zero_codes))))))
  }
  list(
    data = list(country = scalar(countries[[1]]),
                iso3 = scalar(read_pjnz_iso3_from_path(pjnz_paths[[1]]))),
    filters = json_verbatim("null")
  )
}

read_spectrum_region_code <- function(...) {
  naomi::read_spectrum_region_code(...)
}

read_iso3 <- function(file, type) {
  func <- switch(type,
    "pjnz" = read_pjnz_iso3,
    "shape" = read_geojson_iso3,
    stop(t_("INVALID_ISO3", data = list(type = type))))
  func(file)
}

read_regions <- function(file, type) {
  func <- switch(type,
    "shape" = read_geojson_data,
    "population" = read_csv_regions,
    "programme" = read_csv_regions,
    "anc" = read_csv_regions,
    "survey" = read_csv_regions,
    stop(t_("INVALID_REGIONS", data = list(type = type))))
  func(file)
}

#' Validate shape file and return geojson for plotting.
#'
#' This checks that the geojson is for a single country and that each
#' feature has an area id associated with it.
#'
#' @param shape A file object (path, hash, filename) corresponding to
#'   the input shape file.
#'
#' @return An error if invalid or the geojson if valid.
#' @keywords internal
do_validate_shape <- function(shape) {
  # In general, we're going to be reading something other than geojson
  # most likely - something like a shapefile.  That's good news
  # because these are super slow to read in (~3s for the sample file
  # and it's only 2.5MB large).  A caching layer will help, but this
  # is going to lock things up enough we might need to do it
  # asynchronously.
  assert_file_extension(shape, "geojson")
  json <- hintr_geojson_read(shape)
  assert_single_parent_region(json)
  assert_single_country(json, "shape")
  assert_properties_exist(json, "area_id")
  ## TODO: Add region code validation see mrc-1305
  # Then we have to *reread* the file now that we know that it is
  # valid, but but this is not too slow, especially as the file is now
  # in cache (but still ~1/20s)
  list(data = json_verbatim(read_string(shape$path)),
       filters = list("regions" = get_region_filters(json),
                      "level_labels" = get_level_labels(json)))
}

#' Validate population file.
#'
#' Check that population file can be read.
#'
#' @param population Path to input population file.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_population <- function(population) {
  assert_file_extension(population, "csv")
  population <- read_csv(population$path, header = TRUE)
  assert_single_country(population, "population")
  assert_column_names(
    colnames(population),
    c("area_id", "calendar_quarter", "sex", "age_group", "source", "population"))
  assert_calendar_quarter_column(population)
  assert_expected_values(population, "sex", c("male", "female"), all_values=TRUE)
  assert_expected_values(population, "age_group", naomi::get_five_year_age_groups(), all_values=TRUE)
  assert_unique_combinations(population, c("area_id", "calendar_quarter", "sex", "age_group"))
  assert_single_source(population)
  assert_no_na(population, "population")
  assert_column_positive_numeric(population, "population")
  list(data = json_verbatim("null"),
       filters = json_verbatim("null"))
}

#' Validate programme ART data file.
#'
#' Check that programme ART data file can be read and return serialised data.
#'
#' @param programme A file object (path, hash, filename) corresponding to
#'   the input population file.
#'
#' @param shape A file object (path, hash, filename) corresponding to
#'   the input shape file.
#'
#' @param pjnz A file object (path, hash, filename) corresponding to
#'   the input PJNZ file.
#'
#' @param strict If FALSE then run less stringent validation rules, used
#'   for data exploration mode.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_programme <- function(programme, shape, strict = TRUE) {
  assert_file_extension(programme, "csv")
  data <- read_csv(programme$path, header = TRUE)
  assert_single_country(data, "programme")
  assert_column_names(
    colnames(data),
    c("area_id", "calendar_quarter", "sex", "age_group", "art_current"))
  shape_regions <- read_regions(shape, "shape")
  assert_consistent_regions(shape_regions$area_id, unique(data$area_id),
                            "programme")
  assert_single_level_per_year(shape_regions, data)
  assert_unique_combinations(
    data, c("area_id", "calendar_quarter", "sex", "age_group"))
  assert_expected_values(data, "sex", c("male", "female", "both"))
  art_ages <- naomi::get_age_groups()$age_group
  art_ages <- art_ages[!art_ages %in% c("Y000_000", "Y001_004")]
  assert_expected_values(data, "age_group", art_ages)
  data$art_current <- as.numeric(data$art_current)
  assert_column_positive_numeric(data, "art_current")
  assert_calendar_quarter_column(data)
  list(data = data,
       filters = list("age" = get_age_filters(data),
                      "calendar_quarter" = get_quarter_filters(data),
                      "indicators" = get_indicator_filters(data, "programme")),
       warnings = list())
}

#' Validate ANC data file.
#'
#' Check that ANC data file can be read and return serialised data.
#'
#' @param anc Path to input population file.
#'
#' @param shape A file object (path, hash, filename) corresponding to
#'   the input shape file.
#'
#' @param pjnz A file object (path, hash, filename) corresponding to
#'   the input PJNZ file.
#'
#' @param strict If FALSE then run less stringent validation rules, used
#'   for data exploration mode.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_anc <- function(anc, shape, strict = TRUE) {
  assert_file_extension(anc, "csv")
  data <- as.data.frame(naomi::read_anc_testing(anc$path))
  assert_single_country(data, "anc")
  assert_column_names(
    colnames(data),
    c("area_id", "age_group", "year", "anc_clients",
      "anc_known_pos", "anc_already_art", "anc_tested", "anc_tested_pos"))
  shape_regions <- read_regions(shape, "shape")
  assert_consistent_regions(shape_regions$area_id, unique(data$area_id), "ANC")
  assert_single_level_per_year(shape_regions, data)
  assert_unique_combinations(data, c("area_id", "age_group", "year"))
  assert_expected_values(data, "age_group", "Y015_049")
  assert_year_column(data)
  assert_column_positive_numeric(data, c("anc_clients", "anc_known_pos", "anc_already_art",
                                         "anc_tested", "anc_tested_pos"))
  if (strict) {
    assert_anc_client_numbers(data)
  }
  data <- naomi::calculate_prevalence_art_coverage(data)
  list(data = data,
       filters = list("year" = get_year_filters(data),
                      "indicators" = get_indicator_filters(data, "anc")),
       warnings = list())
}

#' Validate survey data file.
#'
#' Check that survey data file can be read and return serialised data.
#'
#' @param survey A file object (path, hash, filename) corresponding to
#'   the input survey file.
#'
#' @param shape A file object (path, hash, filename) corresponding to
#'   the input shape file.
#'
#' @param strict If FALSE then run less stringent validation rules, used
#'   for data exploration mode.
#'
#' @param pjnz A file object (path, hash, filename) corresponding to
#'   the input PJNZ file.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_survey <- function(survey, shape, strict = TRUE) {
  assert_file_extension(survey, "csv")
  data <- read_csv(survey$path, header = TRUE)
  assert_single_country(data, "survey")
  assert_column_names(
    colnames(data),
    c("area_id", "survey_id", "sex", "age_group",
      "indicator", "n_clusters", "n_observations",
      "estimate", "std_error", "ci_lower", "ci_upper"))
  survey_regions <- read_regions(shape, "shape")
  assert_consistent_regions(survey_regions$area_id,
                            read_regions(survey, "survey"),
                            "survey")
  assert_calendar_quarter_column(data, "survey_mid_calendar_quarter")
  assert_unique_combinations(data, c("area_id", "survey_id", "sex", "age_group",
                                     "indicator"))
  assert_expected_values(data, "sex", c("male", "female", "both"))
  assert_expected_values(data, "age_group", naomi::get_age_groups()$age_group)
  assert_column_positive_numeric(data,
                                 c("n_observations", "n_eff_kish", "estimate",
                                   "std_error", "ci_lower", "ci_upper"))
  list(data = data,
       filters = list("age" = get_age_filters(data),
                      "surveys" = get_survey_filters(data),
                      "indicators" = get_indicator_filters(data, "survey")),
       warnings = list())
}

#' Validate collection of baseline data for consistency.
#'
#' @param pjnz A file object (path, hash, filename) corresponding to
#'   the input pjnz file
#' @param shape A file object (path, hash, filename) corresponding to
#'   the input shape file.
#' @param population A file object (path, hash, filename) corresponding to
#' the input population file.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_baseline <- function(pjnz, shape, population) {
  check_pjnz_shape <- !is.null(pjnz) && !is.null(shape)
  check_shape_population <- !is.null(shape) && !is.null(population)
  valid_pjnz_shape <- TRUE
  valid_shape_population <- TRUE
  if (check_pjnz_shape) {
    valid_pjnz_shape <- validate_pjnz_shape(pjnz, shape)
  }
  if (check_shape_population) {
    valid_shape_population <- validate_shape_population(shape, population)
  }
  list(consistent = scalar(valid_pjnz_shape && valid_shape_population))
}

validate_pjnz_shape <- function(pjnz, shape) {
  pjnz_paths <- get_pjnz_paths(pjnz)
  metadata <- lapply(pjnz_paths, specio::read_pjn_metadata)
  ## Already validated that each subnational PJNZ has the same country
  ## so we only need to check 1 path here.
  pjnz_country <- iso_numeric_to_alpha_3(metadata[[1]]$iso3)
  shape_country <- read_iso3(shape, "shape")
  assert_consistent_country(pjnz_country, "pjnz", shape_country, "shape")

  pjnz_spectrum_region_codes <- lapply(metadata, function(x) x$region_code)
  shape_spectrum_region_codes <- read_geojson_spectrum_region_codes(shape)
  assert_consistent_region_codes(pjnz_spectrum_region_codes,
                                shape_spectrum_region_codes)
  TRUE
}

validate_shape_population <- function(shape, population) {
  shape_regions <- read_regions(shape, "shape")
  population_regions <- read_regions(population, "population")
  assert_consistent_regions(shape_regions$area_id, population_regions,
                            "population")
  TRUE
}
