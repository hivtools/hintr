do_validate_pjnz <- function(pjnz) {
  country <- read_country(pjnz)
  if (country == "GBR") {
    stop("Invalid country")
  }
  list(
    data = list(country = scalar(country),
                iso3 = scalar(read_pjnz_iso3(pjnz))),
    filters = scalar(NA)
  )
}

read_iso3 <- function(file, type) {
  func <- switch(type,
    "pjnz" = read_pjnz_iso3,
    "shape" = read_geojson_iso3,
    stop(sprintf("Can't read country from data of type %s.", type)))
  func(file)
}

read_regions <- function(file, type) {
  func <- switch(type,
    "shape" = read_geojson_regions,
    "population" = read_csv_regions,
    "programme" = read_csv_regions,
    "anc" = read_csv_regions,
    "survey" = read_csv_regions,
    stop(sprintf("Can't read regions from data of type %s.", type)))
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
  json <- hintr_geojson_read(shape)
  assert_single_parent_region(json)
  assert_single_country(json, "shape")
  assert_area_id_exists(json)
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
  population <- read_csv(population$path, header = TRUE)
  assert_single_country(population, "population")
  assert_column_names(
    colnames(population),
    c("area_id", "quarter_id", "sex", "age_group_id", "source", "population"))
  list(data = scalar(NA),
       filters = scalar(NA))
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
#' @return An error if invalid.
#' @keywords internal
do_validate_programme <- function(programme, shape) {
  data <- read_csv(programme$path, header = TRUE)
  assert_single_country(data, "programme")
  assert_column_names(
    colnames(data),
    c("area_id", "quarter_id", "sex", "age_group_id", "current_art"))
  assert_consistent_regions(read_regions(shape, "shape"),
                            read_regions(programme, "programme"),
                            "programme")
  list(data = data,
       filters = list("age" = get_age_filters(data),
                      "quarter" = get_quarter_filters(data),
                      "indicators" = get_indicator_filters(data, "programme")))
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
#' @return An error if invalid.
#' @keywords internal
do_validate_anc <- function(anc, shape) {
  data <- read_csv(anc$path, header = TRUE)
  assert_single_country(data, "anc")
  assert_column_names(
    colnames(data),
    c("area_id", "age_group_id", "quarter_id", "anc_clients",
      "ancrt_hiv_status", "ancrt_known_pos", "ancrt_already_art",
      "ancrt_tested", "ancrt_test_pos"))
  assert_consistent_regions(read_regions(shape, "shape"),
                            read_regions(anc, "anc"),
                            "ANC")
  data <- naomi::calculate_prevalence_art_coverage(data)
  list(data = data,
       filters = list("quarter" = get_quarter_filters(data),
                      "indicators" = get_indicator_filters(data, "anc")))
}

#' Validate survey data file.
#'
#' Check that survey data file can be read and return serialised data.
#'
#' @param survey Path to input survey file.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_survey <- function(survey, shape) {
  data <- read_csv(survey$path, header = TRUE)
  assert_single_country(data, "survey")
  assert_column_names(
    colnames(data),
    c("iso3", "area_id", "survey_id", "survey_year", "sex", "age_group_id",
      "indicator", "n_cluster", "n_obs", "est", "se", "ci_l", "ci_u"))
  assert_consistent_regions(read_regions(shape, "shape"),
                            read_regions(survey, "survey"),
                            "survey")
  list(data = data,
       filters = list("age" = get_age_filters(data),
                      "surveys" = get_survey_filters(data),
                      "indicators" = get_indicator_filters(data, "survey")))
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
  check_country <- !is.null(pjnz) && !is.null(shape)
  check_regions <- !is.null(shape) && !is.null(population)
  consistent_country <- TRUE
  consistent_regions <- TRUE

  if (check_country) {
    pjnz_country <- read_iso3(pjnz, "pjnz")
    shape_country <- read_iso3(shape, "shape")
    consistent_country <- assert_consistent_country(pjnz_country, "pjnz",
                                                    shape_country, "shape")
  }
  if (check_regions) {
    shape_regions <- read_regions(shape, "shape")
    population_regions <- read_regions(population, "population")
    consistent_regions <- assert_consistent_regions(
      shape_regions, population_regions, "population")
  }
  list(consistent = scalar(consistent_country && consistent_regions))
}
