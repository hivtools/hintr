do_validate_pjnz <- function(pjnz) {
  country <- read_country(pjnz)
  if (country == "GBR") {
    stop("Invalid country")
  }
  list(
    data = list(country = scalar(country)),
    filters = scalar(NA)
  )
}

read_country <- function(pjnz) {
  ## TODO: Add function to specio to just extract metadata from the PJN and
  ## use this here instead to avoid getting unnecessary data. See mrc-388.
  hiv_params <- specio::read_hivproj_param(pjnz)
  hiv_params$country
}

read_iso3 <- function(file, type) {
  func <- switch(type,
    "pjnz" = read_pjnz_iso3,
    "shape" = read_geojson_iso3,
    stop(sprintf("Can't read country from data of type %s.", type)))
  func(file)
}

read_pjnz_iso3 <- function(pjnz_file) {
  hiv_params <- specio::read_hivproj_param(pjnz_file)
  iso_numeric_to_alpha_3(hiv_params$iso3)
}

## Convert numeric iso3 country code to the alpha-3 code
iso_numeric_to_alpha_3 <- function(numeric_iso) {
  spectrum5_countrylist[which(spectrum5_countrylist$Code == numeric_iso),
                        "iso3"]
}

read_geojson_iso3 <- function(geojson_file) {
  json <- hintr_geojson_read(geojson_file)
  ## At this point we have validated there is only data for 1 country so we can
  ## just take the first.
  json$features[[1]]$properties$iso3
}

read_regions <- function(file, type) {
  func <- switch(type,
    "shape" = read_geojson_regions,
    "population" = read_csv_regions,
    stop(sprintf("Can't read regions from data of type %s.", type)))
  func(file)
}

read_geojson_regions <- function(geojson_file) {
  json <- hintr_geojson_read(geojson_file)
  vapply(json$features, function(x) {
    x$properties$area_id
  }, character(1))
}

read_csv_regions <- function(csv_file) {
  data <- read_csv(csv_file, header = TRUE)
  unique(data$area_id)
}


#' Validate shape file and return geojson for plotting.
#'
#' This checks that the geojson is for a single country and that each
#' feature has an area id associated with it.
#'
#' @param shape Path to input shape file.
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
  assert_single_country(json, "shape")
  assert_area_id_exists(json)
  # Then we have to *reread* the file now that we know that it is
  # valid, but but this is not too slow, especially as the file is now
  # in cache (but still ~1/20s)
  ## TODO: Add shape region filters
  list(data = json_verbatim(read_string(shape)),
       filters = NULL)
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
  population <- read_csv(population, header = TRUE)
  assert_single_country(population, "population")
  assert_column_names(
    colnames(population),
    c("iso3", "area_id", "time", "sex", "age_group_id", "source", "population"))
  list(data = scalar(NA),
       filters = scalar(NA))
}

#' Validate programme ART data file.
#'
#' Check that programme ART data file can be read and return serialised data.
#'
#' @param programme Path to input population file.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_programme <- function(programme) {
  programme <- read_csv(programme, header = TRUE)
  assert_single_country(programme, "programme")
  assert_column_names(
    colnames(programme),
    c("iso3", "area_id", "period", "sex", "age_group_id", "indicator", "value"))
  list(data = programme,
       filters = list("age" = get_age_filters(programme)))
}

#' Validate ANC data file.
#'
#' Check that ANC data file can be read and return serialised data.
#'
#' @param anc Path to input population file.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_anc <- function(anc) {
  anc <- read_csv(anc, header = TRUE)
  assert_single_country(anc, "anc")
  assert_column_names(
    colnames(anc),
    c("iso3", "area_id", "period", "sex", "age_group_id", "indicator", "value"))
  list(data = anc,
       filters = list("age" = get_age_filters(anc)))
}

#' Validate survey data file.
#'
#' Check that survey data file can be read and return serialised data.
#'
#' @param survey Path to input survey file.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_survey <- function(survey) {
  survey <- read_csv(survey, header = TRUE)
  assert_single_country(survey, "survey")
  assert_column_names(
    colnames(survey),
    c("iso3", "area_id", "survey_id", "year", "sex", "age_group_id",
      "indicator", "value", "se", "ci_l", "ci_u"))
  survey
  list(data = survey,
       filters = list("age" = get_age_filters(survey),
                      "surveys" = get_survey_filters(survey)))
}

#' Validate collection of baseline data for consistency.
#'
#' @param pjnz Path to input pjnz file.
#' @param shape Path to input shape file.
#' @param population Path to input population file.
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
                                                    shape_country, "shape",
                                                    convert_to_alpha3 = TRUE)
  }
  if (check_regions) {
    shape_regions <- read_regions(shape, "shape")
    population_regions <- read_regions(population, "population")
    consistent_regions <- assert_consistent_regions(
      shape_regions, population_regions, "population")
  }
  list(complete = scalar(check_country && check_regions),
       consistent = scalar(consistent_country && consistent_regions))
}
