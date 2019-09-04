do_validate_pjnz <- function(pjnz) {
  country <- read_country(pjnz)
  if (country == "GBR") {
    stop("Invalid country")
  }
  list(country = scalar(country))
}


read_country <- function(pjnz) {
  ## TODO: Add function to specio to just extract metadata from the PJN and
  ## use this here instead to avoid getting unnecessary data. See mrc-388.
  hiv_params <- specio::read_hivproj_param(pjnz)
  hiv_params$country
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
  json_verbatim(read_string(shape))
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
  scalar(NA)
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
  programme
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
  anc
}
