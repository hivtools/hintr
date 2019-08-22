do_validate_pjnz <- function(pjnz) {
  country <- read_country(pjnz)
  if (country == "GBR") {
    stop("Invalid country")
  }
  country
}

read_country <- function(pjnz) {
  ## TODO: Add function to specio to just extract metadata from the PJN and
  ## use this here instead to avoid getting unnecessary data. See mrc-388.
  hiv_params <- specio::read_hivproj_param(pjnz)
  hiv_params$country
}


do_validate_shape <- function(shape) {
  json <- geojsonio::geojson_read(shape, method = "local")
  country <- vapply(json$features, function(x) {
    x$properties$iso3
  }, character(1))
  if (length(unique(country)) != 1) {
    stop(sprintf(
      "Shape file contains regions for more than one country. Got %s.",
      toString(unique(country))))
  }
  regions <- get_json_regions(json)
  store_set(to_redis_key(shape, "region_list"), regions)
  json
}

#' Get regions from geoJSON
#'
#' @param json The Geojson.
#'
#' @return The list of regions from the geojson.
#' @keywords internal
get_json_regions <- function(json) {
  vapply(json$features, function(x) {
    x$properties$area_id
  }, character(1))
}
