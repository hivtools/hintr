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
  json <- geojsonio::geojson_read(shape, method = "local")
  assert_single_country(json)
  assert_area_id_exists(json)
  # Then we have to *reread* the file now that we know that it is
  # valid, but but this is not too slow, especially as the file is now
  # in cache (but still ~1/20s)
  json_verbatim(read_string(shape))
}

assert_single_country <- function(json) {
  country <- vapply(json$features, function(x) {
    x$properties$iso3
  }, character(1))
  if (length(unique(country)) != 1) {
    stop(sprintf(
      "Shape file contains regions for more than one country. Got countries %s.",
      toString(unique(country))))
  }
  invisible(TRUE)
}

assert_area_id_exists <- function(json) {
  contains_id <- vapply(json$features, function(x) {
    !is_empty(x$properties$area_id)
  }, logical(1))
  if (!all(contains_id)) {
    missing_count <- sum(!contains_id)
    stop(
      sprintf(
        "Shape file does not contain an area ID for each region. Missing ID for %s %s.",
        missing_count,
        ngettext(missing_count, "feature", "features")
      )
    )
  }
  invisible(TRUE)
}

#' Validate population file.
#'
#' Check that population file can be read.
#'
#' @param shape Path to input population file.
#'
#' @return An error if invalid.
#' @keywords internal
do_validate_population <- function(population) {
  population <- read_csv(population, header = TRUE)
  ## Perhaps this kind of assertion should just be checked by json schema?
  assert_column_names(
    colnames(population),
    c("iso3", "area_id", "time", "sex", "age_group_id", "source", "population"))
  NULL
}

assert_column_names <- function(names, expected_names) {
  missing <- setdiff(expected_names, names)
  if (length(missing) > 0) {
    missing <- setdiff(expected_names, names)
    stop(sprintf("Data missing %s %s",
                 ngettext(length(missing), "column", "columns"),
                 paste(setdiff(expected_names, names), collapse = ", ")))
  }
  invisible(TRUE)
}
