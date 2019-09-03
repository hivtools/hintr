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

assert_single_country <- function(data, type) {
  UseMethod("assert_single_country", data)
}

assert_single_country.geojson <- function(data, type) {
  country <- vapply(data$features, function(x) {
    x$properties$iso3
  }, character(1))
  assert_single_country(country, type)
}

assert_single_country.data.frame <- function(data, type) {
  assert_single_country(data$iso3, type)
}

assert_single_country.character <- function(data, type) {
  if (length(unique(data)) != 1) {
    stop(sprintf(
      "%s file contains regions for more than one country. Got countries %s.",
      to_upper_first(type), toString(unique(data))))
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
