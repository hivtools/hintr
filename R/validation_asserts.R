assert_column_names <- function(names, expected_names) {
  missing <- setdiff(expected_names, names)
  if (length(missing) > 0) {
    missing <- setdiff(expected_names, names)
    stop(sprintf("Data missing %s %s.",
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

assert_consistent_country <- function(country_x, source_x, country_y, source_y,
                                      convert_to_alpha3 = FALSE) {
  if (!is.null(country_x) && !is.null(country_y) &&
      tolower(country_x) != tolower(country_y)) {
    if (convert_to_alpha3) {
      country_x <- iso_numeric_to_alpha_3(country_x)
      country_y <- iso_numeric_to_alpha_3(country_y)
    }
    stop(sprintf("Countries aren't consistent got %s from %s and %s from %s.",
                 country_x, source_x, country_y, source_y))
  }
  invisible(TRUE)
}

assert_consistent_regions <- function(shape_regions, test_regions, test_source) {
  ## Regions are fine if regions from shape file are a super set of the
  ## regions being tested
  if (!is_superset(shape_regions, test_regions)) {
    stop(sprintf(
      "Regions aren't consistent %s file contains regions %s missing from shape file.",
      test_source, paste0(setdiff(test_regions, shape_regions), collapse = ", ")))
  }
  invisible(TRUE)
}

is_superset <- function(super, sub) {
  diff <- setdiff(sub, super)
  length(diff) == 0
}
