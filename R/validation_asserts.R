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
