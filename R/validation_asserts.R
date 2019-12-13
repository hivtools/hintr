assert_column_names <- function(names, expected_names) {
  missing <- setdiff(expected_names, names)
  if (length(missing) > 0) {
    missing <- setdiff(expected_names, names)
    stop(t_("VALIDATION_COLUMN_NAMES",
            list(count = length(missing),
                 missing = paste(missing, collapse = ", "))))
  }
  invisible(TRUE)
}

#' Check that values for specified column belong in a set of expected values
#'
#' Optionally check that the column contains all expected values.
#'
#' @param data The data to check
#' @param column_name The column name to check
#' @param expected_values The set of expected values
#' @param all_values If TRUE check that the column contains all expected values
#'
#' @return TRUE is valid otherwise throws an error
#' @keywords internal
assert_expected_values <- function(data, column_name, expected_values, all_values = FALSE) {

  if (is.null(data[[column_name]])) {
    stop(t_("VALIDATION_COLUMN_REQUIRED", list(name = column_name)))
  }
  values <- unique(data[[column_name]])
  if (all_values) {
    missing_values <- setdiff(expected_values, values)
    if (length(missing_values > 0)) {
      stop(t_("VALIDATION_COLUMN_VALUE_MISSING",
              list(name = column_name, missing = collapse(missing_values))))
    }
  }

  unexpected_values <- setdiff(values, expected_values)
  if (length(unexpected_values) > 0) {
    stop(t_("VALIDATION_COLUMN_VALUE_UNEXPECTED",
            list(name = column_name,
                 unexpected = paste(unexpected_values, collapse=", "))))
  }
  invisible(TRUE)
}


#' Check that values of column match regex pattern
#'
#' @param data Data to check
#' @param column_name Column to check
#' @param pattern Pattern to match
#'
#' @return TRUE is valid otherwise throws an error
#' @keywords internal
assert_column_matches <- function(data, column_name, pattern) {
  if (is.null(data[[column_name]])) {
    stop(t_("VALIDATION_COLUMN_REQUIRED", list(name = column_name)))
  }
  values <- unique(data[[column_name]])
  check <- grepl(pattern, values)
  if (!all(check)) {
    unmatched <- values[which(check == FALSE)]
    stop(t_("VALIDATION_COLUMN_MATCHES",
            list(name = column_name,
                 unmatched = paste(unmatched, collapse = ", "))))
  }
  invisible(TRUE)
}

assert_calendar_quarter_column <- function(data) {
  assert_column_matches(data, "calendar_quarter", "^CY[12][901][0-9]{2}Q[1-4]$")
}

assert_year_column <- function(data) {
  assert_column_matches(data, "year", "^[12][901][0-9][0-9]$")
}

#' Check that the values of a column are not NA
#'
#' @param data data to be checked
#' @param column_names column name to be checked
#'
#' @return TRUE if column contains no NA values else throws error
#' @keywords internal
assert_no_na <- function(data, column_name) {
  column_data <- data[[column_name]]
  if (is.null(column_data)) {
    stop(t_("VALIDATION_COLUMN_REQUIRED", list(name = column_name)))
  }
  if(any(is.na(column_data))) {
    stop(t_("VALIDATION_COLUMN_NO_NA", list(name = column_name)))
  }
  invisible(TRUE)
}

#' Check that the values of a column are positive numeric only
#'
#' @param data data to be checked
#' @param column_names column name to be checked
#'
#' @return TRUE if numeric positive else throws error
#' @keywords internal
assert_column_positive_numeric <- function(data, column_names) {
  out <- lapply(column_names, function(column_name) {
    column_data <- data[[column_name]]
    if (is.null(column_data)) {
      stop(t_("VALIDATION_COLUMN_REQUIRED", list(name = column_name)))
    }
    if (!is.numeric(column_data)) {
      stop(t_("VALIDATION_COLUMN_NUMERIC", list(name = column_name)))
    }
    if (any(column_data < 0, na.rm = TRUE)) {
      stop(t_("VALIDATION_COLUMN_POSITIVE", list(name = column_name)))
    }
    invisible(TRUE)
  })
  invisible(TRUE)
}

#' Checks that the data source column contains a single value
#'
#' @param data Data to check source column for single value
#'
#' @return TRUE is valid else throws error
#' @keywords internal
assert_single_source <- function(data) {
  if (length(unique(data$source)) > 1) {
    stop(t_("VALIDATION_SINGLE_SOURCE",
            list(sources = paste(unique(data$source), collapse=", "))))
  }
  invisible(TRUE)
}

#' Provides some checks on the validity of the ANC data
#'
#' @param data ANC data to validate
#'
#' @return TRUE if data passes validity checks, else throws error
#' @keywords internal
assert_anc_client_numbers <- function(data) {
  check_pos <- data$ancrt_tested - data$ancrt_test_pos
  if (any(check_pos < 0, na.rm = TRUE)) {
    stop(t_("VALIDATION_ANC_CLIENT_NUMBERS1"))
  }

  check_on_art <- (data$ancrt_test_pos + data$ancrt_known_pos) - data$ancrt_already_art
  if (any(check_on_art < 0, na.rm = TRUE)) {
    stop(t_("VALIDATION_ANC_CLIENT_NUMBERS2"))
  }
  invisible(TRUE)
}

#' Check for unique combinations of values in each row of selected columns
#'
#' @param data
#' @param columns_for_unique
#'
#' @return TRUE if the required combinations are unique, else throws error
#' @keywords internal
assert_unique_combinations <- function(data, columns_for_unique) {

  if (any(duplicated(data[ ,columns_for_unique]))) {
    stop(t_("VALIDATION_UNIQUE_COMBINATIONS",
            list(columns = paste(columns_for_unique, collapse = ", "))))
  }
  invisible(TRUE)
}


assert_single_parent_region <- function(json) {
  regions <- vcapply(json$features, function(x) {
    x$properties$area_id
  })
  parent_region <- regions[!grepl("\\_", regions)]
  if (length(parent_region) != 1) {
    stop(t_("VALIDATION_SINGLE_PARENT_REGION",
            list(regions = collapse(parent_region))))
  }
  invisible(TRUE)
}

assert_single_country <- function(data, type) {
  UseMethod("assert_single_country", data)
}

assert_single_country.geojson <- function(data, type) {
  ## TODO: geojson will contain the spectrum ID perhaps that will make a more
  ## appropriate check of single country? See once geojson has been updated by
  ## Jeff mrc-501
  country <- vcapply(data$features, function(x) {
    substr(x$properties$area_id, 1, 3)
  })
  assert_single_country(country, type)
}

assert_single_country.data.frame <- function(data, type) {
  assert_single_country(substr(data$area_id, 1, 3), type)
}

assert_single_country.character <- function(data, type) {
  if (length(unique(data)) == 0) {
    stop(t_("VALIDATION_SINGLE_COUNTRY_NO_REGIONS",
            list(type = to_upper_first(type))))
  } else if (length(unique(data)) != 1) {
    stop(t_("VALIDATION_SINGLE_COUNTRY_MUTIPLE",
            list(type = to_upper_first(type),
                 countries = toString(unique(data)))))
  }
  invisible(TRUE)
}

assert_properties_exist <- function(json, properties) {
 lapply(properties, assert_property_exists, json)
 invisible(TRUE)
}

assert_region_codes_valid <- function(json) {
  contains_property <- features_contain_property(json, "spectrum_region_code")
  missing_count <- sum(!contains_property)
  if (missing_count > 1) {
    stop(t_("VALIDATION_REGION_CODES_VALID", list(count = missing_count)))
  }
  invisible(TRUE)
}

features_contain_property <- function(json, property) {
  vapply(json$features, function(x) {
    !is_empty(x$properties[[property]])
  }, logical(1))
}

assert_property_exists <- function(property, json) {
  contains_property <- features_contain_property(json, property)
  if (!all(contains_property)) {
    missing_count <- sum(!contains_property)
    stop(t_("VALIDATION_PROPERTY_EXISTS",
            list(property = property, count = missing_count)))
  }
  invisible(TRUE)
}

assert_consistent_country <- function(country_x, source_x, country_y, source_y) {
  if (!is.null(country_x) && !is.null(country_y) &&
      tolower(country_x) != tolower(country_y)) {
    stop(t_("VALIDATION_CONSISTENT_COUNTRY",
            list(country_x = country_x, source_x = source_x,
                 country_y = country_y, source_y = source_y)))
  }
  invisible(TRUE)
}

assert_consistent_regions <- function(shape_regions, test_regions, test_source) {
  ## Regions are fine if regions from shape file are a super set of the
  ## regions being tested
  if (!is_superset(shape_regions, test_regions)) {
    missing_regions <- setdiff(test_regions, shape_regions)
    stop(t_("VALIDATION_CONSISTENT_REGION",
            list(source = test_source,
                 count = length(missing_regions),
                 missing = collapse(missing_regions))))
  }
  invisible(TRUE)
}

assert_consistent_region_codes <- function(pjnz_codes, shape_codes) {
  if (!setequal(pjnz_codes, shape_codes)) {
    missing_code_from_shape <- setdiff(pjnz_codes, shape_codes)
    missing_code_from_pjnz <- setdiff(shape_codes, pjnz_codes)
    pjnz_no_missing <- length(missing_code_from_pjnz)
    shape_no_missing <- length(missing_code_from_shape)
    debug_info <- list(
      pjnz_no_missing = pjnz_no_missing,
      pjnz_code_text = ngettext(pjnz_no_missing, "code", "codes"),
      shape_no_missing = shape_no_missing,
      shape_code_text = ngettext(shape_no_missing, "code", "codes"),
      pjnz_missing_codes = collapse(missing_code_from_pjnz),
      shape_missing_codes = collapse(missing_code_from_shape)
    )
    msg <- paste(t_("VALIDATION_CONSISTENT_REGION_CODES1"),
                 t_("VALIDATION_CONSISTENT_REGION_CODES2",
                    list(count = length(missing_code_from_shape),
                         missing = collapse(missing_code_from_shape))),
                 t_("VALIDATION_CONSISTENT_REGION_CODES3",
                    list(count = length(missing_code_from_pjnz),
                         missing = collapse(missing_code_from_pjnz))),
                 sep = "\n")
    stop(msg)
  }
  invisible(TRUE)
}

is_superset <- function(super, sub) {
  diff <- setdiff(sub, super)
  length(diff) == 0
}

assert_file_exists <- function(file) {
  if (is.null(file) || !file.exists(file)) {
    stop(t_("VALIDATION_FILE_EXISTS", list(path = file %||% "NULL")))
  }
  invisible(TRUE)
}

assert_file_extension <- function(file_path, types) {
  extension <- tools::file_ext(file_path)
  if (!any(tolower(extension) %in% tolower(types))) {
    stop(t_("VALIDATION_FILE_EXTENSION",
            list(expected = collapse(types), got = extension)))
  }
  invisible(TRUE)
}
