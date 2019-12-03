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
    stop(sprintf("Data does not contain required column: %s",
                 column_name))
  }
  values <- unique(data[[column_name]])
  if (all_values==TRUE) {
    missing_values <- setdiff(expected_values, values)
    if (length(missing_values>0)) {
      stop(sprintf("Column %s is missing required values: %s",
                   column_name, paste(missing_values, collapse=", ")))
    }
  }

  unexpected_values <- setdiff(values, expected_values)
  if (length(unexpected_values) > 0) {
    stop(sprintf("Unexpected values in column %s: %s",
                 column_name, paste(unexpected_values, collapse=", ")))
  }
  invisible(TRUE)
}


#' Check that values of calendar_quarter and year columns meet standard format
#'
#' @param data data to check
#' @param column_name column to check
#' @param pattern expected format
#'
#' @return TRUE is valid otherwise throws an error
#' @keywords internal
assert_column_matches <- function(data, column_name, pattern) {
  if (is.null(data[[column_name]])) {
    stop(sprintf("Data does not contain required column: %s",
                 column_name))
  }
  values <- unique(data[[column_name]])
  check <- grepl(pattern, values)
  if (!all(check)){
    unmatched <- values[which(check == FALSE)]
    stop(sprintf("Values in column %s do not match required format: %s",
                 column_name, paste(unmatched, collapse=", ")))
  }
  invisible(TRUE)
}

assert_calendar_quarter_column <- function(data) {
  assert_column_matches(data, "calendar_quarter", "^CY[12][901][0-9]{2}Q[1-4]$")
}
assert_year_column <- function(data) {
  assert_column_matches(data, "year", "^[12][901][0-9][0-9]$")
}

#' Checks that the data source column contains a single value
#'
#' @param data data to check source column for single value
#'
#' @return TRUE is valid else throws error
#' @keywords internal
assert_single_source <- function(data) {
  if (length(unique(data$source)) > 1) {
    stop(sprintf("Data should be from a single source. Multiple sources present: %s",
                 paste(unique(data$source), collapse=", ")))
  }
  invisible(TRUE)
}

assert_single_age_1549 <- function(data) {
  if (length(unique(data$age_group)) > 1) {
    stop(sprintf("Data should contain a single age_group 15-49. Multiple age groups present: %s",
                 paste(unique(data$age_group), collapse=", ")))
  }
  if (unique(data$age_group)=="15-49"){
    invisible(TRUE)
  }else{
    stop(sprintf("age_group should be 15-49"))
  }
}

assert_anc_client_numbers <- function(data) {
  check_pos <- data$ancrt_tested - data$ancrt_test_pos
  if (any(check_pos) < 0 ){
    stop(sprintf("The number of people who tested positive is greater than the number of people tested"))
  }

  check_tested <- (data$ancrt_test_pos + data$ancrt_known_pos) - data$ancrt_already_art
  data <- data_frame(ancrt_test_pos=c(2,3,6), ancrt_known_pos=c(4,5,6),
                     ancrt_already_art=c(1,1,1), ancrt_tested=c(2,4,5))
  check_tested
  check_pos
}

assert_single_parent_region <- function(json) {
  regions <- vcapply(json$features, function(x) {
    x$properties$area_id
  })
  parent_region <- regions[!grepl("\\_", regions)]
  if (length(parent_region) != 1) {
    stop(sprintf("Should have located one parent regions but found regions %s.",
                 collapse(parent_region)))
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
    stop(sprintf(
      "%s file contains no regions. Check file has an area_id column.",
      to_upper_first(type)
    ))
  } else if (length(unique(data)) != 1) {
    stop(sprintf(
      "%s file contains regions for more than one country. Got countries %s.",
      to_upper_first(type), toString(unique(data))))
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
    stop(sprintf(
      "Shape file contains %s regions with missing spectrum region code, code can only be missing for country level region.",
      missing_count))
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
    stop(
      sprintf(
        "Shape file does not contain property %s for each region. Missing ID for %s %s.",
        property,
        missing_count,
        ngettext(missing_count, "feature", "features")
      )
    )
  }
  invisible(TRUE)
}

assert_consistent_country <- function(country_x, source_x, country_y, source_y) {
  if (!is.null(country_x) && !is.null(country_y) &&
      tolower(country_x) != tolower(country_y)) {
    stop(sprintf("Countries aren't consistent got %s from %s and %s from %s.",
                 country_x, source_x, country_y, source_y))
  }
  invisible(TRUE)
}

assert_consistent_regions <- function(shape_regions, test_regions, test_source) {
  ## Regions are fine if regions from shape file are a super set of the
  ## regions being tested
  if (!is_superset(shape_regions, test_regions)) {
    missing_regions <- setdiff(test_regions, shape_regions)
    stop(sprintf(
      "Regions aren't consistent %s file contains %d %s missing from shape file including:\n%s",
      test_source,
      length(missing_regions),
      ngettext(length(missing_regions), "region", "regions"),
      collapse(missing_regions)))
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
    stop(glue::glue("Different spectrum region codes in PJNZ and shape file.",
               "{shape_no_missing} {shape_code_text} in PJNZ missing from shape file: {shape_missing_codes}",
               "{pjnz_no_missing} {pjnz_code_text} in shape file missing from PJNZ: {pjnz_missing_codes}",
               .sep = "\n", .envir = as.environment(debug_info)))
  }
  invisible(TRUE)
}

is_superset <- function(super, sub) {
  diff <- setdiff(sub, super)
  length(diff) == 0
}

assert_file_exists <- function(file) {
  if (is.null(file) || !file.exists(file)) {
    stop(sprintf("File at path %s does not exist. Create it, or fix the path.",
                 file %||% "NULL"))
  }
  invisible(TRUE)
}

assert_file_extension <- function(file_path, types) {
  extension <- tools::file_ext(file_path)
  if (!any(tolower(extension) %in% tolower(types))) {
    stop(sprintf("File must be of type %s, got type %s.",
                 collapse(types), extension))
  }
  invisible(TRUE)
}
