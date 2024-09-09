## Use to convert a data frame into a list with format like
## list(
##   list(a = 1, b = 2),
##   list(a = 3, b = 4)
## )
## Use this because after a JSON roundtrip we lose data.frame formatting
## or it applies it too aggressively depending on value
## of simplifyVector in call to simplifyVector
## Use this so that we can use same test for data frame and list type
df_to_list <- function(x) {
  lapply(seq(nrow(x)), function(row_num) { as.list(x[row_num, ])})
}

expect_input_metadata <- function(metadata,
                                  expected_choropleth_sources = c(),
                                  expected_time_series_sources = c()) {
  expect_valid_metadata(metadata)
  if (length(expected_time_series_sources) > 0) {
    expected_plot_settings <- c("timeSeries", "inputChoropleth")
  } else {
    expected_plot_settings <- c("inputChoropleth")
  }
  expect_setequal(names(metadata$plotSettingsControl),
                  expected_plot_settings)

  choropleth_data_sources <- vcapply(
    metadata$plotSettingsControl$inputChoropleth$plotSettings[[1]]$option,
    "[[", "id")
  expect_setequal(choropleth_data_sources, expected_choropleth_sources)

  time_series_data_sources <- vcapply(
    metadata$plotSettingsControl$timeSeries$plotSettings[[1]]$option,
    "[[", "id")
  if (length(expected_time_series_sources) > 0) {
    expect_setequal(time_series_data_sources, expected_time_series_sources)
  } else {
    expect_length(time_series_data_sources, 0)
  }
}

expect_calibrate_plot_metadata <- function(metadata) {
  expect_valid_metadata(metadata)
  expect_equal(names(metadata),
               c("filterTypes", "indicators", "plotSettingsControl"))

  if (is.data.frame(metadata$indicators)) {
    metadata$indicators <- df_to_list(metadata$indicators)
  }
  expect_true(length(metadata$indicators) > 0)
  cols_names <- names(metadata$indicators[[1]])
  expect_setequal(cols_names,
                  c("indicator", "value_column", "error_low_column",
                    "error_high_column", "indicator_column", "indicator_value",
                    "indicator_sort_order", "name", "min", "max", "colour",
                    "invert_scale", "scale", "accuracy", "format"))

  filters <- lapply(metadata$filterTypes, "[[",
                    "column_id")
  ## Ignore attr below as before serialization this will be wrapped in a scalar
  ## class but afterwards it won't
  expect_equal(filters[[1]], "calendar_quarter", ignore_attr = TRUE)
  expect_equal(filters[[2]], "sex", ignore_attr = TRUE)
  expect_equal(filters[[3]], "age_group", ignore_attr = TRUE)
  expect_equal(filters[[4]], "indicator", ignore_attr = TRUE)
  expect_equal(filters[[5]], "data_type", ignore_attr = TRUE)
  expect_equal(filters[[6]], "spectrum_region_code", ignore_attr = TRUE)
}


expect_comparison_metadata <- function(metadata) {
  expect_valid_metadata(metadata)
  expect_equal(names(metadata),
               c("filterTypes", "indicators", "plotSettingsControl"))

  if (is.data.frame(metadata$indicators)) {
    metadata$indicators <- df_to_list(metadata$indicators)
  }
  expect_true(length(metadata$indicators) > 0)
  cols_names <- names(metadata$indicators[[1]])
  expect_setequal(cols_names,
                  c("indicator", "value_column", "error_low_column",
                    "error_high_column", "indicator_column", "indicator_value",
                    "indicator_sort_order", "name", "min", "max", "colour",
                    "invert_scale", "scale", "accuracy", "format"))

  filters <- lapply(metadata$filterTypes, "[[",
                    "column_id")
  ## Ignore attr below as before serialization this will be wrapped in a scalar
  ## class but afterwards it won't
  expect_equal(filters[[1]], "indicator", ignore_attr = TRUE)
  expect_equal(filters[[2]], "area_id", ignore_attr = TRUE)
  expect_equal(filters[[3]], "area_level", ignore_attr = TRUE)
  expect_equal(filters[[4]], "calendar_quarter", ignore_attr = TRUE)
  expect_equal(filters[[5]], "sex", ignore_attr = TRUE)
  expect_equal(filters[[6]], "age_group", ignore_attr = TRUE)
  expect_equal(filters[[7]], "source", ignore_attr = TRUE)
}

mock_filter_types <- function(ids) {
  lapply(ids, function(id) {
    list(
      id = scalar(id),
      column_id = scalar(paste0("col_", id)),
      options = list(
        list(
          id = scalar("opt1"),
          label = scalar("Option 1")
        ),
        list(
          id = scalar("opt2"),
          label = scalar("Option 2")
        )
      )
    )
  })
}

## Test validity of the metadata, namely that ids used in any effect which
## reference filters actually exist in the filter types
expect_valid_metadata <- function(metadata) {
  filter_ids <- lapply(metadata$filterTypes, "[[", "id")
  filter_types <- setNames(metadata$filterTypes, filter_ids)

  plot_names <- names(metadata$plotSettingsControl)
  for (plot in plot_names) {
    default_effects <- metadata$plotSettingsControl[[plot]]$defaultEffect
    default_filter_map <- validate_effects(
      filter_types,
      metadata$plotSettingsControl[[plot]]$defaultEffect,
      paste0("plot: '", plot, "', default effects"))
    for (plot_setting in metadata$plotSettingsControl[[plot]]$plotSettings) {
      for (option in plot_setting$options) {
        validate_effects(
          filter_types,
          plot_setting$options$effect,
          paste0("plot: '",  plot,
                 "', setting: '", plot_setting$id,
                 "', option: '", option$id, "'"),
          default_filter_map)
      }
    }
  }
  invisible(TRUE)
}

#' Validate effects
#'
#' Effects have filterId and stateFilterId
#' stateFilterId is the unique ID for the dropdown
#' filterId is the unique ID of the FilterType, used to identify options
#' this is because there can be multiple dropdowns using the same underlying
#' data e.g. colour and size indicator in the bubble plot
#' setFilter effects have the mapping between filterId and stateFilterId
#' all other effects refer only to the stateFilterId
#'
#' @param filter_types Named filter types
#' @param effects Effects to check
#' @param context Additional context appended to any test failure messages
#' @param state_filter_map Map of state filter ID to filter ID. See above for
#'   details. Used to check that effects refer to a filter which exists.
#'
#' @return Nothing, called for side effect
#' @noRd
validate_effects <- function(filter_types, effects, context,
                             state_filter_map = list()) {
  if (!is.null(effects$setFilters)) {
    filter_ids <- vcapply(effects$setFilters, "[[", "filterId")
    expect_filter_exists(names(filter_types),
                         filter_ids,
                         paste(context, "setFilters"))
    state_filter_ids <- vcapply(effects$setFilters, "[[", "stateFilterId")
    state_filter_map <- c(state_filter_map,
                          setNames(filter_ids, state_filter_ids))
    expect_state_filter_id_unique(state_filter_map)
  } else if (length(state_filter_map) == 0) {
    ## This is kind of gross but logic is, if setFilters is not included
    ## in an effect, then use all the available filters. So have to replicate
    ## this here for validation
    state_filter_map <- setNames(names(filter_types), names(filter_types))
  }
  if (!is.null(effects$setMultiple)) {
    expect_filter_exists(names(filter_types),
                         state_filter_map[unlist(effects$setMultiple)],
                         paste(context, "setMultiple"))
  }
  if (!is.null(effects$setFilterValues)) {
    expect_filter_exists(names(filter_types),
                         state_filter_map[names(effects$setFilterValues)],
                         paste(context, "setFilterValues"))
    for (state_filter_id in names(effects$setFilterValues)) {
      filter_id <- state_filter_map[[state_filter_id]]
      if (isTRUE(filter_types[[filter_id]])) {
        break
      }
      all_option_ids <- get_filter_option_ids(filter_types[[filter_id]]$options)
      expect_option_exists(all_option_ids,
                           effects$setFilterValues[[state_filter_id]],
                           paste(context, "setFilterValues"),
                           effect$id)
    }
  }
  if (!is.null(effects$setHidden)) {
    expect_filter_exists(names(filter_types),
                         state_filter_map[unlist(effects$setHidden)],
                         paste(context, "setHidden"))
  }
  state_filter_map
}

expect_filter_exists <- function(all_filter_ids, effect_filter_ids, context) {
  for (filter_id in effect_filter_ids) {
    expect_true(
      filter_id %in% all_filter_ids,
      sprintf(
        "Checking %s: Failed to find filter with ID '%s' in filterTypes.",
        context, filter_id))
  }
}

expect_option_exists <- function(all_option_ids, effect_option_ids, context,
                                 filter_id) {
  for (option_id in effect_option_ids) {
    expect_true(
      option_id %in% all_option_ids,
      sprintf(
        paste("Checking %s: Failed to find filter option with",
              "ID '%s' in options for filter '%s'"),
        context, option_id, filter_id))
  }
}


expect_state_filter_id_unique <- function(state_filter_map, context) {
  expect_equal(length(names(state_filter_map)),
               length(unique(names(state_filter_map))),
               info = paste0("Checking ", context,
                             ": Found state filter ID used more than",
                             " once. This must be unique"))
}

get_filter_option_ids <- function(filter_options) {
  if (is.data.frame(filter_options)) {
    filter_options <- df_to_list(filter_options)
  }
  vcapply(filter_options, "[[", "id")
}
