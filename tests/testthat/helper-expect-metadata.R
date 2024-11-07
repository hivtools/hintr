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

expect_input_comparison_metadata <- function(metadata) {
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

  # All filters have options
  for (filter in metadata$filterTypes) {
    expect(length(filter$options) > 0,
           sprintf("filter '%s' has no options", filter$id))
  }

  filters <- lapply(metadata$filterTypes, "[[",
                    "column_id")
  ## Ignore attr below as before serialization this will be wrapped in a scalar
  ## class but afterwards it won't
  expect_equal(filters[[1]], "indicator", ignore_attr = TRUE)
  expect_equal(filters[[2]], "area_name", ignore_attr = TRUE)
  expect_equal(filters[[3]], "year", ignore_attr = TRUE)
  expect_equal(filters[[4]], "group", ignore_attr = TRUE)
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
    if (length(metadata$plotSettingsControl[[plot]]$plotSettings) == 0) {
      validate_effects(filter_types, default_effects, list(),
                       paste0("plot: '", plot, "', default effects"))
    } else {
      for (plot_setting in metadata$plotSettingsControl[[plot]]$plotSettings) {
        for (option in plot_setting$options) {
          validate_effects(
            filter_types,
            option$effect,
            default_effects,
            paste0("plot: '",  plot,
                  "', setting: '", plot_setting$id,
                  "', option: '", option$id, "'"))
        }
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
#' @param default_effects Default effects applied for every plot option
#'   must be validated together as effects are additive
#' @param context Additional context appended to any test failure messages
#'
#' @return Nothing, called for side effect
#' @noRd
validate_effects <- function(filter_types, effects, default_effects, context) {
  ## Effects are additive, except for setFilters where effects on an
  ## individual filter override the defaults
  set_filters <- effects$setFilters
  if (length(effects$setFilters) == 0) {
    set_filters <- default_effects$setFilters
  }
  ## Note that this is currently true but is not necessary
  ## If we have e.g. plot control A and B and a default effect C.
  ## We could have A setFilters for all its settings and B and C do not
  ## contain a setFilters. In this case this test would fail for control B
  ## See https://github.com/mrc-ide/hintr/pull/523#discussion_r1751782412
  ## for discussion. This is fine for now, but relax this if we need to.
  expect(length(set_filters) > 0,
         paste(context, "setFilters must be set in plot setting or",
               "default settings."))

  state_filter_ids <- vcapply(set_filters, "[[", "stateFilterId")
  filter_ids <- vcapply(set_filters, "[[", "filterId")
  state_filter_map <- setNames(filter_ids, state_filter_ids)
  expect_filter_exists(names(filter_types),
                       filter_ids,
                       paste(context, "setFilters"))
  expect_state_filter_id_unique(state_filter_map, paste(context, "setFilters"))

  set_multiple <- unlist(c(effects$setMultiple, default_effects$setMultiple))
  if (length(set_multiple) > 0) {
    expect_filter_exists(names(filter_types),
                         state_filter_map[set_multiple],
                         paste(context, "setMultiple"))
  }

  set_filter_values <- c(effects$setFilterValues,
                         default_effects$setFilterValues)
  if (length(set_filter_values) > 0) {
    expect_filter_exists(names(filter_types),
                         state_filter_map[names(set_filter_values)],
                         paste(context, "setFilterValues"))
    for (state_filter_id in names(set_filter_values)) {
      filter_id <- state_filter_map[[state_filter_id]]
      if (isTRUE(filter_types[[filter_id]]$use_shape_regions)) {
        ## If we're referring to a filter which uses regions from
        ## shape file, we have no options in metadata. So skip this check.
        break
      }
      all_option_ids <- get_filter_option_ids(filter_types[[filter_id]]$options)
      expect_option_exists(all_option_ids,
                           set_filter_values[[state_filter_id]],
                           paste(context, "setFilterValues"),
                           state_filter_id)
    }
  }

  set_hidden <- unlist(c(effects$setHidden, default_effects$setHidden))
  if (length(set_hidden) > 0) {
    expect_filter_exists(names(filter_types),
                         state_filter_map[set_hidden],
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
