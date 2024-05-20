get_age_filters <- function(data) {
  ## Assuming data is a data frame with age_group
  filters <- get_age_labels(unique(data$age_group))
  sorted_filters <- filters[order(filters$age_group_sort_order), ]
  construct_filter(sorted_filters, "age_group", "age_group_label")
}

construct_filter <- function(data, id, name) {
  lapply(rownames(data), function(row_number) {
    list(id = scalar(as.character(data[row_number, id])),
         label = scalar(data[row_number, name]))
  })
}

get_sex_filters <- function(data) {
  sexes <- unique(data$sex)
  lapply(sexes, function(sex) {
    list(
      id = scalar(sex),
      label = scalar(to_upper_first(sex))
    )
  })
}

get_age_labels <- function(age_group) {
  age_groups <- naomi::get_age_groups()
  groups <- age_groups$age_group %in% age_group
  missing_ids <- setdiff(age_group, age_groups$age_group)
  if (length(missing_ids) > 0) {
    stop(t_("FILTERS_MISSING_AGE_GROUP", list(group = collapse(missing_ids))))
  }
  age_groups[groups,
             c("age_group", "age_group_label", "age_group_sort_order")]
}

get_survey_filters <- function(data) {
  survey_ids <- sort(unique(data$survey_id), decreasing = TRUE)
  lapply(survey_ids, function(survey_id) {
    list(id = scalar(survey_id),
         label = scalar(survey_id))
  })
}

get_indicator_filters <- function(data, type) {
  ## Input data either long or wide format
  get_filters <- switch(
    type,
    "anc" = read_wide_indicator_filters,
    "programme" = read_wide_indicator_filters,
    "survey" = read_long_indicator_filters,
    stop(t_("FILTERS_CANT_GET_INDICATOR", list(type = type))))
  get_filters(data, type)
}

get_area_level_filters <- function(data) {
  levels <- unique(data$area_level)
  lapply(levels, function(level) {
    list(
      id = scalar(as.character(level)),
      label = scalar(data[data$area_level == level, "area_level_label"][1])
    )
  })
}

get_indicator_options <- function(data_type) {
  metadata <- naomi::get_metadata()
  indicator_metadata <- metadata[
    metadata$data_type == data_type & metadata$plot_type == "barchart",
    c("indicator", "name")]
  names(indicator_metadata) <- c("id", "label")
  ordering <- metadata[
    metadata$data_type == data_type & metadata$plot_type == "barchart",
    c("indicator_sort_order")]
  indicator_metadata[order(ordering), ]
}

#' Read filters from wide format data
#'
#' Expect input data with headers like
#'
#' x, y, z, prevalence, art_cov
#'
#' So a column for each separate indicator. For the data type we have look
#' for the list of possible indicators from the metadata. If that indicator
#' exists in the metadata return it as a possible filter with ID and name.
#'
#' @param data Wide format data
#' @param type The type of data set
#'
#' @return Indicator filters
#' @keywords internal
#'
read_wide_indicator_filters <- function(data, type) {
  metadata <- naomi::get_metadata()
  type_metadata <- metadata[metadata$data_type == type, ]
  present_indicators <- type_metadata[
    type_metadata$value_column %in% colnames(data), ]
  construct_filter(present_indicators, "indicator", "name")
}

#' Read filters from long format data
#'
#' Expect input data with headers like
#'
#' x, y, z, indicator, value
#'
#' Where indicator columns describes the type of indicator the value is for.
#' For the data type we have look for the list of possible indicators from the
#' metadata. If that indicator exists in the metadata return it as a possible
#' filter with ID and name.
#'
#' @param data Long format data
#' @param type The type of data set
#'
#' @return Indicator filters
#' @keywords internal
#'
read_long_indicator_filters <- function(data, type) {
  metadata <- naomi::get_metadata()
  type_metadata <- metadata[metadata$data_type == type, ]
  indicator_column <- unique(type_metadata$indicator_column)
  present_indicators <- type_metadata[
    type_metadata$indicator_value %in% data[[indicator_column]], ]
  construct_filter(present_indicators, "indicator", "name")
}

get_model_output_filters <- function(data) {
  list(
    list(
      id = scalar("area"),
      column_id = scalar("area_id"),
      options = json_verbatim("null"),
      use_shape_regions = scalar(TRUE)
    ),
    list(
      id = scalar("detail"),
      column_id = scalar("area_level"),
      options = get_area_level_filters(data)
    ),
    list(
      id = scalar("period"),
      column_id = scalar("calendar_quarter"),
      options = get_quarter_filters(data)
    ),
    list(
      id = scalar("sex"),
      column_id = scalar("sex"),
      options = get_sex_filters(data)
    ),
    list(
      id = scalar("age"),
      column_id = scalar("age_group"),
      options = get_age_filters(data)
    ),
    list(
      id = scalar("indicator"),
      column_id = scalar("indicator"),
      options = get_indicator_options("output")
    )
  )
}

get_calibrate_plot_filters <- function(data) {
  list(
    list(
      id = scalar("period"),
      column_id = scalar("calendar_quarter"),
      options = get_quarter_filters(data)
    ),
    list(
      id = scalar("sex"),
      column_id = scalar("sex"),
      options = get_sex_filters(data)
    ),
    list(
      id = scalar("age"),
      column_id = scalar("age_group"),
      options = get_age_filters(data)
    ),
    list(
      id = scalar("calibrate_indicator"),
      column_id = scalar("indicator"),
      options = get_indicator_options("calibrate")
    ),
    list(
      id = scalar("type"),
      column_id = scalar("data_type"),
      options = get_data_type_filters()
    ),
    list(
      id = scalar("spectrum_region"),
      column_id = scalar("spectrum_region_code"),
      options = get_spectrum_region_filters(data)
    )
  )
}

get_comparison_plot_filters <- function(data) {
  list(
    list(
      id = scalar("indicator"),
      column_id = scalar("indicator"),
      options = get_indicator_options("comparison")
    ),
    list(
      id = scalar("area"),
      column_id = scalar("area_id"),
      options = json_verbatim("null"),
      use_shape_regions = scalar(TRUE)
    ),
    list(
      id = scalar("detail"),
      column_id = scalar("area_level"),
      options = get_area_level_filters(data)
    ),
    list(
      id = scalar("period"),
      column_id = scalar("calendar_quarter"),
      options = get_quarter_filters(data)
    ),
    list(
      id = scalar("sex"),
      column_id = scalar("sex"),
      options = get_sex_filters(data)
    ),
    list(
      id = scalar("age"),
      column_id = scalar("age_group"),
      options = get_age_filters(data)
    ),
    list(
      id = scalar("source"),
      column_id = scalar("source"),
      options = get_source_filters(data)
    )
  )
}

#' Get selected id-label mapping from list of filter options or column mappings
#'
#' Gets the id to label mapping of a particular type matching a set of IDs.
#' If ids are NULL then this returns all options.
#'
#' @param mappings The full set of output filters or column mappings
#' @param type The type of column to get mappings for, area, age, sex, quarter
#' @param ids Set of IDs to return, or NULL for all options
#' @param key The key to use for individual options. For filter mappings this
#'   is "options" for column mapping this is "value"
#'
#' @return The selected mappings
#' @keywords internal
get_selected_mappings <- function(mappings, type, ids = NULL, key = "options") {
  selected <- NULL
  for (mapping in mappings) {
    if (mapping$id == type) {
      selected <- mapping[[key]]
      break
    }
  }
  if (is.null(selected)) {
    stop(t_("MAPPING_NO_MATCHING", list(type = type)))
  }
  if (!is.null(ids)) {
    keep_mapping <- vapply(selected, function(mapping) {
      mapping$id %in% ids
    }, logical(1))
    selected <- selected[keep_mapping]
  }
  selected
}

get_quarter_filters <- function(data) {
  calendar_quarters <- unique(data$calendar_quarter)
  calendar_quarters <- sort(calendar_quarters, decreasing = TRUE)
  lapply(calendar_quarters, function(quarter) {
    list(id = scalar(as.character(quarter)),
         label = scalar(get_quarter_label(quarter)))
  })
}

get_quarter_label <- function(calendar_quarter) {
  naomi::calendar_quarter_labels(calendar_quarter)
}

get_year_filters <- function(data) {
  years <- unique(data$year)
  years <- sort(years, decreasing = TRUE)
  lapply(years, function(year) {
    list(id = scalar(as.character(year)),
         label = scalar(as.character(year)))
  })
}

get_level_labels <- function(json) {
  labels <- lapply(json$features, function(feature) {
    display <- feature$properties$display %||% TRUE
    list(id = scalar(feature$properties$area_level),
         area_level_label = scalar(feature$properties$area_level_label),
         display = scalar(as.logical(display)))
  })
  unique(labels)
}

get_region_filters <- function(json) {
  cols <- list(area_id = NA_character_,
               parent_area_id = NA_character_,
               area_sort_order = NA_real_,
               area_name = NA_character_)
  extract <- function(name, default) {
    tryCatch({
      vapply(json$features, function(x)
        x$properties[[name]] %||% default, default)
    },
    error = function(e) {
      expected_type <- class(default)
      stop(t_("FILTERS_INCORRECT_TYPE",
              list(name = name, type = expected_type)))
    })
  }
  hierarchy_table <- data_frame(Map(extract, names(cols), cols))
  colnames(hierarchy_table) <- c("id", "parent_id", "sort_order", "label")

  construct_tree(hierarchy_table)
}

get_spectrum_region_filters <- function(data) {
  regions <- unique(data[, c("spectrum_region_code", "spectrum_region_name")])
  regions <- regions[order(regions$spectrum_region_code), ]
  lapply(seq_len(nrow(regions)), function(row_no) {
    row <- regions[row_no, ]
    list(id = scalar(as.character(row$spectrum_region_code)),
         label = scalar(as.character(row$spectrum_region_name)))
  })
}

get_data_type_filters <- function() {
  recursive_scalar(naomi::data_type_labels())
}

get_source_filters <- function(data) {
  sources <- sort(unique(data$source), decreasing = TRUE)
  lapply(sources, function(source) {
    list(id = scalar(source),
         label = scalar(source))
  })
}

#' Create an ordered tree from a data frame.
#'
#' Will create a tree as a nested list from a data frame with specified order.
#'
#' @param data Data frame with at minimum id, parent id and sort order columns
#' to construct tree from.
#' @param id_column Index of column containing IDs.
#' @param parent_id_column Index of column containing parent IDs.
#' @param sort_order_column Index of column containing sort orders.
#'
#' @return The tree represented as a list.
#' @keywords internal
construct_tree <- function(data, id_column = 1, parent_id_column = 2,
                           sort_order_column = 3) {
  root_node <- is.na(data[, parent_id_column])
  if (sum(root_node) != 1) {
    stop(t_("FILTERS_INCORRECT_TREE", list(n = sum(root_node))))
  }
  build_immediate_children <- function(current_node_id) {
    current_node <- data[, id_column] == current_node_id
    children_ids <- which(data[, parent_id_column] == data[current_node, id_column])
    children <- data[children_ids, ]
    ordered_ids <- children[order(children$sort_order), id_column]
    tree <- lapply(data[current_node, -c(parent_id_column, sort_order_column),
                        drop = FALSE], scalar)
    tree$children <- lapply(ordered_ids, build_immediate_children)
    tree
  }

  build_immediate_children(data[root_node, id_column])
}
