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
      label = scalar(t_("OUTPUT_FILTER_AREA")),
      options = json_verbatim("null"),
      use_shape_regions = scalar(TRUE)
    ),
    list(
      id = scalar("quarter"),
      column_id = scalar("calendar_quarter"),
      label = scalar(t_("OUTPUT_FILTER_PERIOD")),
      options = get_quarter_filters(data)
    ),
    list(
      id = scalar("sex"),
      column_id = scalar("sex"),
      label = scalar(t_("OUTPUT_FILTER_SEX")),
      options = get_sex_filters(data)
    ),
    list(
      id = scalar("age"),
      column_id = scalar("age_group"),
      label = scalar(t_("OUTPUT_FILTER_AGE")),
      options = get_age_filters(data)
    )
  )
}

get_barchart_defaults <- function(output, output_filters) {
  list(
    indicator_id = scalar("prevalence"),
    x_axis_id = scalar("age"),
    disaggregate_by_id = scalar("sex"),
    selected_filter_options = list(
      area = get_area_level_filter_option(output),
      quarter = get_selected_filter_options(output_filters, "quarter")[2],
      sex = get_selected_filter_options(output_filters, "sex",
                                        c("female", "male")),
      age = get_selected_filter_options(output_filters, "age",
                                        naomi::get_five_year_age_groups())
    )
  )
}

#' Get selected filter options from full list of output filters.
#'
#' Gets the filter options of a particular type matching a set of IDs.
#' If ids are NULL then this returns all options.
#'
#' @param output_filters The full set of output filters
#' @param filter_type The type of filter to get options for, area, age, sex, quarter
#' @param ids Set of option IDs to reutrn, or NULL for all options
#'
#' @return The selected filter options
#' @keywords internal
get_selected_filter_options <- function(output_filters, filter_type, ids = NULL) {
  options <- NULL
  for (filter in output_filters) {
    if (filter$id == filter_type) {
      options <- filter$options
      break
    }
  }
  if (is.null(options)) {
    stop(t_("FILTERS_NO_MATCHING", list(type = filter_type)))
  }
  if (!is.null(ids)) {
    keep_option <- vapply(options, function(option) {
      option$id %in% ids
    }, logical(1))
    options <- options[keep_option]
  }
  options
}

get_area_level_filter_option <- function(output) {
  ## We expect the areas to be returned in order - return the first region
  ## level as the default
  option <- output[1, c("area_id", "area_name")]
  list(
    list(
      id = scalar(option$area_id),
      label = scalar(option$area_name)
    )
  )
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

get_indicator_display_name <- function(indicator_id) {
  metadata <- naomi::get_metadata()
  display_name <- metadata[metadata$indicator == indicator_id, "name"]
  if (length(unique(display_name)) != 1) {
    stop(t_("FILTERS_NO_DISPLAY_NAME", list(id = indicator_id)))
  }
  display_name[[1]]
}

get_level_labels <- function(json) {
  labels <- lapply(json$features, function(feature) {
    list(id = scalar(feature$properties$area_level),
         area_level_label = scalar(feature$properties$area_level_label),
         display = scalar(as.logical(feature$properties$display)))
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
      expected_type <- typeof(default)
      if (expected_type == "double") {
        expected_type <- "numeric"
      }
      stop(t_("FILTERS_INCORRECT_TYPE",
              list(name = name, type = expected_type)))
    })
  }
  hierarchy_table <- data_frame(Map(extract, names(cols), cols))
  colnames(hierarchy_table) <- c("id", "parent_id", "sort_order", "label")

  construct_tree(hierarchy_table)
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

#' Get hint ID from naomi ID
#'
#' @param naomi_id The ID used in Naomi to identify an indicator.
#'
#' @return The ID as used in hint.
#'
#' @keywords internal
get_hint_id <- function(naomi_id) {
  metadata <- naomi::get_metadata()
  if (naomi_id %in% metadata$indicator) {
    hint_id <- naomi_id
  } else if (naomi_id %in% metadata$indicator_value) {
    hint_id <- metadata[metadata$indicator_value ==
                          as.character(naomi_id), "indicator"]
  } else {
    stop(t_("FILTERS_CANT_GET_HINT_ID", list(id = naomi_id)))
  }
  hint_id[[1]]
}
