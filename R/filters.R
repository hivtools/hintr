get_age_filters <- function(data) {
  ## Assuming data is a data frame with age_group_id
  filters <- get_age_labels(unique(data$age_group_id))
  sorted_filters <- filters[order(filters$age_group_sort_order), ]
  construct_filter(sorted_filters, "age_group_id", "age_group_label")
}

construct_filter <- function(data, id, name) {
  lapply(rownames(data), function(row_number) {
    list(id = scalar(as.character(data[row_number, id])),
         name = scalar(data[row_number, name]))
  })
}

get_age_labels <- function(age_group_ids) {
  age_groups <- naomi::get_age_groups()
  groups <- age_groups$age_group_id %in% age_group_ids
  missing_ids <- setdiff(age_group_ids, age_groups$age_group_id)
  if (length(missing_ids) > 0) {
    stop(sprintf("Found 0 rows for age_group_id %s.",
                 collapse(missing_ids)))
  }
  age_groups[groups,
             c("age_group_id", "age_group_label", "age_group_sort_order")]
}

get_survey_filters <- function(data) {
  survey_ids <- sort(unique(data$survey_id), decreasing = TRUE)
  lapply(survey_ids, function(survey_id) {
    list(id = scalar(survey_id),
         name = scalar(survey_id))
  })
}

get_indicator_filters <- function(data, type) {
  ## Input data either long or wide format
  get_filters <- switch(
    type,
    "anc" = read_wide_indicator_filters,
    "programme" = read_wide_indicator_filters,
    "survey" = read_long_indicator_filters,
    stop(sprintf("Can't get indicator filters for data type %s", type)))
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
  list(age = get_age_filters(data),
       quarter = get_quarter_filters(data),
       indicators = get_id_name_map(data, "indicator_id"))
}

get_quarter_filters <- function(data) {
  quarter_ids <- unique(data$quarter_id)
  lapply(quarter_ids, function(quarter_id) {
    list(id = scalar(as.character(quarter_id)),
         name = scalar(get_quarter_name(quarter_id)))
  })
}

get_quarter_name <- function(quarter_id) {
  naomi::quarter_year_labels(quarter_id)
}

get_id_name_map <- function(data, id_column) {
  ids <- unique(data[, c(id_column)])
  build_list <- function(id) {
    hint_id <- get_hint_id(id)
    list(id = scalar(as.character(hint_id)),
         name = scalar(get_indicator_display_name(hint_id)))
  }
  id_name_pairs <- lapply(ids, build_list)
  unique(id_name_pairs)
}

get_indicator_display_name <- function(indicator_id) {
  metadata <- naomi::get_metadata()
  display_name <- metadata[metadata$indicator == indicator_id, "name"]
  if (length(unique(display_name)) != 1) {
    stop(sprintf("Failed to get display name for hint ID %s.", indicator_id))
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
    vapply(json$features, function(x)
      x$properties[[name]] %||% default, default)
  }
  hierarchy_table <- data_frame(Map(extract, names(cols), cols))
  colnames(hierarchy_table) <- c("id", "parent_id", "sort_order", "name")

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
    stop(sprintf("Got %s root nodes - tree must have 1 root.",
                 sum(root_node)))
  }
  build_immediate_children <- function(current_node_id) {
    current_node <- data[, id_column] == current_node_id
    children_ids <- which(data[, parent_id_column] == data[current_node, id_column])
    children <- data[children_ids, ]
    ordered_ids <- children[order(children$sort_order), id_column]
    tree <- lapply(data[current_node, -c(parent_id_column, sort_order_column),
                        drop = FALSE], scalar)
    tree$options <- lapply(ordered_ids, build_immediate_children)
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
    stop(sprintf("Failed to locate hint ID from naomi_id %s.", naomi_id))
  }
  hint_id
}
