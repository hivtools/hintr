get_age_filters <- function(data) {
  ## Assuming data is a data frame with age_group_id
  filters <- get_age_labels(unique(data$age_group_id))
  sorted_filters <- filters[order(filters$age_group_sort_order), ]
  construct_filter <- function(age_group_id, age_group_label) {
    list(id = scalar(as.character(age_group_id)),
         name = scalar(age_group_label))
  }
  Map(construct_filter,
      sorted_filters$age_group_id, sorted_filters$age_group_label)
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
