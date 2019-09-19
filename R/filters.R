get_age_filters <- function(data) {
  ## Assuming data is a data frame with age_group_id
  age_groups <- unique(data$age_group_id)
  lapply(age_groups, function(age_group_id) {
    list(id = scalar(as.character(age_group_id)),
         name = scalar(get_age_label(age_group_id)))
  })
}

get_age_label <- function(age_group_id) {
  age_groups <- naomi::get_age_groups()
  index <- which(age_groups$age_group_id == age_group_id)
  if (length(index) != 1) {
    stop(sprintf("Found %s rows matching age_group_id %s.",
                 length(index), age_group_id))
  }
  age_groups[index, "age_group_label"]
  ## TODO: sort filter order on metadata from naomi get_age_groups once
  ## that is available mrc-502
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
  extract <- function(x) {
    vcapply(x$properties[c("area_id", "parent_area_id", "area_name")],
            function(x) x %||% NA_character_)
  }
  hierarchy <- vapply(json$features, extract, character(3))
  rownames(hierarchy) <- c("id", "parent_id", "name")
  construct_tree(as.data.frame(t(hierarchy), stringsAsFactors = FALSE))
}

construct_tree <- function(data, id_column = 1, parent_id_column = 2) {
  root_node <- is.na(data[, parent_id_column])
  if (sum(root_node) != 1) {
    stop(sprintf("Got %s root nodes - tree must have 1 root.",
                 sum(root_node)))
  }

  build_immediate_children <- function(current_node_id) {
    current_node <- data[, id_column] == current_node_id
    children <- data[, parent_id_column] == data[current_node, id_column]
    tree <- lapply(data[current_node, -parent_id_column, drop = FALSE], scalar)
    tree$options <- lapply(which(children), function(child) {
      build_immediate_children(data[child, id_column])
    })
    tree
  }

  build_immediate_children(data[root_node, id_column])
}
