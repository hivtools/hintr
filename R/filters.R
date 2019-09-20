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

get_model_output_filters <- function(data) {
  list(age = get_age_filters(data),
       quarter = get_quarter_filters(data),
       indicator = get_id_name_map(data, "indicator_id", "indicator_label"))
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

get_id_name_map <- function(data, id_column, name_column) {
  ids <- unique(data[, c(id_column, name_column)])
  if(nrow(unique(ids)) != length(unique(ids[, id_column]))) {
    stop("ID used more than once, ids must be unique.")
  }
  build_list <- function(id, name) {
    list(id = scalar(as.character(id)),
         name = scalar(name))
  }
  Map(build_list, ids[, 1], ids[, 2])
}
