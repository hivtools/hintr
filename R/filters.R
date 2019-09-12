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
}

get_survey_filters <- function(data) {
  survey_ids <- unique(data$survey_id)
  lapply(survey_ids, function(survey_id) {
    list(id = scalar(survey_id),
         name = scalar(survey_id))
  })
}
