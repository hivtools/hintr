context("filters")

test_that("get_age_label correctly maps to label and returns useful error", {
  expect_equivalent(get_age_labels("50-54"), data_frame(age_group = "50-54",
                                              age_group_label = "50-54",
                                              age_group_sort_order = 23))

  expect_equivalent(get_age_labels(c("00-04", "15-19", "50-54")),
                    data_frame(age_group = c("00-04", "15-19", "50-54"),
                               age_group_label = c("00-04", "15-19", "50-54"),
                               age_group_sort_order = c(13, 16, 23)))
  expect_error(get_age_labels("00-90"),
               "Age groups metadata contains 0 rows for age_group 00-90. Speak to administrator.")
  expect_error(get_age_labels(c("00-90", "-20-09")),
               "Age groups metadata contains 0 rows for age_group 00-90, -20-09. Speak to administrator.")
})

test_that("get_age_filters gets available filter options in correct order", {
  data <- data_frame(test = c(1, 2, 3, 4, 5, 6, 7),
                     age_group = c("45-49", "35-49", "05-09", "45-49", "45-49",
                                   "45-49", "05-09"))
  filters <- get_age_filters(data)
  expect_equal(filters, list(
    list(
      id = scalar("35-49"),
      label = scalar("35-49")
    ),
    list(
      id = scalar("05-09"),
      label = scalar("05-09")
    ),
    list(
      id = scalar("45-49"),
      label = scalar("45-49")
    )
  ))

  expect_equal(get_age_filters(NULL), list())
  expect_equal(get_age_filters(data.frame(age_group = NULL)), list())
})

test_that("get_survey_filters gets available filter options and sorts them", {
  data <- data_frame(test = c(1, 2, 3, 4, 5, 6, 7),
                     survey_id = c("MWI2004DHS", "MWI2004DHS", "MWI2004DHS",
                                   "surv2", "surv2", "test", "test"))
  filters <- get_survey_filters(data)
  expect_equal(filters, list(
    list(
      id = scalar("test"),
      label = scalar("test")
    ),
    list(
      id = scalar("surv2"),
      label = scalar("surv2")
    ),
    list(
      id = scalar("MWI2004DHS"),
      label = scalar("MWI2004DHS")
    )
  ))

  expect_equal(get_survey_filters(NULL), list())
  expect_equal(get_age_filters(data.frame(survey_id = NULL)), list())
})

test_that("get_quarter_filters gets quarter names from ids", {
  data <- data.frame(calendar_quarter = c("CY2013Q2", "CY2016Q1"))
  expected_filters <- list(
    list(
      id = scalar("CY2016Q1"),
      label = scalar("March 2016")
    ),
    list(
      id = scalar("CY2013Q2"),
      label = scalar("June 2013")
    )
  )
  expect_equal(get_quarter_filters(data), expected_filters)
})

test_that("get_year_filters returns year labels and ids", {
  data <- data.frame(year = c(2010, 2013, 2016))
  expected_filters <- list(
    list(
      id = scalar("2016"),
      label = scalar("2016")
    ),
    list(
      id = scalar("2013"),
      label = scalar("2013")
    ),
    list(
      id = scalar("2010"),
      label = scalar("2010")
    )
  )
  expect_equal(get_year_filters(data), expected_filters)
})

test_that("can construct sorted tree from data frame", {
  data <- data_frame(
    id = c("MWI", "MWI_1_1", "MWI_1_2", "MWI_2_1", "MWI_2_2"),
    parent_id = c(NA, "MWI", "MWI", "MWI_1_1", "MWI_1_1"),
    sort_order = c(1, 2, 3, 4, 5)
  )
  expected_tree <- list(
    id = scalar("MWI"),
    children = list(
      list(
        id = scalar("MWI_1_1"),
        children = list(
          list(
            id = scalar("MWI_2_1"),
            children = list()
          ),
          list(
            id = scalar("MWI_2_2"),
            children = list()
          )
        )
      ),
      list(
        id = scalar("MWI_1_2"),
        children = list()
      )
    )
  )
  expect_equal(construct_tree(data), expected_tree)

  data <- data_frame(
    id = c("MWI", "MWI_1_1", "MWI_1_2", "MWI_2_1", "MWI_2_2"),
    label = c("Malawi", "Northern", "Central", "Chitipa", "Karonga"),
    parent_id = c(NA, "MWI", "MWI", "MWI_1_1", "MWI_1_1"),
    sort_order = c(1, 2, 3, 4, 5)
  )
  expected_tree <- list(
    id = scalar("MWI"),
    label = scalar("Malawi"),
    children = list(
      list(
        id = scalar("MWI_1_1"),
        label = scalar("Northern"),
        children = list(
          list(
            id = scalar("MWI_2_1"),
            label = scalar("Chitipa"),
            children = list()
          ),
          list(
            id = scalar("MWI_2_2"),
            label = scalar("Karonga"),
            children = list()
          )
        )
      ),
      list(
        id = scalar("MWI_1_2"),
        label = scalar("Central"),
        children = list()
      )
    )
  )
  expect_equal(
    construct_tree(data, parent_id_column = 3, sort_order_column = 4),
    expected_tree)
})

test_that("construct tree creates tree in correct order", {
  data <- data_frame(
    id = c("MWI", "MWI_1_1", "MWI_1_2", "MWI_2_1", "MWI_2_2"),
    label = c("Malawi", "Northern", "Central", "Chitipa", "Karonga"),
    parent_id = c(NA, "MWI", "MWI", "MWI_1_1", "MWI_1_1"),
    sort_order = c(2, 3, 5, 4, 1)
  )
  tree <- construct_tree(data, parent_id_column = 3)
  ## Ordering is respected within the level
  expect_equal(tree$id, scalar("MWI"))
  expect_equal(tree$children[[1]]$id, scalar("MWI_1_1"))
  expect_equal(tree$children[[2]]$id, scalar("MWI_1_2"))
  expect_equal(tree$children[[1]]$children[[1]]$id, scalar("MWI_2_2"))
  expect_equal(tree$children[[1]]$children[[2]]$id, scalar("MWI_2_1"))
})

test_that("error thrown when tree can't be constructed", {
  data <- data_frame(
    id = c("MWI", "MWI_1_1", "MWI_1_2", "MWI_2_1", "MWI_2_2"),
    parent_id = c(NA, NA, "MWI", "MWI_1_1", "MWI_1_1")
  )
  expect_error(construct_tree(data),
               "Got 2 root nodes - tree must have 1 root.")
})

test_that("naomi IDs can be mapped to hint IDs", {
  expect_equal(get_hint_id(2), "prevalence")
  expect_equal(get_hint_id("2"), "prevalence")
  expect_equal(get_hint_id("prev"), "prevalence")
  expect_equal(get_hint_id("art_coverage"), "art_coverage")
  expect_error(get_hint_id("missing"),
               "Failed to locate hint ID from naomi_id missing.")
})

test_that("can get indicator display name", {
  expect_equal(get_indicator_display_name("vls"), "Viral load suppression")
  expect_equal(get_indicator_display_name("prevalence"), "HIV prevalence")
  expect_error(get_indicator_display_name("missing"),
               "Failed to get display name for hint ID missing.")
})

test_that("can get indicator filters for survey data", {
  survey_path <- file.path("testdata", "survey.csv")
  survey <- read_csv(survey_path, header = TRUE)
  filters <- get_indicator_filters(survey, "survey")

  expect_length(filters, 4)
  expect_equal(filters[[1]]$id, scalar("prevalence"))
  expect_equal(filters[[1]]$label, scalar("HIV prevalence"))
  expect_equal(filters[[2]]$id, scalar("art_coverage"))
  expect_equal(filters[[2]]$label, scalar("ART coverage"))
  expect_equal(filters[[3]]$id, scalar("recent"))
  expect_equal(filters[[3]]$label, scalar("Proportion recently infected"))
  expect_equal(filters[[4]]$id, scalar("vls"))
  expect_equal(filters[[4]]$label, scalar("Viral load suppression"))
})

test_that("can get indicator filters for programme data", {
  programme_path <- file.path("testdata", "programme.csv")
  programme <- read_csv(programme_path, header = TRUE)
  filters <- get_indicator_filters(programme, "programme")

  expect_length(filters, 1)
  expect_equal(filters[[1]]$id, scalar("current_art"))
  expect_equal(filters[[1]]$label, scalar("ART number (attending)"))
})

test_that("can get indicator filters for anc data", {
  anc_path <- file.path("testdata", "anc.csv")
  anc <- read_csv(anc_path, header = TRUE)
  ## We will have calculated prev and art coverage for ANC data
  anc <- naomi::calculate_prevalence_art_coverage(anc)
  filters <- get_indicator_filters(anc, "anc")

  expect_length(filters, 2)
  expect_equal(filters[[1]]$id, scalar("prevalence"))
  expect_equal(filters[[1]]$label, scalar("HIV prevalence"))
  expect_equal(filters[[2]]$id, scalar("art_coverage"))
  expect_equal(filters[[2]]$label, scalar("ART coverage"))
})

test_that("error thrown for unknown type", {
  data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  expect_error(get_indicator_filters(data, "unknown"),
               "Can't get indicator filters for data type unknown.")
})

test_that("can get area filter option", {
  test_mock_model_available()
  output <- readRDS(mock_model$output_path)
  expect_equal(get_area_level_filter_option(output), list(
    list(
      id = scalar("MWI"),
      label = scalar("Malawi")
    )
  ))

  output$area_name[[1]] <- "test"
  expect_equal(get_area_level_filter_option(output), list(
    list(
      id = scalar("MWI"),
      label = scalar("test")
    )
  ))

  output <- output[output$area_level != 0, ]
  expect_equal(get_area_level_filter_option(output), list(
    list(
      id = scalar("MWI_1_1"),
      label = scalar("Northern")
    )
  ))
})

test_that("can get defaults for bar chart", {
  test_mock_model_available()
  output <- readRDS(mock_model$output_path)
  filters <- get_model_output_filters(output)
  defaults <- get_barchart_defaults(output, filters)

  expect_equal(names(defaults), c("indicator_id", "x_axis_id",
                                  "disaggregate_by_id",
                                  "selected_filter_options"))
  expect_equal(names(defaults$selected_filter_options),
               c("area", "quarter", "sex", "age"))
  expect_equal(defaults$selected_filter_options$area, list(
    list(
      id = scalar("MWI"),
      label = scalar("Malawi")
    )
  ))
  expect_equal(defaults$selected_filter_options$quarter, list(
    list(
      id = scalar("CY2018Q3"),
      label = scalar("September 2018")
    )
  ))
  expect_equal(defaults$selected_filter_options$sex, list(
    list(
      id = scalar("female"),
      label = scalar("Female")
    ),
    list(
      id = scalar("male"),
      label = scalar("Male")
    )
  ))
  age_options <- defaults$selected_filter_options$age
  expect_length(age_options, 17)
  expect_equal(age_options[[1]], list(
    id = scalar("00-04"),
    label = scalar("00-04")
  ))
  expect_equal(age_options[[length(age_options)]], list(
    id = scalar("80+"),
    label = scalar("80+")
  ))
})

test_that("can get selected filter options", {
  test_mock_model_available()
  output <- readRDS(mock_model$output_path)
  filters <- get_model_output_filters(output)
  selected_options <- get_selected_filter_options(filters, "age", "00-04")
  expect_equal(selected_options, list(
    list(
      id = scalar("00-04"),
      label = scalar("00-04")
    )
  ))

  selected_options <- get_selected_filter_options(filters, "sex")
  expect_equal(selected_options, list(
    list(
      id = scalar("both"),
      label = scalar("Both")
    ),
    list(
      id = scalar("female"),
      label = scalar("Female")
    ),
    list(
      id = scalar("male"),
      label = scalar("Male")
    )
  ))

  expect_error(get_selected_filter_options(filters, "test"),
               "Found no matching filters for type test")

  selected_options <- get_selected_filter_options(filters, "sex",
                                                  c("male", "test"))
  expect_equal(selected_options, list(
    list(
      id = scalar("male"),
      label = scalar("Male")
    )
  ))
})
