get_filter_option_mocks <- function() {
  list(
    get_area_level_filters = mock("area_level_filters"),
    get_quarter_filters = mock("quarter_filters"),
    get_sex_filters = mock("sex_filters"),
    get_age_filters = mock("age_filters"),
    get_indicator_options = mock("indicator_filters")
  )
}

test_that("get_age_label correctly maps to label and returns useful error", {
  expect_equal(get_age_labels("Y050_054"),
               data_frame(age_group = "Y050_054",
                          age_group_label = "50-54",
                          age_group_sort_order = 25),
               ignore_attr = TRUE)

  expect_equal(get_age_labels(c("Y000_004", "Y015_019", "Y050_054")),
               data_frame(age_group = c("Y000_004", "Y015_019", "Y050_054"),
                          age_group_label = c("0-4", "15-19", "50-54"),
                          age_group_sort_order = c(15, 18, 25)),
               ignore_attr = TRUE)
  expect_error(get_age_labels("Y000_090"),
               "Age groups metadata contains 0 rows for age_group Y000_090. Speak to administrator.")
  expect_error(get_age_labels(c("Y000_090", "-20-09")),
               "Age groups metadata contains 0 rows for age_group Y000_090, -20-09. Speak to administrator.")
})

test_that("get_age_filters gets available filter options in correct order", {
  data <- data_frame(test = c(1, 2, 3, 4, 5, 6, 7),
                     age_group = c("Y045_049", "Y035_049", "Y005_009", "Y045_049", "Y045_049",
                                   "Y045_049", "Y005_009"))
  filters <- get_age_filters(data)
  expect_equal(filters, list(
    list(
      id = scalar("Y035_049"),
      label = scalar("35-49")
    ),
    list(
      id = scalar("Y005_009"),
      label = scalar("5-9")
    ),
    list(
      id = scalar("Y045_049"),
      label = scalar("45-49")
    )
  ))

  expect_equal(get_age_filters(NULL), list())
  expect_equal(get_age_filters(data.frame(age_group = NULL)), list())
})

test_that("get_survey_filters gets available filter options and sorts them", {
  data <- data_frame(test = c(1, 2, 3, 4, 5, 6, 7),
                     survey_id = c("DEMO2004DHS", "DEMO2004DHS", "DEMO2004DHS",
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
      id = scalar("DEMO2004DHS"),
      label = scalar("DEMO2004DHS")
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

test_that("can get indicator filters for survey data", {
  survey_path <- file.path("testdata", "survey.csv")
  survey <- read_csv(survey_path, header = TRUE)
  filters <- get_indicator_filters(survey, "survey")

  expect_length(filters, 4)
  expect_equal(filters[[1]]$id, scalar("prevalence"))
  expect_equal(filters[[1]]$label, scalar("HIV prevalence"))
  expect_equal(filters[[2]]$id, scalar("art_coverage"))
  expect_equal(filters[[2]]$label, scalar("ART coverage"))
  expect_equal(filters[[3]]$id, scalar("recent_infected"))
  expect_equal(filters[[3]]$label, scalar("Proportion recently infected"))
  expect_equal(filters[[4]]$id, scalar("viral_suppression_plhiv"))
  expect_equal(filters[[4]]$label, scalar("Viral load suppression"))
})

test_that("can get indicator filters for programme data", {
  programme_path <- file.path("testdata", "programme.csv")
  programme <- read_csv(programme_path, header = TRUE)
  filters <- get_indicator_filters(programme, "programme")

  expect_length(filters, 4)
  expect_equal(filters[[1]]$id, scalar("art_current"))
  expect_equal(filters[[1]]$label, scalar("ART number (attending)"))
  expect_equal(filters[[2]]$id, scalar("art_new"))
  expect_equal(filters[[2]]$label, scalar("ART new"))
  expect_equal(filters[[3]]$id, scalar("vl_tested_12mos"))
  expect_equal(filters[[3]]$label, scalar("VL tested"))
  expect_equal(filters[[4]]$id, scalar("vl_suppressed_12mos"))
  expect_equal(filters[[4]]$label, scalar("VL tests suppressed"))
})

test_that("can get indicator filters for anc data", {
  anc_path <- file.path("testdata", "anc.csv")
  anc <- read_csv(anc_path, header = TRUE)
  ## We will have calculated prev and art coverage for ANC data
  anc <- naomi::calculate_prevalence_art_coverage(anc)
  filters <- get_indicator_filters(anc, "anc")

  expect_length(filters, 2)
  expect_equal(filters[[1]]$id, scalar("anc_prevalence"))
  expect_equal(filters[[1]]$label, scalar("ANC HIV prevalence"))
  expect_equal(filters[[2]]$id, scalar("anc_art_coverage"))
  expect_equal(filters[[2]]$label, scalar("ANC prior ART coverage"))
})

test_that("error thrown for unknown type", {
  data <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  expect_error(get_indicator_filters(data, "unknown"),
               "Can't get indicator filters for data type unknown.")
})

test_that("can get selected filter options", {
  test_mock_model_available()
  output <- naomi::read_hintr_output(mock_calibrate$plot_data_path)
  filters <- get_model_output_filters(output)
  selected_options <- get_selected_mappings(filters, "age", "Y000_004")
  expect_equal(selected_options, list(
    list(
      id = scalar("Y000_004"),
      label = scalar("0-4")
    )
  ))

  selected_options <- get_selected_mappings(filters, "sex")
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

  expect_error(get_selected_mappings(filters, "test"),
               "Found no matching filters for type test")

  selected_options <- get_selected_mappings(filters, "sex", c("male", "test"))
  expect_equal(selected_options, list(
    list(
      id = scalar("male"),
      label = scalar("Male")
    )
  ))
})

test_that("get_spectrum_region_filters gets regions from data", {
  data <- data.frame(spectrum_region_code = c(0, 0, 1, 1),
                     spectrum_region_name = c("Northern", "Northern",
                                              "Southern", "Southern"),
                     value = c(1, 2, 3, 4))
  filters <- get_spectrum_region_filters(data)
  expect_equal(length(filters), 2)
  expect_equal(filters[[1]]$id, scalar("0"))
  expect_equal(filters[[1]]$label, scalar("Northern"))
  expect_equal(filters[[2]]$id, scalar("1"))
  expect_equal(filters[[2]]$label, scalar("Southern"))
})

test_that("can get model output filters", {
  mocks <- get_filter_option_mocks()
  filters <- call_with_mocks_object({
    get_model_output_filters("test_data")
  }, mocks)
  expect_args(mocks$get_area_level_filters, 1, "test_data")
  expect_args(mocks$get_quarter_filters, 1, "test_data")
  expect_args(mocks$get_sex_filters, 1, "test_data")
  expect_args(mocks$get_age_filters, 1, "test_data")

  expect_equal(filters,
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
        options = "area_level_filters"
      ),
      list(
        id = scalar("period"),
        column_id = scalar("calendar_quarter"),
        options = "quarter_filters"
      ),
      list(
        id = scalar("sex"),
        column_id = scalar("sex"),
        options = "sex_filters"
      ),
      list(
        id = scalar("age"),
        column_id = scalar("age_group"),
        options = "age_filters"
      ),
      list(
        id = scalar("indicator"),
        column_id = scalar("indicator"),
        options = "indicator_filters"
      )
    )
  )
})
