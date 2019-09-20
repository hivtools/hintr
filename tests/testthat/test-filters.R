context("filters")

test_that("get_age_label correctly maps to label and returns useful error", {
  expect_equal(get_age_label(11), "50-54")
  expect_error(get_age_label(-5), "Found 0 rows matching age_group_id -5.")
  expect_error(get_age_label(50), "Found 0 rows matching age_group_id 50.")
})

test_that("get_age_filters gets available filter options", {
  data <- data.frame(test = c(1, 2, 3, 4, 5, 6, 7),
                     age_group_id = c(10, 27, 2, 10, 10, 10, 2),
                     stringsAsFactors = FALSE)
  filters <- get_age_filters(data)
  expect_equal(filters, list(
    list(
      id = scalar("10"),
      name = scalar("45-49")
    ),
    list(
      id = scalar("27"),
      name = scalar("35-49")
    ),
    list(
      id = scalar("2"),
      name = scalar("5-9")
    )
  ))

  expect_equal(get_age_filters(NULL), list())
  expect_equal(get_age_filters(data.frame(age_group_id = NULL)), list())
})

test_that("get_survey_filters gets available filter options and sorts them", {
  data <- data.frame(test = c(1, 2, 3, 4, 5, 6, 7),
                     survey_id = c("MWI2004DHS", "MWI2004DHS", "MWI2004DHS",
                                   "surv2", "surv2", "test", "test"),
                     stringsAsFactors = FALSE)
  filters <- get_survey_filters(data)
  expect_equal(filters, list(
    list(
      id = scalar("test"),
      name = scalar("test")
    ),
    list(
      id = scalar("surv2"),
      name = scalar("surv2")
    ),
    list(
      id = scalar("MWI2004DHS"),
      name = scalar("MWI2004DHS")
    )
  ))

  expect_equal(get_survey_filters(NULL), list())
  expect_equal(get_age_filters(data.frame(survey_id = NULL)), list())
})

test_that("get_id_name_map correctly builds map", {
  data <- data.frame(id = c(1, 1, 2, 3),
                     name = c("one", "one", "two", "three"),
                     stringsAsFactors = FALSE)
  expected_map <- list(
    list(
      id = scalar("1"),
      name = scalar("one")
    ),
    list(
      id = scalar("2"),
      name = scalar("two")
    ),
    list(
      id = scalar("3"),
      name = scalar("three")
    )
  )
  expect_equal(get_id_name_map(data, "id", "name"), expected_map)
})

test_that("get_id_name_map throws error if non-unique ids", {
  data <- data.frame(id = c(1, 1, 3),
                     name = c("one", "two", "three"),
                     stringsAsFactors = FALSE)
  expect_error(get_id_name_map(data, "id", "name"),
               "ID used more than once, ids must be unique.")
})

test_that("get_quarter_filters gets quarter names from ids", {
  data <- data.frame(quarter_id = c(465, 454))
  expected_filters <- list(
    list(
      id = scalar("465"),
      name = scalar("Jan-Mar 2016")
    ),
    list(
      id = scalar("454"),
      name = scalar("Apr-Jun 2013")
    )
  )
  expect_equal(get_quarter_filters(data), expected_filters)
})
