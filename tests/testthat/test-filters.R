context("filters")

test_that("get_age_label correctly maps to label and returns useful error", {
  expect_equivalent(get_age_labels(11), data_frame(age_group_id = 11,
                                              age_group_label = "50-54",
                                              age_group_sort_order = 23))

  expect_equivalent(get_age_labels(c(11, 12, 13)),
                    data_frame(age_group_id = c(11, 12, 13),
                               age_group_label = c("50-54", "55-59", "60-64"),
                               age_group_sort_order = c(23, 24, 25)))
  expect_error(get_age_labels(-5), "Found 0 rows for age_group_id -5.")
  expect_error(get_age_labels(c(-5, 50)),
               "Found 0 rows for age_group_id -5, 50.")
})

test_that("get_age_filters gets available filter options in correct order", {
  data <- data_frame(test = c(1, 2, 3, 4, 5, 6, 7),
                     age_group_id = c(10, 27, 2, 10, 10, 10, 2))
  filters <- get_age_filters(data)
  expect_equal(filters, list(
    list(
      id = scalar("27"),
      name = scalar("35-49")
    ),
    list(
      id = scalar("2"),
      name = scalar("5-9")
    ),
    list(
      id = scalar("10"),
      name = scalar("45-49")
    )
  ))

  expect_equal(get_age_filters(NULL), list())
  expect_equal(get_age_filters(data.frame(age_group_id = NULL)), list())
})

test_that("get_survey_filters gets available filter options and sorts them", {
  data <- data_frame(test = c(1, 2, 3, 4, 5, 6, 7),
                     survey_id = c("MWI2004DHS", "MWI2004DHS", "MWI2004DHS",
                                   "surv2", "surv2", "test", "test"))
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

test_that("can construct sorted tree from data frame", {
  data <- data_frame(
    id = c("MWI", "MWI.1", "MWI.2", "MWI.1.1", "MWI.1.2"),
    parent_id = c(NA, "MWI", "MWI", "MWI.1", "MWI.1"),
    sort_order = c(1, 2, 3, 4, 5)
  )
  expected_tree <- list(
    id = scalar("MWI"),
    options = list(
      list(
        id = scalar("MWI.1"),
        options = list(
          list(
            id = scalar("MWI.1.1"),
            options = list()
          ),
          list(
            id = scalar("MWI.1.2"),
            options = list()
          )
        )
      ),
      list(
        id = scalar("MWI.2"),
        options = list()
      )
    )
  )
  expect_equal(construct_tree(data), expected_tree)

  data <- data_frame(
    id = c("MWI", "MWI.1", "MWI.2", "MWI.1.1", "MWI.1.2"),
    name = c("Malawi", "Northern", "Central", "Chitipa", "Karonga"),
    parent_id = c(NA, "MWI", "MWI", "MWI.1", "MWI.1"),
    sort_order = c(1, 2, 3, 4, 5)
  )
  expected_tree <- list(
    id = scalar("MWI"),
    name = scalar("Malawi"),
    options = list(
      list(
        id = scalar("MWI.1"),
        name = scalar("Northern"),
        options = list(
          list(
            id = scalar("MWI.1.1"),
            name = scalar("Chitipa"),
            options = list()
          ),
          list(
            id = scalar("MWI.1.2"),
            name = scalar("Karonga"),
            options = list()
          )
        )
      ),
      list(
        id = scalar("MWI.2"),
        name = scalar("Central"),
        options = list()
      )
    )
  )
  expect_equal(
    construct_tree(data, parent_id_column = 3, sort_order_column = 4),
    expected_tree)
})

test_that("construct tree creates tree in correct order", {
  data <- data_frame(
    id = c("MWI", "MWI.1", "MWI.2", "MWI.1.1", "MWI.1.2"),
    name = c("Malawi", "Northern", "Central", "Chitipa", "Karonga"),
    parent_id = c(NA, "MWI", "MWI", "MWI.1", "MWI.1"),
    sort_order = c(2, 3, 5, 4, 1)
  )
  tree <- construct_tree(data, parent_id_column = 3)
  ## Ordering is respected within the level
  expect_equal(tree$id, scalar("MWI"))
  expect_equal(tree$options[[1]]$id, scalar("MWI.1"))
  expect_equal(tree$options[[2]]$id, scalar("MWI.2"))
  expect_equal(tree$options[[1]]$options[[1]]$id, scalar("MWI.1.2"))
  expect_equal(tree$options[[1]]$options[[2]]$id, scalar("MWI.1.1"))
})

test_that("error thrown when tree can't be constructed", {
  data <- data_frame(
    id = c("MWI", "MWI.1", "MWI.2", "MWI.1.1", "MWI.1.2"),
    parent_id = c(NA, NA, "MWI", "MWI.1", "MWI.1")
  )
  expect_error(construct_tree(data),
               "Got 2 root nodes - tree must have 1 root.")
})
