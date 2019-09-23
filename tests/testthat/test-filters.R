context("filters")

test_that("get_age_label correctly maps to label and returns useful error", {
  expect_equal(get_age_label(11), "50-54")
  expect_error(get_age_label(-5), "Found 0 rows matching age_group_id -5.")
  expect_error(get_age_label(50), "Found 0 rows matching age_group_id 50.")
})

test_that("get_age_filters gets available filter options", {
  data <- data_frame(test = c(1, 2, 3, 4, 5, 6, 7),
                     age_group_id = c(10, 27, 2, 10, 10, 10, 2))
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
