test_that("get_programme_time_series returns data and columns", {
  programme <- file_object(file.path("testdata", "programme.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  out <- get_programme_time_series(programme, shape)

  expect_equal(names(out), c("data", "metadata", "warnings"))
  expect_true(nrow(out$data) > 100) ## Check that we have read out some data
  expect_setequal(
    colnames(out$data),
    c(
      "area_id", "area_name", "area_level", "area_hierarchy",
      "time_period", "quarter", "plot", "value"
    )
  )

  columns <- out$metadata$columns
  expect_length(columns, 6)
  expect_equal(columns[[1]]$id, scalar("plot_type"))
  expect_equal(columns[[1]]$column_id, scalar("plot"))
  expect_equal(columns[[1]]$label, scalar("Plot type"))
  expect_length(columns[[1]]$values, 19)
  expect_setequal(
    names(columns[[1]]$values[[1]]),
    c("id", "label", "description", "format", "accuracy")
  )
  expect_equal(columns[[2]]$id, scalar("area_level"))
  expect_equal(columns[[2]]$column_id, scalar("area_level"))
  expect_equal(columns[[2]]$label, scalar("Area level"))
  expect_length(columns[[2]]$values, 5)
  expect_equal(columns[[2]]$values[[1]]$id, scalar("0"))
  expect_equal(columns[[2]]$values[[1]]$label, scalar("Country"))
  expect_equal(columns[[3]]$id, scalar("time_period"))
  expect_equal(columns[[3]]$column_id, scalar("time_period"))
  expect_equal(columns[[3]]$label, scalar("Time period"))
  expect_length(columns[[3]]$values, 8)
  expect_equal(columns[[4]]$id, scalar("quarter"))
  expect_equal(columns[[4]]$column_id, scalar("quarter"))
  expect_equal(columns[[4]]$label, scalar("Quarter"))
  expect_length(columns[[4]]$values, 1)
  expect_equal(columns[[4]]$values[[1]], list(
    id = scalar("Q4"),
    label = scalar("Quarter 4")
  ))
  expect_equal(columns[[5]]$id, scalar("area"))
  expect_equal(columns[[5]]$column_id, scalar("area_id"))
  expect_equal(columns[[5]]$label, scalar("Area"))
  expect_equal(columns[[6]]$id, scalar("area_name"))
  expect_equal(columns[[6]]$column_id, scalar("area_name"))
  expect_equal(columns[[6]]$label, scalar("Area name"))
  expect_length(columns[[6]]$values, 40)

  area_values <- columns[[5]]$values
  expect_equal(names(area_values), c("id", "label", "children"))
  expect_equal(area_values$id, scalar("MWI"))
  expect_equal(area_values$label, scalar("Malawi - Demo"))
  expect_length(area_values$children, 3)
  expect_equal(area_values$children[[1]]$label, scalar("Northern"))
  expect_equal(area_values$children[[2]]$label, scalar("Central"))
  expect_equal(area_values$children[[3]]$label, scalar("Southern"))

  expect_length(out$metadata$defaults, 1)
  selected_filters <- out$metadata$defaults$selected_filter_options
  expect_length(selected_filters, 3)
  expect_equal(names(selected_filters), c("plot_type", "area_level", "quarter"))
})

test_that("get_anc_time_series returns data and columns", {
  anc <- file_object(file.path("testdata", "anc.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  out <- get_anc_time_series(anc, shape)
  expect_equal(names(out), c("data", "metadata", "warnings"))
  expect_true(nrow(out$data) > 100) ## Check that we have read out some data
  expect_setequal(
    colnames(out$data),
    c(
      "area_id", "area_name", "area_level", "area_hierarchy",
      "age_group", "time_period", "quarter", "plot", "value"
    )
  )

  columns <- out$metadata$columns
  expect_length(columns, 7)
  expect_equal(columns[[1]]$id, scalar("plot_type"))
  expect_equal(columns[[1]]$column_id, scalar("plot"))
  expect_equal(columns[[1]]$label, scalar("Plot type"))
  expect_length(columns[[1]]$values, 9)
  expect_setequal(
    names(columns[[1]]$values[[1]]),
    c("id", "label", "description", "format", "accuracy")
  )
  expect_equal(columns[[2]]$id, scalar("area_level"))
  expect_equal(columns[[2]]$column_id, scalar("area_level"))
  expect_equal(columns[[2]]$label, scalar("Area level"))
  expect_length(columns[[2]]$values, 5)
  expect_equal(columns[[2]]$values[[1]]$id, scalar("0"))
  expect_equal(columns[[2]]$values[[1]]$label, scalar("Country"))
  expect_equal(columns[[3]]$id, scalar("age"))
  expect_equal(columns[[3]]$column_id, scalar("age_group"))
  expect_equal(columns[[3]]$label, scalar("Age"))
  expect_length(columns[[3]]$values, 1)
  expect_equal(columns[[4]]$column_id, scalar("time_period"))
  expect_equal(columns[[4]]$label, scalar("Time period"))
  expect_length(columns[[4]]$values, 8)
  expect_equal(columns[[5]]$id, scalar("quarter"))
  expect_equal(columns[[5]]$column_id, scalar("quarter"))
  expect_equal(columns[[5]]$label, scalar("Quarter"))
  expect_length(columns[[5]]$values, 1)
  expect_equal(columns[[5]]$values[[1]], list(
    id = scalar("Q4"),
    label = scalar("Quarter 4")
  ))
  expect_equal(columns[[6]]$id, scalar("area"))
  expect_equal(columns[[6]]$column_id, scalar("area_id"))
  expect_equal(columns[[6]]$label, scalar("Area"))
  expect_equal(columns[[7]]$id, scalar("area_name"))
  expect_equal(columns[[7]]$column_id, scalar("area_name"))
  expect_equal(columns[[7]]$label, scalar("Area name"))
  expect_length(columns[[7]]$values, 40)

  area_values <- columns[[6]]$values
  expect_equal(names(area_values), c("id", "label", "children"))
  expect_equal(area_values$id, scalar("MWI"))
  expect_equal(area_values$label, scalar("Malawi - Demo"))
  expect_length(area_values$children, 3)
  expect_equal(area_values$children[[1]]$label, scalar("Northern"))
  expect_equal(area_values$children[[2]]$label, scalar("Central"))
  expect_equal(area_values$children[[3]]$label, scalar("Southern"))

  expect_length(out$metadata$defaults, 1)
  selected_filters <- out$metadata$defaults$selected_filter_options
  expect_length(selected_filters, 4)
  expect_equal(
    names(selected_filters),
    c("plot_type", "area_level", "age", "quarter")
  )
})

test_that("get_default_id_label_map errors if ID has more than 1 label", {
  data <- data.frame(
    id = c(1, 1, 2, 2),
    name = c("one", "one", "two", "three"),
    stringsAsFactors = FALSE
  )
  expect_error(
    get_default_id_label_map(data, "id", "name"),
    "Data is invalid, number of unique name values and id values is not equal"
  )
})
