test_that("get_programme_time_series returns data and filters", {
  programme <- file_object(file.path("testdata", "programme.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  out <- get_programme_time_series(programme, shape)

  expect_equal(names(out), c("data", "metadata"))
  expect_true(nrow(out$data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(out$data),
                  c("area_id", "area_name", "area_level", "area_level_label",
                    "time_step", "time_period", "plot", "value"))

  filters <- out$metadata$filters
  expect_length(filters, 3)
  expect_equal(filters[[1]]$id, scalar("plot_type"))
  expect_equal(filters[[1]]$column_id, scalar("plot"))
  expect_equal(filters[[1]]$label, scalar("Plot type"))
  expect_length(filters[[1]]$options, 5)
  expect_setequal(names(filters[[1]]$options[[1]]),
                  c("id", "label", "description"))
  expect_equal(filters[[2]]$id, scalar("area_level"))
  expect_equal(filters[[2]]$column_id, scalar("area_level_label"))
  expect_equal(filters[[2]]$label, scalar("Area level"))
  expect_length(filters[[2]]$options, 5)
  expect_equal(filters[[3]]$id, scalar("time_step"))
  expect_equal(filters[[3]]$column_id, scalar("time_step"))
  expect_equal(filters[[3]]$label, scalar("Time step"))
  expect_length(filters[[3]]$options, 1)

  expect_length(out$metadata$defaults, 1)
  selected_filters <- out$metadata$defaults$selected_filter_options
  expect_length(selected_filters, 3)
  expect_equal(names(selected_filters),
               c("plot_type", "area_level", "time_step"))
})

test_that("get_anc_time_series returns data and filters", {
  anc <- file_object(file.path("testdata", "anc.csv"))
  shape <- file_object(file.path("testdata", "malawi.geojson"))
  out <- get_anc_time_series(anc, shape)
  expect_equal(names(out), c("data", "metadata"))
  expect_true(nrow(out$data) > 100) ## Check that we have read out some data
  expect_setequal(colnames(out$data),
                  c("area_id", "area_name", "area_level", "area_level_label",
                    "age_group", "time_period", "time_step", "plot", "value"))

  filters <- out$metadata$filters
  expect_length(filters, 3)
  expect_equal(filters[[1]]$id, scalar("plot_type"))
  expect_equal(filters[[1]]$column_id, scalar("plot"))
  expect_equal(filters[[1]]$label, scalar("Plot type"))
  expect_length(filters[[1]]$options, 10)
  expect_setequal(names(filters[[1]]$options[[1]]),
                  c("id", "label", "description"))
  expect_equal(filters[[2]]$id, scalar("area_level"))
  expect_equal(filters[[2]]$column_id, scalar("area_level_label"))
  expect_equal(filters[[2]]$label, scalar("Area level"))
  expect_length(filters[[2]]$options, 5)
  expect_equal(filters[[3]]$id, scalar("age"))
  expect_equal(filters[[3]]$column_id, scalar("age_group"))
  expect_equal(filters[[3]]$label, scalar("Age"))
  expect_length(filters[[3]]$options, 1)

  expect_length(out$metadata$defaults, 1)
  selected_filters <- out$metadata$defaults$selected_filter_options
  expect_length(selected_filters, 3)
  expect_equal(names(selected_filters),
               c("plot_type", "area_level", "age"))
})
