test_that("can build metadata response", {
  metadata <- do_plotting_metadata("MWI")
  expect_true(all(names(metadata) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(metadata$survey), "choropleth")
  expect_equal(names(metadata$anc), "choropleth")
  expect_equal(names(metadata$output), c("barchart", "choropleth"))
  expect_equal(names(metadata$programme), "choropleth")
  expect_length(metadata$anc$choropleth$indicators, 2)
  expect_equal(metadata$anc$choropleth$indicators[[1]]$indicator,
               scalar("anc_prevalence"))
  expect_equal(metadata$anc$choropleth$indicators[[2]]$indicator,
               scalar("anc_art_coverage"))
  expect_equal(metadata$anc$choropleth$indicators[[1]]$name,
               scalar("ANC HIV prevalence"))
  expect_equal(metadata$anc$choropleth$indicators[[2]]$name,
               scalar("ANC prior ART coverage"))
  expect_length(metadata$programme$choropleth$indicators, 4)
  programme_indicators <- metadata$programme$choropleth$indicators
  expect_equal(programme_indicators[[1]]$indicator, scalar("art_current"))
  expect_equal(programme_indicators[[1]]$name,
               scalar("ART number (attending)"))
  expect_equal(programme_indicators[[2]]$indicator, scalar("art_new"))
  expect_equal(programme_indicators[[2]]$name, scalar("ART new"))
  expect_equal(programme_indicators[[3]]$indicator, scalar("vl_tested_12mos"))
  expect_equal(programme_indicators[[3]]$name, scalar("VL tested"))
  expect_equal(programme_indicators[[4]]$indicator, scalar("vl_suppressed_12mos"))
  expect_equal(programme_indicators[[4]]$name, scalar("VL tests suppressed"))
})

test_that("error thrown when metadata contains conflicting information", {
  mock_get_plotting_metadata <- mock(data_frame(
    data_type = rep("survey", 2),
    plot_type = rep("choropleth", 2),
    indicator = rep("prevalence", 2),
    indicator_sort_order = c(1, 2)
  ))
  with_mocked_bindings(
    expect_error(
      do_plotting_metadata("Malawi"),
      "Expected only 1 row for indicator, data type, plot type combination.
Check each combination is unique in configuration."),
    get_plotting_metadata = mock_get_plotting_metadata)
})

test_that("can get plot settings control", {
  stub(get_plot_settings_control, "get_choropleth_settings", "choropleth_settings")
  stub(get_plot_settings_control, "get_barchart_settings", "barchart_settings")
  stub(get_plot_settings_control, "get_table_settings", "table_settings")
  stub(get_plot_settings_control, "get_bubble_settings", "bubble_settings")

  expect_equal(get_plot_settings_control(),
    list(
      choropleth = "choropleth_settings",
      barchart = "barchart_settings",
      table = "table_settings",
      bubble = "bubble_settings"
    )
  )
})

test_that("can get choropleth settings", {
  mocks <- get_filter_mocks()
  expected_filter_calls <- get_mock_args_from_vector(c("indicator", "detail", "area", "period", "sex", "age"))
  choropleth_settings <- call_with_mocks_object({
    get_choropleth_settings()
  }, mocks)
  expect_equal(mock_args(mocks$get_filter_from_id), expected_filter_calls)
  expect_equal(choropleth_settings,
    list(
      defaultFilterTypes = rep(list("filter_ref"), 6),
      plotSettings = list(
        list(
          id = scalar("default"),
          label = scalar(""),
          options = list(
            list(
              id = scalar(""),
              label = scalar(""),
              effect = list(
                setMultiple = "area"
              )
            )
          )
        )
      )
    )
  )
})

test_that("can get barchart settings", {
  mocks <- get_filter_mocks()
  base_filter_ids <- c("area", "period", "sex", "age")
  all_filter_ids <- c(c("indicator", "detail"), base_filter_ids)
  expected_x_axis_or_disagg_by_calls <- get_mock_args_from_vector(base_filter_ids)
  expected_filter_calls <- get_mock_args_from_vector(all_filter_ids)
  barchart_settings <- call_with_mocks_object({
      get_barchart_settings()
  }, mocks)
  expect_equal(mock_args(mocks$get_x_axis_or_disagg_by_option), expected_x_axis_or_disagg_by_calls)
  expect_equal(mock_args(mocks$get_filter_from_id), expected_filter_calls)
  expect_equal(barchart_settings,
    list(
      defaultFilterTypes = rep(list("filter_ref"), 6),
      plotSettings = list(
        list(
          id = scalar("x_axis"),
          label = scalar("X Axis"),
          options = rep(list("x_axis_or_disagg_by_option"), 4)
        ),
        list(
          id = scalar("disagg_by"),
          label = scalar("Disaggregate By"),
          options = rep(list("x_axis_or_disagg_by_option"), 4)
        )
      )
    )
  )
})

test_that("can get table settings", {
  stub(get_table_settings, "get_table_presets", "table_presets")
  table_settings <- get_table_settings()
  expect_equal(table_settings,
    list(
      plotSettings = list(
        list(
          id = scalar("presets"),
          label = scalar("Presets"),
          options = "table_presets"
        )
      )
    )
  )
})

test_that("can get bubble settings", {
  mocks <- get_filter_mocks()
  bubble_settings <- call_with_mocks_object({
      get_bubble_settings()
  }, mocks)
  expected_filter_calls <- get_mock_args_from_vector(c("detail", "area", "period", "sex", "age"))
  expect_equal(mock_args(mocks$get_filter_from_id), expected_filter_calls)
  expected_indicators <- list(
    list(
      filterId = scalar("indicator"),
      label = scalar("Size Indicator"),
      stateFilterId = scalar("sizeIndicator")
    ),
    list(
      filterId = scalar("indicator"),
      label = scalar("Colour Indicator"),
      stateFilterId = scalar("colourIndicator")
    )
  )
  expect_equal(bubble_settings,
    list(
      defaultFilterTypes = append(expected_indicators, rep(list("filter_ref"), 5)),
      plotSettings = list(
        list(
          id = scalar("default"),
          label = scalar(""),
          options = list(
            list(
              id = scalar(""),
              label = scalar(""),
              effect = list(
                setMultiple = "area"
              )
            )
          )
        )
      )
    )
  )
})

test_that("can get table presets", {
  mocks <- get_filter_mocks()
  stub(get_table_presets, "naomi::get_five_year_age_groups", "five_year_age_groups")
  expected_filter_calls_1 <- get_mock_args_from_vector(c("indicator", "detail", "period", "sex", "age"))
  expected_filter_calls_2 <- get_mock_args_from_vector(c("indicator", "area", "period", "sex", "age"))
  expected_filter_calls <- append(expected_filter_calls_1, expected_filter_calls_2)
  table_presets <- call_with_mocks_object({
      get_table_presets()
  }, mocks)
  expect_equal(mock_args(mocks$get_filter_from_id), expected_filter_calls)
  expect_equal(table_presets,
    list(
      list(
        id = scalar("sex_by_area"),
        label = scalar("Sex by area"),
        effect = list(
          setFilters = rep(list("filter_ref"), 5),
          setMultiple = c("sex")
        )
      ),
      list(
        id = scalar("sex_by_5_year_age_group"),
        label = scalar("Sex by 5 year age group"),
        effect = list(
          setFilters = rep(list("filter_ref"), 5),
          setMultiple = c("sex", "age"),
          setFilterValues = list(
            age = "five_year_age_groups"
          )
        )
      )
    )
  )
})

test_that("can get x axis or disagg by option", {
  stub(get_x_axis_or_disagg_by_option, "get_label_for_id", "label")
  option <- get_x_axis_or_disagg_by_option("test_id")
  expect_equal(option,
    list(
      id = scalar("test_id"),
      label = scalar("label"),
      effect = list(
        setMultiple = "test_id"
      )
    )
  )
})

test_that("can get filter from id", {
  stub(get_filter_from_id, "get_label_for_id", "label")
  filter <- get_filter_from_id("test_id")
  expect_equal(filter,
    list(
      filterId = scalar("test_id"),
      label = scalar("label"),
      stateFilterId = scalar("test_id")
    )
  )
})

test_that("can get label from id", {
  ids <- c("area", "period", "sex", "age", "detail", "indicator")
  labels <- c("Area", "Period", "Sex", "Age", "Area level", "Indicator")
  for (i in seq_along(ids)) {
    expect_equal(get_label_for_id(ids[[i]]), labels[[i]])
  }
})
