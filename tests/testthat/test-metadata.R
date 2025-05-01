test_that("can get output plot settings control", {
  stub(get_output_plot_settings_control,
       "get_choropleth_settings", "choropleth_settings")
  stub(get_output_plot_settings_control,
       "get_barchart_settings", "barchart_settings")
  stub(get_output_plot_settings_control,
       "get_table_settings", "table_settings")
  stub(get_output_plot_settings_control,
       "get_bubble_settings", "bubble_settings")
  stub(get_output_plot_settings_control,
       "get_cascade_settings", "cascade_settings")

  expect_equal(get_output_plot_settings_control(),
               list(
                 choropleth = "choropleth_settings",
                 barchart = "barchart_settings",
                 table = "table_settings",
                 bubble = "bubble_settings",
                 cascade = "cascade_settings"
               ))
})

test_that("can get choropleth settings", {
  mocks <- get_filter_mocks()
  expected_filter_calls <- get_mock_args_from_vector(c("indicator", "detail", "area", "period", "sex", "age"))
  filter_types <- mock_filter_types("detail")
  choropleth_settings <- call_with_mocks_object({
    get_choropleth_settings(filter_types)
  }, mocks)
  expect_equal(mock_args(mocks$get_filter_from_id), expected_filter_calls)
  expect_equal(choropleth_settings,
    list(
      defaultEffect = list(
        setFilters = rep(list("filter_ref"), 6),
        setMultiple = "area",
        setFilterValues = list(
          indicator = "prevalence",
          detail = list(scalar("opt2"))
        )
      ),
      plotSettings = list()
    )
  )
})

test_that("can get barchart settings", {
  mocks <- get_filter_mocks()
  default_filter_ids <- c("indicator", "area", "period", "sex", "age")
  all_filter_ids <- c("indicator", "detail", "area", "period", "sex", "age")
  expected_filter_calls <- get_mock_args_from_vector(c(default_filter_ids, all_filter_ids))
  stub(get_barchart_settings, "naomi::get_five_year_age_groups", "five_year_age_groups")
  filter_types <- mock_filter_types("period")
  barchart_settings <- call_with_mocks_object({
      get_barchart_settings(filter_types)
  }, mocks)
  expect_equal(mock_args(mocks$get_filter_from_id), expected_filter_calls)
  expect_equal(barchart_settings$defaultEffect, list(
        setFilters = rep(list("filter_ref"), 5),
        setFilterValues = list(
          indicator = "prevalence",
          period = list(scalar("opt2")),
          sex = "both"
        )
      )
  )
  control_ids <- lapply(barchart_settings$plotSettings, "[[", "id")
  ## Sets filter values for same property it sets multiple except for area
  all_opts <- c(barchart_settings$plotSettings[[1]]$options,
                barchart_settings$plotSettings[[2]]$options)
  for (opt in all_opts) {
    if (opt$id != "area") {
      expect_setequal(names(opt$effect), c("setMultiple", "setFilterValues"))
      expect_equal(names(opt$effect$setFilterValues), opt$effect$setMultiple)
    }
  }
})

test_that("can get table settings", {
  stub(get_table_settings, "get_table_presets", "table_presets")
  filter_types <- mock_filter_types("period")
  table_settings <- get_table_settings(filter_types)
  expect_equal(table_settings,
    list(
      defaultEffect = list(
        setFilterValues = list(
          indicator = "prevalence",
          period = list(scalar("opt2"))
        )
      ),
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
  filter_types <- mock_filter_types("detail")
  bubble_settings <- call_with_mocks_object({
      get_bubble_settings(filter_types)
  }, mocks)
  expected_filter_calls <- get_mock_args_from_vector(c("detail", "area", "period", "sex", "age"))
  expect_equal(mock_args(mocks$get_filter_from_id), expected_filter_calls)
  expected_indicators <- list(
    list(
      filterId = scalar("indicator"),
      label = scalar("Colour Indicator"),
      stateFilterId = scalar("colourIndicator")
    ),
    list(
      filterId = scalar("indicator"),
      label = scalar("Size Indicator"),
      stateFilterId = scalar("sizeIndicator")
    )
  )
  expect_equal(
    bubble_settings,
    list(
      defaultEffect = list(
        setFilters = c(expected_indicators, rep(list("filter_ref"), 5)),
        setMultiple = "area",
        setFilterValues = list(
          colourIndicator = "prevalence",
          sizeIndicator = "plhiv",
          detail = list(scalar("opt2"))
        )
      ),
      plotSettings = list()
    )
  )
})

test_that("can get table presets", {
  mocks <- get_filter_mocks()
  stub(get_table_presets, "naomi::get_five_year_age_groups", "five_year_age_groups")
  expected_filter_calls_1 <- get_mock_args_from_vector(c("indicator", "area", "detail", "period", "sex", "age"))
  expected_filter_calls_2 <- get_mock_args_from_vector(c("indicator", "area", "period", "sex", "age"))
  expected_filter_calls <- append(expected_filter_calls_1, expected_filter_calls_2)
  filter_types <- mock_filter_types("detail")
  table_presets <- call_with_mocks_object({
      get_table_presets(filter_types)
  }, mocks)
  expect_equal(mock_args(mocks$get_filter_from_id), expected_filter_calls)
  expect_equal(table_presets,
    list(
      list(
        id = scalar("sex_by_area"),
        label = scalar("Sex by area"),
        effect = list(
          setFilters = rep(list("filter_ref"), 6),
          setHidden = c("area"),
          setMultiple = c("sex", "area"),
          setFilterValues = list(
            detail = list(scalar("opt2"))
          ),
          customPlotEffect = list(
            row = "area",
            column = "sex"
          )
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
          ),
          customPlotEffect = list(
            row = "age",
            column = "sex"
          )
        )
      )
    )
  )
})

test_that("can get cascade settings", {
  mocks <- get_filter_mocks()
  filter_ids <- c("indicator", "detail", "area", "period", "sex", "age")
  expected_filter_calls <- get_mock_args_from_vector(filter_ids)
  stub(get_cascade_settings, "naomi::get_five_year_age_groups", "five_year_age_groups")
  filter_types <- mock_filter_types("detail")
  cascade_settings <- call_with_mocks_object({
    get_cascade_settings(filter_types)
  }, mocks)
  expect_equal(cascade_settings$defaultEffect, list(
    setFilters = rep(list("filter_ref"), 6),
    setFilterValues = list(
      indicator = c("plhiv_attend", "aware_plhiv_attend",
                    "art_current")
    ),
    setHidden = c("indicator")
  ))

  control_ids <- lapply(cascade_settings$plotSettings, "[[", "id")
  expect_setequal(control_ids, c("x_axis", "disagg_by"))
  ## Sets filter values for same property it sets multiple except for area
  expect_equal(cascade_settings$plotSettings[[1]]$options,
               list("x_axis_or_disagg_by_option"))
  expect_equal(cascade_settings$plotSettings[[2]]$options,
               list("x_axis_or_disagg_by_option"))
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

test_that("review inputs function works with anc", {
  input <- setup_payload_review_inputs_metadata(test_path("testdata"))
  metadata <- review_input_filter_metadata(input)
  ## Testthat skips if only calling custom expect function, so add this
  ## noddy test to force it to run.
  expect_setequal(names(metadata),
                  c("filterTypes", "indicators", "plotSettingsControl"))
  expect_input_metadata(metadata,
                        c("survey", "programme", "anc"),
                        c("programme", "programme_comparison", "anc"))
})

test_that("review inputs function is translated", {
  input <- setup_payload_review_inputs_metadata(test_path("testdata"),
                                                include_anc = FALSE,
                                                include_programme = FALSE)
  response <- with_hintr_language("fr", {
    metadata <- review_input_filter_metadata(input)
  })
  filter_ids <- vcapply(response$filterTypes, "[[", "id")
  survey_indicator <- response$filterTypes[
    filter_ids == "map_survey_indicator"][[1]]
  expect_equal(survey_indicator$options[[1]]$label, scalar("Prévalence du VIH"))
  expect_equal(response$indicators$name[1], "Prévalence du VIH")
})

test_that("filters not returned if indicator missing from input data", {
  input <- setup_payload_review_inputs_metadata(test_path("testdata"),
                                                include_anc = FALSE)
  input_json <- jsonlite::fromJSON(input, simplifyVector = FALSE)
  input_json$data$programme$path <- test_path("testdata",
                                              "programme_no_vls.csv")
  input <- jsonlite::toJSON(input_json)
  response <- review_input_filter_metadata(input)

  filter_ids <- vcapply(response$filterTypes, "[[", "id")
  programme_indicators <- response$filterTypes[
    filter_ids == "map_programme_indicator"][[1]]
  expect_length(programme_indicators$options, 2)
  expect_equal(programme_indicators$options[[1]]$id, scalar("art_current"))
  expect_equal(programme_indicators$options[[1]]$label,
               scalar("ART number (attending)"))
  expect_equal(programme_indicators$options[[2]]$id, scalar("art_new"))
  expect_equal(programme_indicators$options[[2]]$label,
               scalar("ART new"))

  expect_input_metadata(response,
                        c("survey", "programme"),
                        c("programme", "programme_comparison"))
})

test_that("api can call review_input_metadata", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  input <- setup_payload_review_inputs_metadata(test_path("testdata"))
  res <- api$request("POST", "/review-input/metadata", body = input)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_input_metadata(body$data,
                        c("survey", "anc", "programme"),
                        c("anc", "programme", "programme_comparison"))
})

test_that("metadata only returned if file present", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  input <- setup_payload_review_inputs_metadata(test_path("testdata"),
                                                include_anc = FALSE)
  res <- api$request("POST", "/review-input/metadata", body = input)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_input_metadata(body$data,
                        c("survey", "programme"),
                        c("programme", "programme_comparison"))
})

test_that("time series metadata only returned if anc or programme", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  input <- setup_payload_review_inputs_metadata(test_path("testdata"),
                                                include_anc = FALSE,
                                                include_programme = FALSE)
  res <- api$request("POST", "/review-input/metadata", body = input)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_input_metadata(body$data, c("survey"), c())
})
