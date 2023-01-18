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
  mock_get_plotting_metadata <- mockery::mock(data_frame(
    data_type = rep("survey", 2),
    plot_type = rep("choropleth", 2),
    indicator = rep("prevalence", 2),
    indicator_sort_order = c(1, 2)
  ))
  with_mock(get_plotting_metadata = mock_get_plotting_metadata, {
    expect_error(do_plotting_metadata("Malawi"),
                 "Expected only 1 row for indicator, data type, plot type combination.
Check each combination is unique in configuration.")
  })
})
