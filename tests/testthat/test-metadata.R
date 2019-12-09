context("metadata")

test_that("can build metadata response", {
  metadata <- do_plotting_metadata("Malawi")
  expect_true(all(names(metadata) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(metadata$survey), "choropleth")
  expect_equal(names(metadata$anc), "choropleth")
  expect_equal(names(metadata$output), c("barchart", "choropleth"))
  expect_equal(names(metadata$programme), "choropleth")
  expect_length(metadata$anc$choropleth$indicators, 2)
  expect_equal(metadata$anc$choropleth$indicators[[1]]$indicator,
               scalar("prevalence"))
  expect_equal(metadata$anc$choropleth$indicators[[2]]$indicator,
               scalar("art_coverage"))
  expect_equal(metadata$anc$choropleth$indicators[[1]]$name,
               scalar("HIV prevalence"))
  expect_equal(metadata$anc$choropleth$indicators[[2]]$name,
               scalar("ART coverage"))
})

test_that("error thrown when metadata contains conflicting information", {
  mock_get_plotting_metadata <- mockery::mock(data_frame(
    data_type = rep("survey", 2),
    plot_type = rep("choropleth", 2),
    indicator = rep("prevalence", 2)
  ))
  with_mock("naomi::get_plotting_metadata" = mock_get_plotting_metadata, {
    expect_error(do_plotting_metadata("Malawi"),
                 "Expected only 1 row for indicator, data type, plot type combination.
Check each combination is unique in configuration.")
  })
})
