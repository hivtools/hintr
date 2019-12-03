context("on load")

test_that("version info is loaded on package load", {
  expect_true(!is.null(cfg$version_info))
  expect_equal(names(cfg$version_info), c("hintr", "naomi", "rrq", "traduire"))
  expect_true(all(grepl("^(\\d+\\.)?(\\d+\\.)?(\\*|\\d+)$", cfg$version_info)))

  version_info <- get_version_info()
  expect_equal(version_info, cfg$version_info)
})
