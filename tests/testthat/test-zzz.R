context("on load")

test_that("version info is loaded on package load", {
  expect_true(!is.null(cfg$version_info))
  expect_equal(names(cfg$version_info), c("hintr", "naomi", "rrq", "traduire"))
  expect_true(all(grepl("^(\\d+\\.)?(\\d+\\.)?(\\*|\\d+)$", cfg$version_info)))

  version_info <- get_version_info()
  expect_equal(version_info, cfg$version_info)
})

test_that("translation initialisation calls traduire_register", {
  expect_true("package:hintr" %in% traduire::translator_list())
  expect_error(hintr_translator(), NA)
  hintr_translator_unregister()
  expect_false("package:hintr" %in% traduire::translator_list())
  expect_error(hintr_translator())
  hintr_init_traduire()
  expect_error(hintr_translator(), NA)
  expect_true("package:hintr" %in% traduire::translator_list())
})
