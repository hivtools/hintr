context("utils")

test_that("can check for empty", {
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(character(0)))
  expect_true(is_empty(""))
  expect_true(is_empty("   "))
  expect_false(is_empty("test"))
})

test_that("system file returns useful message when file cannot be located ", {
  args <- list("testdata", "missing_file.txt", package = "hintr")
  expect_error(system_file("testdata", "missing_file.txt", package = "hintr"),
"Failed to locate file from args
testdata missing_file.txt hintr")
})
