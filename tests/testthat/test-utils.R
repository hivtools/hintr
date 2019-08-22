context("utils")

test_that("can check for empty", {
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(character(0)))
  expect_true(is_empty(""))
  expect_true(is_empty("   "))
  expect_false(is_empty("test"))
})
