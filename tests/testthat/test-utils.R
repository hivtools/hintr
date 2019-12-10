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
  args <- list("testdata", "missing_file.txt")
  expect_error(system_file("testdata", "missing_file.txt"),
"Failed to locate file from args
testdata missing_file.txt")
})

test_that("collapse prepares vector for printing", {
  test_vector <- c("one", "two", "three", "four", "five", "six")
  expect_equal(collapse(test_vector),
               "one, two, three, four, five, six")
  expect_equal(collapse(test_vector, limit = 27),
               "one, two, three, four, ...")
  expect_equal(collapse(test_vector, limit = 28),
               "one, two, three, four, five, ...")
  expect_equal(collapse(test_vector, limit = 25, end = NULL),
               "one, two, three, four")
  expect_equal(collapse(test_vector, limit = 40),
               "one, two, three, four, five, six")
  expect_equal(collapse(test_vector, limit = 25, end = "etc."),
               "one, two, three, four, etc.")
  expect_equal(collapse(test_vector, collapse = " and ", limit = 13, end = NULL),
               "one and two")
  expect_equal(collapse(test_vector, limit = 1), "...")
})

test_that("file_copy reports error on failure", {
  expect_error(file_copy(tempfile(), tempfile()),
               "Copying .* failed")
})

test_that("throttle does not call functions very often", {
  f <- mockery::mock(1, 2)
  g <- throttle(f, 0.5)
  expect_equal(g(), 1)
  expect_null(g())
  mockery::expect_called(f, 1)
  Sys.sleep(0.6)
  expect_equal(g(), 2)
  mockery::expect_called(f, 2)
})

test_that("no_error swallows all errors", {
  f <- function(x) {
    if (x < 0) {
      stop("expected positive x")
    }
  }
  expect_silent(no_error(f(1)))
  expect_silent(no_error(f(-1)))
})
