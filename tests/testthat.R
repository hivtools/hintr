library(testthat)
library(hintr)
library(mockery)

call_with_mocks_object <- function(code, mocks) {
    binded_code <- function(...) {
        with_mocked_bindings(code, ...)
    }
    do.call(binded_code, mocks)
}

get_mock_args_from_vector <- function(vector) {
  # mock args are lists of lists
  lapply(as.list(vector), list)
}

test_check("hintr")
