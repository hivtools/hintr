library(testthat)
library(hintr)
library(mockery)

call_with_mocks_object <- function(code, mocks) {
    binded_code <- function(...) {
        with_mocked_bindings(code, ...)
    }
    do.call(binded_code, mocks)
}

test_check("hintr")
