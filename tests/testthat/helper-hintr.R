## This helper lets us use with_mock interactively see
## https://github.com/krlmlr/mockr/pull/29
with_mock <- function(..., .parent = parent.frame()) {
  ## We need to manually remove .onLoad from the list otherwise
  ## mockr tries to mget it from .parent and can't mget the
  ## .onLoad function in this context (not sure why though, see issue above)
  .parent$.onLoad <- get(".onLoad", envir = asNamespace("hintr"))
  mockr::with_mock(..., .parent = .parent, .env = "hintr")
}
