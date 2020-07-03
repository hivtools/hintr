# The caching layer logic is a bit complicated, in order to make the
# cache flexible and no too annoying in tests.
#
# Each Queue object will create a cache within the package env
# (hintenv$list), with a name corresponding to the current queue id,
# and then set the current queue id (hintenv$current_id), and register
# as a finaliser code to destroy their cache.
#
# This means that while a queue is active, the cache will work, but if
# queue A is cleaned up after queue B is started, the cleanup will not
# affect queue B, and queue A gets a clean cache.
hintenv <- new.env(parent = emptyenv())
hintenv$cache <- list()

with_cache <- function(key, namespace, cache, expr) {
  cache <- get_cache(cache)
  if (!is.null(cache) && cache$exists(key, namespace)) {
    return(cache$get(key, namespace))
  }
  value <- force(expr)
  if (!is.null(cache)) {
    cache$set(key, value, namespace, use_cache = FALSE)
  }
  value
}

new_cache <- function() {
  storr::storr_rds(tempfile(), compress = FALSE, mangle_key = FALSE)
}

get_cache <- function(cache) {
  cache %||% if (!is.null(hintenv$current)) hintenv$cache[[hintenv$current]]
}

set_cache <- function(id) {
  hintenv$cache[[id]] <- new_cache()
  hintenv$current <- id
}

clear_cache <- function(id) {
  if (!is.null(id)) {
    x <- hintenv$cache[[id]]
    if (!is.null(x)) {
      x$destroy()
      hintenv$cache[[id]] <- NULL
    }
  }
}
