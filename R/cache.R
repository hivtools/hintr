with_cache <- function(key, namespace, cache, expr) {
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
