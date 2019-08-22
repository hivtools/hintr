global_store <- new.env(parent = emptyenv())

store_start <- function(root, name = "hintr", global = TRUE) {
  if (!global || is.null(global_store$store)) {
    message("connecting to redis at ", redux::redis_config()$url)
    con <- redux::hiredis()

    message("Starting store")
    store <- redux::storr_redis_api(prefix = "store", con = con)

    global_store$store <- store
  }
  invisible(global_store$store)
}

store_data <- function(key, data) {
  global_store$store$set(key, data)
}

refresh_store <- function() {
  global_store$store$destroy()
  store_start()
}

#' Convert a set of names to a key appropriate for storing in redis.
#'
#' @param name Base name for redis key
#' @param ... Any additional names for key
#'
#' @return The redis key name
#'
#' @keywords internal
to_redis_key <- function(name, ...) {
  paste(name, ..., sep = ":")
}
