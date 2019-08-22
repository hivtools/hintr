context("store")

test_that("names can be converted to redis key", {
  expect_equal(to_redis_key("/path/to/file"), "/path/to/file")
  expect_equal(to_redis_key("/path/to/file", "other_args", "test"),
               "/path/to/file:other_args:test")
})

test_that("global store can be started", {
  test_redis_available()
  store <- store_start()
  expect_s3_class(store, "storr")

  store$set("test:key", "value")
  expect_equal(store$list(), "test:key")
  expect_equal(store$get("test:key"), "value")

  ## values can be stores using utility functions
  df <- data.frame(c(1,2,3), c(4,5,6))
  store_data("key:123", df)
  expect_length(store$list(), 2)
  expect_equal(store$get("key:123"), df)

  ## refresh the store for any other tests
  refresh_store()
})
