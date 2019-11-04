context("cache")

test_that("with_cache does not call target expression on cache hit", {
  cache <- new_cache()
  key <- "aaa"
  namespace <- "ns"
  fn <- mockery::mock(1, 2, 3)
  expect_equal(with_cache(key, namespace, cache, fn()), 1)
  expect_equal(with_cache(key, namespace, cache, fn()), 1)

  expect_equal(cache$get(key, namespace), 1)
  expect_equal(cache$list(namespace), key)

  mockery::expect_called(fn, 1)
})

test_that("with_cache calls target expression on cache miss, get on hit", {
  cache <- new_cache()
  key1 <- "aaa"
  key2 <- "bbb"
  namespace <- "ns"
  fn <- mockery::mock(1, 2, 3)
  unlockBinding("get", cache)
  cache$get <- mockery::mock(-1, -2, -3)

  expect_equal(with_cache(key1, namespace, cache, fn()), 1)
  expect_equal(with_cache(key2, namespace, cache, fn()), 2)

  expect_equal(with_cache(key1, namespace, cache, fn()), -1)
  expect_equal(with_cache(key2, namespace, cache, fn()), -2)

  mockery::expect_called(fn, 2)
  mockery::expect_called(cache$get, 2)
})

test_that("with_cache calls target expression if cache is missing", {
  key <- "aaa"
  namespace <- "ns"
  fn <- mockery::mock(1, 2, 3)
  expect_equal(with_cache(key, namespace, NULL, fn()), 1)
  expect_equal(with_cache(key, namespace, NULL, fn()), 2)
  mockery::expect_called(fn, 2)
})

test_that("Global cache interaction", {
  id <- ids::random_id()
  set_cache(id)

  cache <- hintenv$cache[[id]]
  fn <- mockery::mock(1, 2, 3)
  unlockBinding("get", cache)
  cache$get <- mockery::mock(-1, -2, -3)

  expect_identical(get_cache(NULL), cache)
  expect_identical(get_cache(TRUE), TRUE)

  expect_equal(with_cache("aaa", "ns", NULL, fn()), 1)
  expect_equal(with_cache("aaa", "ns", NULL, fn()), -1)

  mockery::expect_called(fn, 1)
  mockery::expect_called(cache$get, 1)

  clear_cache(id)
  expect_null(get_cache(NULL))
  expect_equal(with_cache("aaa", "ns", NULL, fn()), 2)

  mockery::expect_called(fn, 2)
  mockery::expect_called(cache$get, 1)
})
