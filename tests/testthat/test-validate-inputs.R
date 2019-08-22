context("validate-inputs")

test_that("baseline inputs can be validated and return data", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  ## TODO: Expand validation to include other input files
  expect_equal(do_validate_pjnz(pjnz), "Botswana")

  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    expect_error(do_validate_pjnz(pjnz), "Invalid country")
  })
})

test_that("country can be read from PJNZ file", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  expect_equal(read_country(pjnz), "Botswana")
})


test_that("regions can be retrieved from geojson", {
  shape <- system.file("testdata", "malawi.geojson", package = "hintr")
  json <- geojsonio::geojson_read(shape, method = "local")
  regions <- get_json_regions(json)
  expect_length(regions, 502)
  expect_true(all(vapply(regions, function(x) {
    grepl("MWI.*", x)
  }, logical(1))))
})

test_that("do_validate_shape returns json and stores region list in redis", {
  test_redis_available()
  ## ensure clean store
  refresh_store()
  shape <- system.file("testdata", "malawi.geojson", package = "hintr")
  json <- do_validate_shape(shape)
  expect_equal(names(json), c("type", "name", "crs", "features"))
  expect_equal(length(json$features), 502)

  ## Regions have been added to store at correct location
  regions <- store_get(to_redis_key(shape, "region_list"))
  expect_length(regions, 502)
})
