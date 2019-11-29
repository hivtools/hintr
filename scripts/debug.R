# library(hintr)
env <- loadNamespace("hintr")
attach(env)
options(error = traceback)

pjnz <- file.path("tests/testthat/testdata", "Malawi2019.PJNZ")
shape <- file.path("tests/testthat/testdata", "malawi.geojson")
population <- file.path("tests/testthat/testdata", "population.csv")

# TODO (mrc-663): we should require file objects here.
if (!is.null(pjnz)) {
  pjnz <- file_object(pjnz)
}
if (!is.null(shape)) {
  shape <- file_object(shape)
}
if (!is.null(population)) {
  population <- file_object(population)
}
response <- do_validate_baseline(pjnz, shape, population)
