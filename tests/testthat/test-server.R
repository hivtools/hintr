context("server")

test_that("Root", {
  server <- hintr_server()

  r <- httr::GET(server$url)
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_to_json(r), "Welcome to hintr")
})

test_that("validate pjnz", {
  server <- hintr_server()

  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  body <- list(type = scalar("pjnz"), path = scalar(pjnz))

  r <- httr::POST(paste0(server$url, "/validate"), body = body,
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_to_json(r),
               list(status = "success",
                    errors = structure(list(), names = character(0)),
                    data = list(filename = "Botswana2018.PJNZ",
                                type = "pjnz",
                                data = list(country = "Botswana"))))
})
