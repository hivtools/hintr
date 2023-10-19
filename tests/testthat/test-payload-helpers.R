test_that("error thrown if trying to create invalid download payload", {
  expect_error(setup_payload_download_request(include_notes = FALSE,
                                              include_state = FALSE,
                                              include_pjnz = FALSE),
               "Must include one or more of notes, state or pjnz in payload")
})
