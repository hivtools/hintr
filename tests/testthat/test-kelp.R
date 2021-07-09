test_that("files can be saved and downloaded from seaweedFS", {
  payload <- setup_submit_payload()
  files <- yaml::read_yaml(payload)$data
  output <- kelp_save_files(test_kelp, files)

  dir <- tempfile()
  dir.create(dir)
  output_files <- kelp_download_files(test_kelp, output, dir)

  expect_equal(names(files), names(output_files))
  for (file in names(output_files)) {
    expect_equivalent(tools::md5sum(files[[file]]$path),
                      tools::md5sum(output_files[[file]]$path))
  }
})

test_that("paths can be saved and downloaded from seaweedFS", {
  output <- kelp_save_paths(test_kelp, mock_model)

  dir <- tempfile()
  dir.create(dir)
  output_files <- kelp_download_paths(test_kelp, output, dir)

  expect_equal(names(mock_model), names(output_files))
  paths <- mock_model[vapply(mock_model, is_file_path, logical(1))]
  others <- mock_model[!(names(mock_model) %in% names(paths))]
  for (file in names(paths)) {
    expect_equivalent(tools::md5sum(mock_model[[file]]),
                      tools::md5sum(output_files[[file]]))
  }
  for (other in names(others)) {
    expect_equal(mock_model[[other]], output_files[[other]])
  }
})
