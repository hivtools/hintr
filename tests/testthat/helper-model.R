mock_model <- list(output_path = file.path("testdata", "output.RDS"),
                   spectrum_path = file.path("testdata", "spectrum_download.zip"),
                   summary_path = file.path("testdata", "summary_download.zip"))

mock_run_model <- mockery::mock(mock_model, cycle = TRUE)
