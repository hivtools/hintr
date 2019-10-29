Sys.setenv("USE_MOCK_MODEL" = "true")

mock_model <- list(output_path = file.path("testdata", "malawi_output.RDS"),
                   spectrum_path = file.path("testdata", "malawi_spectrum_download.zip"),
                   summary_path = file.path("testdata", "malawi_summary_download.zip"))
