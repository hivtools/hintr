test_that("single task can be migrated", {
  test_mock_model_available()
  q <- test_queue_result(model = mock_model_v0.1.38,
                         calibrate = mock_model_v0.1.38)
  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(q$calibrate_id, q$queue),
                 sprintf("Successfully migrated %s", q$calibrate_id))
  expect_equal(migrated$id, q$calibrate_id)
  expect_setequal(names(migrated$prev_res),
                  c("output_path", "spectrum_path", "coarse_output_path",
                    "calibration_path", "summary_report_path", "metadata"))
  expect_setequal(names(migrated$new_res),
                  c("plot_data_path", "model_output_path", "version",
                    "warnings"))
  expect_equal(migrated$from, "x.x.x")
  expect_equal(migrated$to, "2.5.0")
  expect_equal(migrated$action, "Successfully migrated")

  ## Result has been migrated
  res <- q$queue$result(q$calibrate_id)
  expect_equal(res, migrated$new_res)
})

test_that("already up to date task is not migrated", {
  q <- test_queue_result()
  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(q$calibrate_id, q$queue),
                 sprintf("Not migrating %s, already up to date",
                         q$calibrate_id))
  expect_equal(migrated$id, q$calibrate_id)
  expect_equal(migrated$prev_res, q$queue$result(q$calibrate_id))
  expect_equal(migrated$action, "No change - up to date")
})

test_that("invalid output format is not migrated", {
  test_mock_model_available()
  q <- test_queue_result(model = mock_model_v0.1.2,
                         calibrate = mock_model_v0.1.2,
                         clone_output = FALSE)
  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(q$calibrate_id, q$queue),
                 sprintf("Not migrating %s, invalid output format",
                         q$calibrate_id))
  expect_equal(migrated$id, q$calibrate_id)
  expect_equal(migrated$prev_res, q$queue$result(q$calibrate_id))
  expect_equal(migrated$action, "No change - not migrateable")
})

test_that("all tasks can be migrated", {
  test_mock_model_available()
  ## Get 3 task results, 1 mock run, 1 mock calibrate, 1 run, 1 calibrate
  q <- test_queue_result(model = mock_model_v0.1.38,
                         calibrate = mock_model_v0.1.38)
  model_payload <- setup_payload_submit()
  model_submit <- submit_model(q$queue)
  run_response <- model_submit(readLines(model_payload))
  expect_true("id" %in% names(run_response))
  result <- q$queue$queue$task_wait(run_response$id)

  calibrate_payload <- setup_payload_calibrate()
  model_calibrate <- submit_calibrate(q$queue)
  calibrate_response <- model_calibrate(run_response$id,
                                        readLines(calibrate_payload))
  expect_true("id" %in% names(calibrate_response))
  result <- q$queue$queue$task_wait(calibrate_response$id)

  ## Run migration
  t <- tempfile()
  dir.create(t)
  msg <- capture_messages(migrate <- run_migration(q$queue, t))

  expect_equal(sum(grepl("Migrating", msg)), 4) ## 4 Migrating messages
  expect_equal(sum(grepl(sprintf(
    "Successfully migrated %s", q$model_run_id), msg)), 1)
  expect_equal(sum(grepl(sprintf(
    "Successfully migrated %s", q$calibrate_id), msg)), 1)
  expect_equal(sum(grepl(sprintf(
    "Not migrating %s", run_response$id), msg)), 1)
  expect_equal(sum(grepl(sprintf(
    "Not migrating %s", calibrate_response$id), msg)), 1)
  expect_equal(sum(grepl("Saving summary csv", msg)), 1)

  expect_length(migrate, 4)
  files <- list.files(t)
  expect_length(files, 2)
  summary <- read.csv(file.path(t, "summary.csv"))
  expect_setequal(colnames(summary), c("id", "action"))
  expect_equal(nrow(summary), 4)
  expect_setequal(summary$id, c(q$model_run_id, q$calibrate_id, run_response$id,
                                calibrate_response$id))
  expect_setequal(summary$action,
                  c("Successfully migrated", "No change - up to date"))

  migration_output <- naomi::read_hintr_output(file.path(t, "output.qs"))
  expect_equal(migration_output, migrate)

  ## Data has been migrated
  expect_equal(names(q$queue$result(q$model_run_id)),
               c("plot_data_path", "model_output_path", "version", "warnings"))
  expect_equal(names(q$queue$result(q$calibrate_id)),
               c("plot_data_path", "model_output_path", "version", "warnings"))
  expect_equal(names(q$queue$result(run_response$id)),
               c("plot_data_path", "model_output_path", "version", "warnings"))
  expect_equal(names(q$queue$result(calibrate_response$id)),
               c("plot_data_path", "model_output_path", "version", "warnings"))
})

test_that("only completed tasks are migrated", {
  test_mock_model_available()
  ## Setup errored model run
  queue <- MockQueue$new()
  path <- setup_payload_submit()
  model_submit <- submit_model(queue)
  response <- model_submit(readLines(path))
  expect_true("id" %in% names(response))
  out <- queue$queue$task_wait(response$id)

  t <- tempfile()
  dir.create(t)
  msg <- capture_messages(migrated <- run_migration(queue, t))
  expect_equal(sum(grepl("Migrating", msg)), 0) ## Nothing was migrated
  expect_equal(migrated, list())
})

test_that("migration can be run in dry-run mode", {
  test_mock_model_available()
  ## Get 3 task results, 1 mock run, 1 mock calibrate, 1 run, 1 calibrate
  q <- test_queue_result(model = mock_model_v0.1.38,
                         calibrate = mock_model_v0.1.38)
  model_payload <- setup_payload_submit()
  model_submit <- submit_model(q$queue)
  run_response <- model_submit(readLines(model_payload))
  expect_true("id" %in% names(run_response))
  result <- q$queue$queue$task_wait(run_response$id)

  calibrate_payload <- setup_payload_calibrate()
  model_calibrate <- submit_calibrate(q$queue)
  calibrate_response <- model_calibrate(run_response$id,
                                        readLines(calibrate_payload))
  expect_true("id" %in% names(calibrate_response))
  result <- q$queue$queue$task_wait(calibrate_response$id)

  ## Run migration
  t <- tempfile()
  dir.create(t)
  msg <- capture_messages(migrate <- run_migration(q$queue, t, dry_run = TRUE))

  expect_equal(sum(grepl("Migrating", msg)), 4) ## 4 Migrating messages
  expect_equal(sum(grepl(sprintf(
    "Successfully migrated %s", q$model_run_id), msg)), 1)
  expect_equal(sum(grepl(sprintf(
    "Successfully migrated %s", q$calibrate_id), msg)), 1)
  expect_equal(sum(grepl(sprintf(
    "Not migrating %s", run_response$id), msg)), 1)
  expect_equal(sum(grepl(sprintf(
    "Not migrating %s", calibrate_response$id), msg)), 1)
  expect_equal(sum(grepl("Saving summary csv", msg)), 1)

  expect_length(migrate, 4)
  files <- list.files(t)
  expect_length(files, 2)
  summary <- read.csv(file.path(t, "summary.csv"))
  expect_setequal(colnames(summary), c("id", "action"))
  expect_equal(nrow(summary), 4)
  expect_setequal(summary$id, c(q$model_run_id, q$calibrate_id, run_response$id,
                                calibrate_response$id))
  expect_setequal(summary$action,
                  c("Successfully migrated", "No change - up to date"))

  migration_output <- qs::qread(file.path(t, "output.qs"))
  expect_equal(migration_output, migrate)

  ## Data has not been migrated
  expect_equal(names(q$queue$result(q$model_run_id)),
               c("output_path", "spectrum_path", "coarse_output_path",
                 "calibration_path", "summary_report_path", "metadata"))
  expect_equal(names(q$queue$result(q$calibrate_id)),
               c("output_path", "spectrum_path", "coarse_output_path",
                 "calibration_path", "summary_report_path", "metadata"))
  expect_equal(names(q$queue$result(run_response$id)),
               c("plot_data_path", "model_output_path", "version", "warnings"))
  expect_equal(names(q$queue$result(calibrate_response$id)),
               c("plot_data_path", "model_output_path", "version", "warnings"))
})
