context("migrations")

test_that("single task can be migrated", {
  test_mock_model_available()
  q <- test_queue_result(model = mock_model_v0.1.34,
                         calibrate = mock_model_v0.1.34)
  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(q$calibrate_id, q$queue, t),
                 sprintf("Successfully migrated %s, details: ", q$calibrate_id))
  expect_equal(migrated$id, q$calibrate_id)
  expect_setequal(names(migrated$prev_res),
                  c("output_path", "spectrum_path", "coarse_output_path",
                    "calibration_path", "summary_report_path", "metadata"))
  expect_setequal(names(migrated$new_res),
                  c("plot_data_path", "model_output_path", "version"))
  expect_equal(migrated$from, "x.x.x")
  expect_equal(migrated$to, "2.4.0")
  expect_equal(migrated$action, "Successfully migrated")
  expect_equivalent(migrated$status, "COMPLETE")
  expect_equal(readRDS(migrated$migration_file), migrated)
  expect_length(list.files(t), 1)

  ## Result has been migrated
  res <- q$queue$result(q$calibrate_id)
  expect_equal(res, migrated$new_res)

  ## Migrating again does not modify output or create new file
  expect_message(migrated2 <- migrate_task(q$calibrate_id, q$queue, t),
                 sprintf("Not migrating %s, task aleady migrated see details: ",
                         q$calibrate_id))
  expect_equal(migrated2$id, q$calibrate_id)
  expect_equal(migrated2$action, "Result already migrated")
  expect_equal(migrated2$migration_file, migrated$migration_file)
})

test_that("already up to date task is not migrated", {
  q <- test_queue_result()
  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(q$calibrate_id, q$queue, t),
                 sprintf("Not migrating %s, already up to date, details: ",
                         q$calibrate_id))
  expect_equal(migrated$id, q$calibrate_id)
  expect_equal(migrated$prev_res, q$queue$result(q$calibrate_id))
  expect_equal(migrated$action, "No change - up to date")
  expect_equal(readRDS(migrated$migration_file), migrated)
})

test_that("only completed tasks are migrated", {
  test_mock_model_available()
  ## Setup errored model run
  queue <- MockQueue$new()
  path <- setup_submit_payload()
  model_submit <- submit_model(queue)
  response <- model_submit(readLines(path))
  expect_true("id" %in% names(response))
  out <- queue$queue$task_wait(response$id)

  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(response$id, queue, t), sprintf(
    "Not migrating %s task not complete, task status: ERROR, details:",
    response$id))
  expect_equal(migrated$id, response$id)
  expect_equivalent(migrated$status, "ERROR")
  expect_equal(migrated$action, "No change - incomplete model run")
  expect_equal(readRDS(migrated$migration_file), migrated)
})

test_that("invalid output format is not migrated", {
  test_mock_model_available()
  q <- test_queue_result(model = mock_model_v0.1.2,
                         calibrate = mock_model_v0.1.2,
                         clone_output = FALSE)
  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(q$calibrate_id, q$queue, t),
                 sprintf("Not migrating %s, invalid output format, details:",
                         q$calibrate_id))
  expect_equal(migrated$id, q$calibrate_id)
  expect_equivalent(migrated$status, "COMPLETE")
  expect_equal(migrated$prev_res, q$queue$result(q$calibrate_id))
  expect_equal(migrated$action, "No change - not migrateable")
  expect_equal(readRDS(migrated$migration_file), migrated)
})

test_that("all tasks can be migrated", {
  test_mock_model_available()
  ## Get 3 task results, 1 mock run, 1 mock calibrate, 1 run, 1 calibrate
  q <- test_queue_result(model = mock_model_v0.1.34,
                         calibrate = mock_model_v0.1.34)
  model_payload <- setup_submit_payload()
  model_submit <- submit_model(q$queue)
  run_response <- model_submit(readLines(model_payload))
  expect_true("id" %in% names(run_response))
  result <- q$queue$queue$task_wait(run_response$id)

  calibrate_payload <- setup_calibrate_payload()
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
  expect_length(files, 5)
  summary <- read.csv(file.path(t, "summary.csv"))
  expect_setequal(colnames(summary), c("id", "action", "migration_file"))
  expect_equal(nrow(summary), 4)
  expect_setequal(summary$id, c(q$model_run_id, q$calibrate_id, run_response$id,
                                calibrate_response$id))
  expect_setequal(summary$action,
                  c("Successfully migrated", "No change - up to date"))
  migration_files <- lapply(migrate, "[[", "migration_file")
  expect_setequal(summary$migration_file, migration_files)
})
