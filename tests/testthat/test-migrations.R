test_that("single task can be migrated", {
  test_mock_model_available()
  q <- test_queue_result(model = mock_model_v1.1.15,
                         calibrate = mock_calibrate_v1.1.15)
  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(q$calibrate_id, q$queue,
                                          "2.9.10", "2.9.11", dry_run = FALSE),
                 sprintf("Successfully migrated %s", q$calibrate_id))
  expect_equal(migrated$id, q$calibrate_id)
  expect_true(naomi:::is_hintr_output(migrated$new_res))
  expect_equal(tools::file_ext(migrated$new_res$plot_data_path), "duckdb")
  expect_true(file.exists(migrated$new_res$plot_data_path))
  expect_equal(migrated$prev_res$model_output_path,
               migrated$new_res$model_output_path)
  expect_equal(migrated$new_res$version, "2.9.11")
  expect_equal(migrated$from, "2.9.10")
  expect_equal(migrated$to, "2.9.11")
  expect_equal(migrated$action, "Successfully migrated")

  ## Result has been migrated
  res <- q$queue$result(q$calibrate_id)
  expect_equal(res, migrated$new_res)

  ## File is a duckdb file which can be read
  expect_silent(naomi::read_hintr_output(migrated$new_res$plot_data_path))
})

test_that("already up to date task is not migrated", {
  q <- test_queue_result()
  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(q$calibrate_id, q$queue,
                                          "2.9.10", "2.9.11", dry_run = FALSE),
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
  expect_message(migrated <- migrate_task(q$calibrate_id, q$queue,
                                          "2.9.10", "2.9.11", dry_run = FALSE),
                 sprintf("Not migrating %s, invalid output format",
                         q$calibrate_id))
  expect_equal(migrated$id, q$calibrate_id)
  expect_equal(migrated$prev_res, q$queue$result(q$calibrate_id))
  expect_equal(migrated$action, "No change - not migrateable")
})

test_that("model output is not migrated", {
  test_mock_model_available()
  q <- test_queue_result(model = mock_model_v1.1.15,
                         calibrate = mock_calibrate_v1.1.15)
  t <- tempfile()
  dir.create(t)
  expect_message(migrated <- migrate_task(q$model, q$queue,
                                          "2.9.10", "2.9.11", dry_run = FALSE),
                 sprintf(
                   "Not migrating %s, this result does not have plot data",
                   q$model_run_id))
  expect_equal(migrated$id, q$model_run_id)
  expect_equal(migrated$prev_res, q$queue$result(q$model_run_id))
  expect_equal(migrated$action,
               "No change - only migrating plot data and this result has none")
})

test_that("all tasks can be migrated", {
  test_mock_model_available()
  ## Get 4 task results, 1 mock run, 1 mock calibrate, 1 run, 1 calibrate
  q <- test_queue_result(model = mock_model_v1.1.15,
                         calibrate = mock_calibrate_v1.1.15)
  model_payload <- setup_payload_submit()
  model_submit <- submit_model(q$queue)
  run_response <- model_submit(model_payload)
  expect_true("id" %in% names(run_response))
  result <- q$queue$queue$task_wait(run_response$id)

  calibrate_payload <- setup_payload_calibrate()
  model_calibrate <- submit_calibrate(q$queue)
  calibrate_response <- model_calibrate(run_response$id, calibrate_payload)
  expect_true("id" %in% names(calibrate_response))
  result <- q$queue$queue$task_wait(calibrate_response$id)

  ## Store some data we'll use for testing later
  mock_run_result <- q$queue$result(q$model_run_id)
  mock_calibrate_result <- q$queue$result(q$calibrate_id)
  real_run_result <- q$queue$result(run_response$id)
  real_calibrate_result <- q$queue$result(calibrate_response$id)

  ## Run migration
  t <- tempfile()
  dir.create(t)
  msg <- capture_messages(
    migrate <- run_migration(q$queue, t, "2.9.10", "2.9.11", dry_run = FALSE))

  expect_equal(sum(grepl("Migrating", msg)), 4) ## 4 Migrating messages
  expect_equal(sum(grepl(sprintf(
    "Not migrating %s", q$model_run_id), msg)), 1)
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
  expect_setequal(
    summary$action,
    c("Successfully migrated", "No change - up to date",
    "No change - only migrating plot data and this result has none"))

  migration_output <- naomi::read_hintr_output(file.path(t, "output.qs"))
  expect_equal(migration_output, migrate)

  ## Data has been migrated
  migrated_mock_run_result <- q$queue$result(q$model_run_id)
  migrated_mock_calibrate_result <- q$queue$result(q$calibrate_id)
  migrated_real_run_result <- q$queue$result(run_response$id)
  migrated_real_calibrate_result <- q$queue$result(calibrate_response$id)

  expect_equal(migrated_mock_run_result, mock_run_result)
  expect_equal(names(mock_calibrate_result),
               names(migrated_mock_calibrate_result))
  expect_equal(mock_calibrate_result$model_output_path,
               migrated_mock_calibrate_result$model_output_path)
  expect_true(mock_calibrate_result$plot_data_path !=
                migrated_mock_calibrate_result$plot_data_path)
  expect_equal(tools::file_ext(migrated_mock_calibrate_result$plot_data_path),
               "duckdb")
  expect_equal(migrated_real_run_result, real_run_result)
  expect_equal(migrated_real_calibrate_result, real_calibrate_result)
})

test_that("only completed tasks are migrated", {
  test_mock_model_available()
  ## Setup errored model run
  queue <- MockQueue$new()
  payload <- setup_payload_submit()
  model_submit <- submit_model(queue)
  response <- model_submit(payload)
  expect_true("id" %in% names(response))
  out <- queue$queue$task_wait(response$id)

  expect_equal(out$status, "ERROR")

  t <- tempfile()
  dir.create(t)
  msg <- capture_messages(migrated <- run_migration(queue, t, "2.9.10",
                                                    "2.9.11", dry_run = FALSE))
  expect_equal(sum(grepl("Migrating", msg)), 0) ## Nothing was migrated
  expect_equal(migrated, list())
})

test_that("migration can be run in dry-run mode", {
  test_mock_model_available()
  ## Get 34task results, 1 mock run, 1 mock calibrate, 1 run, 1 calibrate
  q <- test_queue_result(model = mock_model_v1.1.15,
                         calibrate = mock_calibrate_v1.1.15)
  model_payload <- setup_payload_submit()
  model_submit <- submit_model(q$queue)
  run_response <- model_submit(model_payload)
  expect_true("id" %in% names(run_response))
  result <- q$queue$queue$task_wait(run_response$id)

  calibrate_payload <- setup_payload_calibrate()
  model_calibrate <- submit_calibrate(q$queue)
  calibrate_response <- model_calibrate(run_response$id, calibrate_payload)
  expect_true("id" %in% names(calibrate_response))
  result <- q$queue$queue$task_wait(calibrate_response$id)

  ## Store some data we'll use for testing later
  mock_run_result <- q$queue$result(q$model_run_id)
  mock_calibrate_result <- q$queue$result(q$calibrate_id)
  real_run_result <- q$queue$result(run_response$id)
  real_calibrate_result <- q$queue$result(calibrate_response$id)

  ## Run migration
  t <- tempfile()
  dir.create(t)
  msg <- capture_messages(migrate <- run_migration(q$queue, t, "2.9.10",
                                                   "2.9.11", dry_run = TRUE))

  expect_equal(sum(grepl("Migrating", msg)), 4) ## 4 Migrating messages
  expect_equal(sum(grepl(sprintf(
    "Not migrating %s", q$model_run_id), msg)), 1)
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
  expect_setequal(
    summary$action,
    c("Successfully migrated", "No change - up to date",
      "No change - only migrating plot data and this result has none"))

  migration_output <- qs::qread(file.path(t, "output.qs"))
  expect_equal(migration_output, migrate)

  ## Data has not been migrated
  migrated_mock_run_result <- q$queue$result(q$model_run_id)
  migrated_mock_calibrate_result <- q$queue$result(q$calibrate_id)
  migrated_real_run_result <- q$queue$result(run_response$id)
  migrated_real_calibrate_result <- q$queue$result(calibrate_response$id)

  expect_equal(migrated_mock_run_result, mock_run_result)
  expect_equal(migrated_mock_calibrate_result, mock_calibrate_result)
  expect_equal(migrated_real_run_result, real_run_result)
  expect_equal(migrated_real_calibrate_result, real_calibrate_result)
})
