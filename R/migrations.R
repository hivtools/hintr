run_migration <- function(queue, log_dir, to_version, dry_run = TRUE) {
  log_dir <- normalizePath(log_dir, mustWork = TRUE)
  tasks <- queue$queue$task_list()
  status <- queue$queue$task_status(tasks)
  completed_tasks <- tasks[status == "COMPLETE"]
  migrations <- lapply(completed_tasks, migrate_task, queue,
                       to_version, dry_run)
  summary <- lapply(migrations, function(migration) {
    list(
      id = migration$id,
      action = migration$action
    )
  })

  time_now <- iso_time_str()
  summary <- do.call(rbind, summary)
  summary_path <- file.path(log_dir, sprintf("summary_%s.csv", time_now))
  message(sprintf("Saving summary csv %s", summary_path))
  utils::write.csv(summary, summary_path, row.names = FALSE)

  log_path <- file.path(log_dir, sprintf("log_%s.qs", time_now))
  message(sprintf("Saving output qs %s", log_path))
  qs::qsave(migrations, log_path, preset = "fast")
  list(
    summary_path = summary_path,
    log_path = log_path
  )
}

migrate_task <- function(task_id, queue, to_version, dry_run) {
  message(sprintf("Migrating %s", task_id))
  res <- queue$queue$task_result(task_id)
  if (!naomi:::is_hintr_output(res) ||
      !all(c("plot_data_path", "model_output_path") %in% names(res))) {
    message(sprintf("Not migrating %s, invalid output format", task_id))
    return(list(
      id = task_id,
      prev_res = res,
      action = "No change - not migrateable"
    ))
  }
  if (!is.null(res$version) &&
      numeric_version(res$version) >= numeric_version(to_version)) {
    message(sprintf("Not migrating %s, already up to date", task_id))
    return(list(
      id = task_id,
      prev_res = res,
      action = "No change - up to date"
    ))
  }
  if (is.null(res$plot_data_path)) {
    ## This will be null for model fits, only written out during calibrate
    message(
      sprintf("Not migrating %s, this result does not have plot data", task_id))
    return(list(
      id = task_id,
      prev_res = res,
      action = "No change - only migrating plot data and this result has none"
    ))
  }

  new_res <- migrate(res, to_version, dry_run)
  if (!dry_run) {
    store <- rrq:::rrq_object_store(queue$queue$con,
                                    r6_private(queue$queue)$keys)
    hash <- store$set(new_res, task_id)
    queue$queue$con$HSET(r6_private(queue$queue)$keys$task_result, task_id,
                         hash)
  }
  out <- list(
    id = task_id,
    prev_res = res,
    new_res = new_res,
    to = to_version,
    action = "Successfully migrated"
  )
  message(sprintf("Successfully migrated %s", task_id))
  out
}

migrate <- function(res, new_version, dry_run) {
  plot_data <- naomi::read_hintr_output(res$plot_data_path)
  new_plot_data_path <- tempfile("plot_data",
                                 tmpdir = dirname(res$plot_data_path),
                                 fileext = ".duckdb")
  if (!dry_run) {
    naomi:::hintr_save(plot_data, new_plot_data_path)
    unlink(res$plot_data_path)
  }
  res$plot_data_path <- new_plot_data_path
  res$version <- new_version
  res
}

r6_private <- function(x) {
  x[[".__enclos_env__"]]$private
}
