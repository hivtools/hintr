run_migration <- function(queue, output_dir) {
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  tasks <- queue$queue$task_list()
  migrations <- lapply(tasks, migrate_task, queue, output_dir)
  summary <- lapply(migrations, function(migration) {
    list(
      id = migration$id,
      action = migration$action,
      migration_file = migration$migration_file
    )
  })
  summary <- do.call(rbind, summary)
  summary_path <- file.path(output_dir, "summary.csv")
  message(sprintf("Saving summary csv %s", summary_path))
  write.csv(summary, summary_path, row.names = FALSE)
  migrations
}

migrate_task <- function(task_id, queue, output_dir) {
  message(sprintf("Migrating %s", task_id))
  output_file <- file.path(output_dir,
                           migrate_filename(task_id, "x.x.x", "2.4.0"))
  if (file.exists(output_file)) {
    message(sprintf("Not migrating %s, task aleady migrated see details: %s",
                    task_id, output_file))
    out <- list(
      id = task_id,
      action = "Result already migrated",
      migration_file = output_file
    )
    return(out)
  }
  status <- queue$queue$task_status(task_id)
  if (!identical(unname(status), "COMPLETE")) {
    message(sprintf(
      "Not migrating %s task not complete, task status: %s, details: %s",
      task_id, status, output_file))
    out <- list(
      id = task_id,
      status = status,
      action = "No change - incomplete model run",
      migration_file = output_file
    )
    saveRDS(out, file = output_file)
    return(out)
  }

  res <- queue$queue$task_result(task_id)
  if (!is.null(res$version) && identical(as.character(res$version), "2.4.0")) {
    message(sprintf("Not migrating %s, already up to date, details: %s",
                    task_id, status, output_file))
    out <- list(
      id = task_id,
      status = status,
      prev_res = res,
      action = "No change - up to date",
      migration_file = output_file
    )
    saveRDS(out, file = output_file)
    return(out)
  }
  if (!all(c("output_path", "calibration_path") %in% names(res))) {
    message(sprintf("Not migrating %s, invalid output format, details:",
                    task_id, output_file))
    out <- list(
      id = task_id,
      status = status,
      prev_res = res,
      action = "No change - not migrateable",
      migration_file = output_file
    )
    saveRDS(out, file = output_file)
    return(out)
  }

  new_res <- migrate(res)
  queue$queue$con$HSET(queue$queue$keys$task_result, task_id,
                       redux::object_to_bin(new_res))
  out <- list(
    id = task_id,
    status = status,
    prev_res = res,
    new_res = new_res,
    from = "x.x.x",
    to = "2.4.0",
    action = "Successfully migrated",
    migration_file = output_file
  )
  saveRDS(out, file = output_file)
  message(sprintf("Successfully migrated %s, details: %s",
                  task_id, output_file))
  out
}

migrate <- function(res) {
  naomi:::build_hintr_output(res$output_path,
                             res$calibration_path)
}

migrate_filename <- function(task_id, from_version, to_version) {
  sprintf("%s-migrate-%s-%s", task_id, from_version, to_version)
}
