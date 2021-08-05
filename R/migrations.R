run_migration <- function(queue, output_dir, dry_run = FALSE) {
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  tasks <- queue$queue$task_list()
  status <- queue$queue$task_status(tasks)
  completed_tasks <- tasks[status == "COMPLETE"]
  migrations <- lapply(completed_tasks, migrate_task, queue, dry_run)
  summary <- lapply(migrations, function(migration) {
    list(
      id = migration$id,
      action = migration$action
    )
  })
  summary <- do.call(rbind, summary)
  summary_path <- file.path(output_dir, "summary.csv")
  message(sprintf("Saving summary csv %s", summary_path))
  output_path <- file.path(output_dir, "output.rds")
  write.csv(summary, summary_path, row.names = FALSE)
  message(sprintf("Saving output RDS %s", output_path))
  saveRDS(migrations, output_path)
  migrations
}

migrate_task <- function(task_id, queue, dry_run = FALSE) {
  message(sprintf("Migrating %s", task_id))
  res <- queue$queue$task_result(task_id)
  if (!is.null(res$version) && identical(as.character(res$version), "2.5.0")) {
    message(sprintf("Not migrating %s, already up to date", task_id))
    return(list(
      id = task_id,
      prev_res = res,
      action = "No change - up to date"
    ))
  }
  if (!all(c("output_path", "calibration_path") %in% names(res))) {
    message(sprintf("Not migrating %s, invalid output format", task_id))
    return(list(
      id = task_id,
      prev_res = res,
      action = "No change - not migrateable"
    ))
  }

  new_res <- migrate(res)
  if (!dry_run) {
    store <- rrq:::rrq_object_store(queue$queue$con,
                                    queue$queue$keys)
    hash <- store$set(new_res, task_id)
    queue$queue$con$HSET(queue$queue$keys$task_result, task_id, hash)
  }
  out <- list(
    id = task_id,
    prev_res = res,
    new_res = new_res,
    from = "x.x.x",
    to = "2.5.0",
    action = "Successfully migrated"
  )
  message(sprintf("Successfully migrated %s", task_id))
  out
}

migrate <- function(res) {
  naomi:::build_hintr_output(res$output_path,
                             res$calibration_path)
}
