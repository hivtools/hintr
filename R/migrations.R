run_migration <- function(queue, log_dir, to_version, dry_run = TRUE) {
  log_dir <- normalizePath(log_dir, mustWork = TRUE)
  tasks <- rrq::rrq_task_list(controller = queue$controller)
  status <- rrq::rrq_task_status(tasks, controller = queue$controller)

  running_tasks <- tasks[status %in% c("RUNNING", "PENDING")]
  for (task in running_tasks) {
    ## Wait for running tasks to finish before attempting migration
    message(sprintf("Waiting for task %s", task))
    rrq::rrq_task_wait(task, controller = queue$controller)
  }

  completed_tasks <- tasks[status == "COMPLETE"]
  migrations <- lapply(completed_tasks, migrate_task, queue,
                       to_version, dry_run)
  write_migration_log(migrations, log_dir)
}

write_migration_log <- function(migrations, log_dir) {
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
  res <- rrq::rrq_task_result(task_id, controller = queue$controller)
  if (!naomi:::is_hintr_output(res) ||
      !all(c("plot_data_path", "model_output_path") %in% names(res))) {
    is_download <- "path" %in% names(res)
    if (is_download) {
      msg <- sprintf("Not migrating %s, this is a download output", task_id)
      log <- "No change - not migrating download outputs"
    } else {
      msg <- sprintf("Not migrating %s, invalid output format", task_id)
      log <- "No change - not migrateable"
    }
    message(msg)
    return(list(
      id = task_id,
      prev_res = res,
      action = log
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
  if (!file.exists(res$plot_data_path)) {
    ## Have seen some instances of prod where plot data doesn't exist
    ## it's probably really old model fit so not going to
    ## worry about it to much and just skip it
    message(
      sprintf("Not migrating %s, plot data path does not exist", task_id))
    return(list(
      id = task_id,
      prev_res = res,
      action = "No change - plot data path does not exist"
    ))
  }

  new_res <- migrate(res, to_version, dry_run)
  if (!dry_run) {
    ## rrq stores results using an object store
    ## So when an rrq completes a job successfully it generates an R object
    ## This R object is either serialized and stored in redis or if it is
    ## big it can be offloaded onto disk. The object store manages this
    ## and also manages reference counting to avoid saving
    ## duplicate values into redis or to disk. Note that this is different
    ## to how naomi outputs are stored on disk.
    ##
    ## When a job finishes, the R object is hashed. If that hash already
    ## exists in the object store then we add a new reference to it
    ## with tag equal to the task id. This returns the hash which can be
    ## used to locate this object later.
    ## We then save the has into redis at the `task_result` key for this task
    ##
    ## So in Naomi example what happens is,
    ## run_model or run_calibrate return a hintr_output object
    ## this is a list which contains things like plot_data_path,
    ## model_output_path, version, warnings
    ## This is hashed and saved into the object store. Then that gives
    ## a hash back which we save as the task result.
    ##
    ## So to change the result saved for a task during this migration we
    ## need to
    ## 1. Add the new result into the object store, getting the hash back
    ## 2. Save the hash into redis as the `task_result`
    ## The object store is an internal part of rrq and not exposed so
    ## we need to be a big naughty here use ::: to access it.
    store <- queue$controller$store
    hash <- store$set(new_res, task_id)
    queue$controller$con$HSET(queue$controller$keys$task_result, task_id,
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


run_task_data_migration <- function(queue, log_dir, to_version, dry_run = TRUE) {
  log_dir <- normalizePath(log_dir, mustWork = TRUE)
  tasks <- rrq::rrq_task_list(controller = queue$controller)

  migrations <- lapply(tasks, migrate_task_data, queue$controller,
                       to_version, dry_run)
  write_migration_log(migrations, log_dir)
}

migrate_task_data <- function(task_id, controller, to_version, dry_run) {
  message(sprintf("Migrating %s", task_id))

  con <- controller$con
  keys <- controller$keys
  expr <- con$HGET(keys$task_expr, task_id)
  task <- redux::bin_to_object(expr)

  expected_keys_old <- c("expr", "objects")
  expected_keys_new <- c("expr", "variables", "type")
  is_old_task <- has_keys(task, expected_keys_old)
  is_new_task <- has_keys(task, expected_keys_new)
  if (is_old_task) {
    new_res <- task
    new_res$type <- "expr"
    new_res$variables <- new_res$objects
    new_res$objects <- NULL

    ## Test we can read it with rrq
    t <- rrq:::task_load_from_store(new_res, controller$store)
    if (!has_keys(t, expected_keys_new)) {
      stop(sprintf("Failed to migrate task %s", task_id))
    }
  } else if (is_new_task) {
    message(sprintf("Not migrating %s, this is already a new task", task_id))
    return(list(
      id = task_id,
      prev_res = task,
      action = "Not migrating, this is already a new task"
    ))
  } else {
    message(sprintf("Not migrating %s, unknown task type", task_id))
    return(list(
      id = task_id,
      prev_res = task,
      action = "Not migrating, unknown task type"
    ))
  }

  if (!dry_run) {
    new_expr <- redux::object_to_bin(new_res)
    con$HSET(keys$task_expr, task_id, new_expr)
  }
  out <- list(
    id = task_id,
    prev_res = task,
    new_res = new_res,
    to = to_version,
    action = "Successfully migrated"
  )
  message(sprintf("Successfully migrated %s", task_id))
  out
}

has_keys <- function(what, keys) {
  length(names(what)) == length(keys) && all(names(what) %in% keys)
}
