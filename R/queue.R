global_queue <- new.env(parent = emptyenv())

model_queue_start <- function(root, workers = 2, name = "hintr",
                              global = TRUE) {
  if (!global || is.null(global_queue$queue)) {
    ctx <- context::context_load(context_init(root, name))

    message("connecting to redis at ", redux::redis_config()$url)
    con <- redux::hiredis()

    message("Starting queue")
    rrq <- rrq::rrq_controller(ctx, con)
    if (workers > 0L) {
      rrq::worker_spawn(rrq, workers)
      reg.finalizer(rrq, model_queue_finalize)
    }

    if (!global) {
      # return(rrq)
    }

    global_queue$queue <- rrq
  }
  invisible(global_queue$queue)
}

model_queue_submit <- function(data, queue = global_queue$queue) {
  queue$enqueue_(quote(hintr:::run_model(data)))
}

model_queue_status <- function(id, queue = global_queue$queue) {
  status <- unname(queue$task_status(id))
  done <- c("ERROR", "COMPLETE")
  if (status %in% done) {
    list(done = TRUE,
         status = status,
         success = status == "COMPLETE",
         queue = 0)
  } else {
    list(done = FALSE,
         status = status,
         success = NULL,
         queue = queue$task_position(id))
  }
}


model_queue_result <- function(id, queue = global_queue$queue) {
  queue$task_result(id)
}


model_queue_remove <- function(id, queue = global_queue$queue) {
  queue$task_delete(id)
}

## Not part of the api exposed functions, used in tests
model_queue_stop <- function(queue = global_queue$queue) {
  global <- identical(queue, global_queue$queue)
  queue$destroy(delete = TRUE)
  if (global) {
    global_queue$queue <- NULL
  }
}

model_queue_finalize <- function(queue) {
  message("Stopping workers")
  queue$worker_stop()
}

## Support for queue building
context_init <- function(root, name = "hintr") {
  context::context_save(root,
                        sources = character(0),
                        packages = "hintr",
                        name = name)
}
