read_worker_config <- function() {
  worker_config <- Sys.getenv("HINTR_WORKER_CONFIG")
  if (!is.null(worker_config) && worker_config != "") {
    config <- jsonlite::fromJSON(worker_config,
                                 simplifyVector = TRUE)
  } else {
    config <- jsonlite::read_json(system_file("worker_config.json"),
                                  simplifyVector = TRUE)
  }

  ## Reverse the order of the job config so that look-ups later are easier
  ## we want something like list(job_name = list(country_iso3 = queue_name)))
  config$job_mapping <- parse_and_validate_worker_config(config)
  config
}

register_workers <- function(controller) {
  for (worker_name in names(cfg$workers$workers)) {
    args <- cfg$workers$workers[[worker_name]]
    args_pass <- args[formalArgs(rrq::rrq_worker_config)]
    args_pass <- args_pass[!vlapply(args_pass, is.null)]
    message(paste0("Registering worker ", worker_name,
                   " processing queue(s): '",
                   collapse(args_pass$queue, "', '"),
                   "'"))
    worker_cfg <- do.call(rrq::rrq_worker_config,
                          args = args_pass)
    rrq::rrq_worker_config_save(worker_name, worker_cfg,
                                controller = controller)
  }
}

parse_and_validate_worker_config <- function(config) {
  if (!all(c("workers", "queues") %in% names(config))) {
    stop("Worker config must have keys 'workers' and 'queues'.")
  }
  queue_names <- names(config$queues)
  for (worker_name in names(config$workers)) {
    worker <- config$workers[[worker_name]]
    missing <- !(worker$queue %in% queue_names)
    if (any(missing)) {
      stop(paste0("Worker '", worker_name,
                 "' is configured to listen to queue(s) '",
                 collapse(worker$queue[missing], collapse = "', '"),
                 "' which is/are missing from config."))
    }
  }

  job_mapping <- build_job_mappings(config$queues)

  ## Assert that:
  ##   * One and only one queue has "default" for each job
  ##   * A country is not present more than once for each job
  for (job_name in names(job_mapping)) {
    job_cfg <- job_mapping[[job_name]]
    country_names <- names(job_cfg)
    dupes <- country_names[duplicated(country_names)]
    if (length(dupes) > 0) {
      stop(paste0("Multiple queues configured to handle the same job. Job '",
                  job_name,
                  "' for country '",
                  collapse(dupes[[1]]),
                  "' is configured on multiple queues. It must be handled by a ",
                  "single queue."))
    }
  }

  job_mapping
}

build_job_mappings <- function(queues) {
  mappings <- list()
  for (queue_name in names(queues)) {
    queue_config <- queues[[queue_name]]

    lapply(names(queue_config$jobs), function(job_name) {
      countries <- queue_config$jobs[[job_name]]
      job <- stats::setNames(
        lapply(seq_along(countries), function(x) queue_name),
        countries)
      if (!(job_name %in% names(mappings))) {
        mappings[job_name] <<- list(job)
      } else {
        mappings[[job_name]] <<- c(mappings[[job_name]], job)
      }
    })
  }
  mappings
}

get_queue_from_job_name <- function(job_name, iso3 = NULL,
                                    extra_memory = FALSE,
                                    worker_config = cfg$workers) {
  # This option is set if a user explicitly sets this advanced model option
  if (extra_memory) {
    for (queue in names(cfg$workers$queues)) {
      if (cfg$workers$queues[[queue]]$extra_memory_override) {
        return(queue)
      }
    }
  }
  if (!(job_name %in% names(worker_config$job_mapping))) {
    hintr_error(t_("FAILED_QUEUE"), "INVALID_QUEUE_CONFIG")
  }
  queue_config <- worker_config$job_mapping[[job_name]]
  if (!is.null(iso3) && iso3 %in% names(queue_config)) {
    queue_config[[iso3]]
  } else {
    queue_config$default
  }
}

validate_worker_name <- function(name) {
  if (!(name %in% names(cfg$workers$workers))) {
    stop(paste0("Cannot start worker with config '",
                name,
                "' this is not in configuration."))
  }
  invisible(TRUE)
}

wake_up <- function(queue) {
  function() {
    wake_up <- vlapply(cfg$workers$queues, "[[", "wake_up")
    wake_up_queues <- names(cfg$workers$queues)[wake_up]
    ids <- lapply(wake_up_queues, queue$submit_wake)
    names(ids) <- wake_up_queues
    list(ids = ids)
  }
}
