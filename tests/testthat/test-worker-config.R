test_that("can build job mapping", {
  queues <- list(
    run = list(
      jobs = list(
        fit = "default"
      )
    )
  )
  mappings <- build_job_mappings(queues)
  expect_equal(mappings, list(fit = list(default = "run")))

  queues <- list(
    run = list(
      jobs = list(
        fit = "default"
      )
    ),
    calibrate_only = list(
      jobs = list(
        calibrate = "default",
        spectrum = "default"
      )
    )
  )
  mappings <- build_job_mappings(queues)
  expect_equal(mappings, list(
    fit = list(default = "run"),
    calibrate = list(default = "calibrate_only"),
    spectrum = list(default = "calibrate_only")
  ))

  queues <- list(
    small = list(
      jobs = list(
        fit = "default",
        spectrum = "default"
      )
    ),
    large = list(
      jobs = list(
        fit = c('COD', 'MOZ'),
        calibrate = "default",
        spectrum = c('COD')
      )
    )
  )
  mappings <- build_job_mappings(queues)
  expect_equal(mappings, list(
    fit = list(default = "small", COD = "large", MOZ = "large"),
    spectrum = list(default = "small", COD = "large"),
    calibrate = list(default = "large")
  ))

  queues <- list(
    small = list(
      jobs = list(
        fit = "default",
        spectrum = "default"
      )
    ),
    medium = list(
      jobs = list(
        fit = 'TZA'
      )
    ),
    large = list(
      jobs = list(
        fit = c('COD', 'MOZ'),
        calibrate = "default",
        spectrum = c('COD')
      )
    )
  )
  mappings <- build_job_mappings(queues)
  expect_equal(mappings, list(
    fit = list(default = "small", TZA = "medium", COD = "large", MOZ = "large"),
    spectrum = list(default = "small", COD = "large"),
    calibrate = list(default = "large")
  ))
})

test_that("worker config must container expected keys", {
  expect_error(
    parse_and_validate_worker_config(list()),
    "Worker config must only have keys 'workers' and 'queues'.",
    fixed = TRUE
  )
})

test_that("worker config must not reference unkown queues", {
  config <- list(
    workers = list(
      x = list(
        queue = "missing",
        heartbeat_period = 2,
        wake_up = TRUE
      )
    ),
    queues = list()
  )
  expect_error(
    parse_and_validate_worker_config(config),
    paste("Worker 'x' is configured to listen to queue(s)",
          "'missing' which is/are missing from config"),
    fixed = TRUE
  )
})

test_that("worker config invalid if duplicate country per job", {
  config <- list(
    workers = list(
      x = list(
        queue = "q",
        heartbeat_period = 2,
        wake_up = TRUE
      )
    ),
    queues = list(
      q = list(
        jobs = list(
          fit = 'default'
        )
      ),
      q2 = list(
        jobs = list(
          fit = 'default'
        )
      )
    )
  )
  expect_error(
    parse_and_validate_worker_config(config),
    paste("Multiple queues configured to handle the same job. Job 'fit' for",
          "country 'default' is configured on multiple queues. It must be",
          "handled by a single queue."),
    fixed = TRUE
  )
})

test_that("can get worker from loaded worker config", {
  worker_cfg <- list(
    job_mapping = list(
      fit = list(
        default = "run",
        COD = "large"
      ),
      calibrate = list(
        default = "calibrate"
      )
    )
  )
  expect_equal(get_queue_from_job_name("fit", "MWI", worker_cfg), "run")
  expect_equal(get_queue_from_job_name("fit", "COD", worker_cfg), "large")
  expect_equal(get_queue_from_job_name("fit", NULL, worker_cfg), "run")
  expect_equal(get_queue_from_job_name("calibrate", "COD", worker_cfg),
               "calibrate")
  expect_error(get_queue_from_job_name("unk", "MWI", worker_cfg),
               "Failed to get relevant queue from config")
})

test_that("can check for valid worker name", {
  expect_true(validate_worker_name("localhost"))
  expect_error(validate_worker_name("unk"),
               paste("Cannot start worker with config 'unk'",
                     "this is not in configuration."))
})
