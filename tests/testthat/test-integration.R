context("integration")

test_that("queue can be setup and run end-to-end", {
  test_redis_available()
  local({
    rrq <- model_queue_start(tempfile())
    id <- model_queue_submit(list(sleep = 0.0))
    repeat {
      if (model_queue_status(id)$status %in% c("COMPLETE", "ERROR")) {
        break
      }
      sleep(1)
    }
    result <- model_queue_result(id)
    expect_equal(result, 2)
  })
})
