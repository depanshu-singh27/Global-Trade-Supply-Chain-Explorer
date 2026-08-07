test_that("pipeline state initializes missing requests as planned", {
  cfg <- load_config("development", TEST_ROOT)
  plan_dt <- data.table::data.table(
    request_id = c("r1", "r2"),
    dataset_type = c("dA", "dB")
  )
  st0 <- data.table::data.table(request_id = "r1", dataset_type = "dA", status = "succeeded",
                                attempts = 1L, started_at = NA_character_, completed_at = NA_character_,
                                http_status = NA_integer_, result_row_count = NA_integer_,
                                raw_file = "x", raw_checksum = "abc",
                                error_category = NA_character_, error_message = NA_character_)

  st <- init_state_from_plan(plan_dt, existing_state = st0)
  expect_true(all(plan_dt$request_id %in% st$request_id))
  expect_equal(st[request_id == "r1"]$status, "succeeded")
  expect_equal(st[request_id == "r2"]$status, "planned")
})

test_that("recover_stale_running moves old running requests to planned", {
  st <- data.table::data.table(
    request_id = c("r1"),
    status = "running",
    started_at = as.character(as.POSIXct(Sys.time(), tz = "UTC") - (200 * 60)),
    attempts = 1L,
    dataset_type = "dA",
    completed_at = NA_character_,
    http_status = NA_integer_,
    result_row_count = NA_integer_,
    raw_file = NA_character_,
    raw_checksum = NA_character_,
    error_category = NA_character_,
    error_message = NA_character_
  )
  st2 <- recover_stale_running(st, stale_minutes = 120)
  expect_equal(st2$status, "planned")
})

test_that("select_requests_to_run respects max_requests", {
  st <- data.table::data.table(
    request_id = c("r1", "r2", "r3"),
    dataset_type = c("d", "d", "d"),
    status = c("planned", "retryable_failed", "permanently_failed"),
    attempts = 0L,
    started_at = NA_character_,
    completed_at = NA_character_,
    http_status = NA_integer_,
    result_row_count = NA_integer_,
    raw_file = NA_character_,
    raw_checksum = NA_character_,
    error_category = NA_character_,
    error_message = NA_character_
  )
  st_sel <- select_requests_to_run(st, retry_failed_only = FALSE, max_requests = 1)
  expect_equal(nrow(st_sel), 1L)
  expect_true(st_sel$request_id %in% c("r1", "r2"))
})
