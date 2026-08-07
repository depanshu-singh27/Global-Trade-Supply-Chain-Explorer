test_that("should_skip_cached returns TRUE only for succeeded + matching checksum", {
  cfg <- load_config("development", TEST_ROOT)
  tmp <- tempfile(fileext = ".json")
  writeLines("hello", tmp, useBytes = TRUE)
  md5 <- safe_md5_file(tmp)

  st <- data.table::data.table(
    request_id = "r1",
    dataset_type = "trade_global_hs85_annual",
    status = "succeeded",
    attempts = 1L,
    started_at = NA_character_,
    completed_at = NA_character_,
    http_status = 200L,
    result_row_count = 1L,
    raw_file = tmp,
    raw_checksum = md5,
    error_category = NA_character_,
    error_message = NA_character_
  )
  req_row <- data.table::data.table(request_id = "r1", raw_file = tmp)
  expect_true(should_skip_cached(st, req_row, cfg = cfg))

  st$raw_checksum <- "bad"
  expect_false(should_skip_cached(st, req_row, cfg = cfg))
})
