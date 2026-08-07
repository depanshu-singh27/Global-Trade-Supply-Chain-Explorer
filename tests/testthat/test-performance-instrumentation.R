test_that("timing stats exclude warmups and compute median/p95", {
  i <- 0L
  timed <- run_timed_iterations(function() {
    i <<- i + 1L
    Sys.sleep(0.001)
    i
  }, iterations = 4L, warmup = 2L, profile_memory = TRUE)
  expect_equal(timed$iterations, 4L)
  expect_equal(timed$warmup_iterations, 2L)
  expect_equal(i, 6L)
  expect_true(is.finite(timed$median_ms))
  expect_true(timed$p95_ms + 1e-9 >= timed$median_ms)
  expect_true(timed$minimum_ms <= timed$maximum_ms + 1e-9)
  expect_false(is.infinite(timed$median_ms))
  expect_false(is.nan(timed$median_ms))
})

test_that("perf counters disabled by default and increment when enabled", {
  reset_perf_counters()
  enable_perf_counters(FALSE)
  expect_false(perf_counters_enabled())
  inc_perf_counter("snapshot_load_count")
  expect_equal(get_perf_counters()$snapshot_load_count, 0L)
  enable_perf_counters(TRUE)
  reset_perf_counters()
  inc_perf_counter("snapshot_load_count")
  expect_equal(get_perf_counters()$snapshot_load_count, 1L)
  enable_perf_counters(FALSE)
  reset_perf_counters()
})

test_that("benchmark row metadata and digest are safe", {
  timed <- run_timed_iterations(function() list(ok = TRUE), iterations = 2L, warmup = 0L)
  cfg <- make_tiny_perf_cfg()
  env <- capture_benchmark_environment(cfg)
  row <- make_benchmark_row(
    timed, list(cfg = cfg, env = env),
    operation = "unit_op", module = "unit",
    dataset_tier = "synthetic", cold_or_warm = "warm"
  )
  expect_false(isTRUE(row$browser_automation))
  expect_false(grepl(paste0("COMTRADE", "_", "PRIMARY"), paste(row, collapse = " ")))
  expect_true(startsWith(row$result_checksum, "chk_"))
})
