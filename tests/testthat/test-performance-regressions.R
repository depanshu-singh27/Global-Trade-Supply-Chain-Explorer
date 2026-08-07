test_that("warmup exclusion and comparison helpers", {
  base <- data.table::data.table(
    module = "overview", operation = "overview_year_aggregations",
    dataset_tier = "actual", cold_or_warm = "warm", cache_state = "n/a",
    median_ms = 10, p95_ms = 12, iterations = 3L, result_checksum = "chk_a"
  )
  opt <- data.table::copy(base)
  opt[, `:=`(median_ms = 8, p95_ms = 9, result_checksum = "chk_a")]
  cmp <- compare_benchmark_phases(base, opt)
  expect_equal(nrow(cmp), 1L)
  expect_true(isTRUE(cmp$checksum_match))
  expect_true(cmp$median_improvement_pct > 0)
})

test_that("250ms claim rejected without evidence", {
  rows <- data.table::data.table(
    operation = "shock_actual_capacity_no_persist",
    benchmark_status = "ok",
    p95_ms = 500
  )
  claim <- claim_250ms_supported(rows)
  expect_false(isTRUE(claim$supported))
})

test_that("validation distinguishes tiers and forbids browser claims", {
  cfg <- make_tiny_perf_cfg()
  runs <- data.table::data.table(
    module = c("overview", "forecasting", "tier3"),
    operation = c("a", "forecast_ui_filter_prepare", "tier3_complete_coverage"),
    dataset_tier = c("actual", "fixture", "future_complete"),
    dataset_mode = c("actual_processed", "fixture_synthetic_non_production", "unavailable"),
    cold_or_warm = c("warm", "warm", "warm"),
    median_ms = c(1, 2, NA_real_),
    p95_ms = c(2, 3, NA_real_),
    minimum_ms = c(1, 1, NA_real_),
    maximum_ms = c(3, 4, NA_real_),
    benchmark_status = c("ok", "ok", "unavailable"),
    browser_automation = FALSE
  )
  v <- validate_performance_results(runs, cfg)
  expect_true(all(v$status[v$check_id %in% c("p95_ge_median", "fixture_forecast_labelled", "tier3_unavailable_while_partial", "browser_not_claimed")] == "pass"))
})

test_that("semantic equivalence smoke for overview and shock fixtures", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  sc <- make_base_scenario(acknowledge_partial_coverage = TRUE)
  r1 <- run_shock_scenario(det, sc, coverage = cov)
  r2 <- run_shock_scenario(det, sc, coverage = cov)
  expect_true(isTRUE(r1$ok))
  expect_equal(r1$scenario$scenario_hash %||% r1$manifest$scenario_hash,
               r2$scenario$scenario_hash %||% r2$manifest$scenario_hash)
})

test_that("absent performance UI state is safe", {

  out <- load_performance_summary_for_ui()
  expect_true(is.list(out))
  expect_true("available" %in% names(out))
})

test_that("no unsupported 250ms claim text in README performance section if absent evidence", {
  readme <- paste(readLines(file.path(TEST_ROOT, "README.md"), warn = FALSE), collapse = "\n")

  expect_false(grepl("shock simulation (completes|runs) under 250 ms", readme, ignore.case = TRUE))
})
