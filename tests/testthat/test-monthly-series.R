test_that("monthly calendar preserves missing vs zero", {
  raw <- data.table::data.table(
    period = c("202301", "202302"),
    year = c(2023L, 2023L),
    month = c(1L, 2L),
    reporter_iso3 = "DEU", partner_iso3 = "CHN", hs_code = "8542", flow_code = "M",
    reporter_name = "Germany", partner_name = "China", commodity_description = "ICs",
    reporter_code = "276", partner_code = "156", flow_name = "Import", hs_revision = "HS",
    trade_value_usd = c(100, 0), source_row_count = 1L,
    source_updated_at = NA_character_, request_id = "r1"
  )
  meta <- data.table::data.table(
    series_id = "DEU__CHN__8542__M",
    reporter_iso3 = "DEU", partner_iso3 = "CHN", hs_code = "8542", flow_code = "M",
    reporter_name = "Germany", partner_name = "China", commodity_description = "ICs",
    reporter_code = "276", partner_code = "156"
  )
  long <- build_monthly_series_long(raw, meta, start_year = 2023L, end_year = 2023L)
  expect_equal(nrow(long), 12L)
  expect_true(long[period == "202301"]$value_observed)
  expect_true(long[period == "202302"]$value_zero)
  expect_true(long[period == "202303"]$value_missing)
  expect_false(long[period == "202303"]$value_zero)
  expect_equal(make_series_id("DEU", "CHN", "8542", "M"), "DEU__CHN__8542__M")
})

test_that("quality and stable selection ignore forecast performance", {
  bundle <- make_forecast_fixture_bundle(12L)
  q <- compute_monthly_series_quality(bundle$monthly_long)
  expect_true(nrow(q) >= 10)
  sel <- select_stable_forecast_series(q, candidates = bundle$candidates, n = 10L)
  expect_true(nrow(sel) <= 10)
  expect_true(all(sel$stable))

  expect_true(any(q$selection_or_rejection_reason != "meets_stability_criteria") || nrow(sel) == nrow(q))
})

test_that("imputation audit and excessive gap rejection", {
  s <- make_monthly_fixture_series(missing_months = 10:20, n = 36L)
  rej <- prepare_forecast_model_input(s, mode = "short_gap_linear", max_gap = 2L)
  expect_true(isTRUE(rej$rejected))
  s2 <- make_monthly_fixture_series(missing_months = 10L, n = 36L)
  ok <- prepare_forecast_model_input(s2, mode = "short_gap_linear", max_gap = 2L)
  expect_false(isTRUE(ok$rejected))
  expect_true(ok$imputed_months >= 1L)
})
