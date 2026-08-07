test_that("pipeline recover, cache skip and quota stop helpers", {
  plan <- plan_monthly_forecast_requests(
    data.table::data.table(
      series_id = "DEU__CHN__8542__M",
      reporter_iso3 = "DEU", partner_iso3 = "CHN",
      reporter_code = "276", partner_code = "156",
      hs_code = "8542", flow_code = "M",
      candidate_version = FORECAST_CANDIDATE_VERSION
    )
  )
  st <- init_state_from_plan(plan)
  expect_true(all(st$status == "planned"))
  stale_ts <- format(
    as.POSIXct(Sys.time(), tz = "UTC") - 5 * 3600,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  st[1, `:=`(status = "running", started_at = stale_ts)]
  st2 <- recover_stale_running(st, stale_minutes = 120)
  expect_equal(st2$status[1], "planned")
  cls <- classify_http_failure(403L, body_text = "quota exceeded")
  expect_equal(cls$status, "quota_blocked")
})

test_that("downloads strip secrets and paths", {
  dt <- data.table::data.table(
    series_id = "DEU__CHN__8542__M",
    predicted_value_usd = 1,
    secret_key = "should_drop",
    raw_path = "B:\\Global Trade\\data\\raw\\x.json"
  )
  out <- forecast_download_table(dt)
  expect_false("secret_key" %in% names(out))
  expect_false(any(grepl("path|key", names(out), ignore.case = TRUE)))
  expect_false(forecast_contains_forbidden_content(out))
  expect_equal(mape_unavailable_message(), "Unavailable due to zero or missing actual values.")
  expect_false(grepl("15%", forecast_methodology_notice()))
})

test_that("candidate ranking excludes aggregates world and self", {
  det <- data.table::data.table(
    reporter_iso3 = c("DEU", "DEU", "DEU", "DEU"),
    partner_iso3 = c("CHN", "WLD", "DEU", "USA"),
    hs_code = c("8542", "8542", "8542", "TOTAL"),
    flow_code = c("M", "M", "M", "M"),
    year = 2024L,
    trade_value_usd = c(1e9, 9e9, 1e8, 5e8),
    reporter_name = "Germany", partner_name = "x", commodity_description = "y",
    reporter_code = "276", partner_code = c("156", "0", "276", "842")
  )
  cand <- build_annual_forecast_candidates(det, top_n = 10L)
  expect_false(any(cand$partner_iso3 %in% c("WLD", "W00")))
  expect_false(any(cand$partner_iso3 == cand$reporter_iso3))
  expect_false(any(cand$hs_code %in% c("TOTAL", "9999")))
})
