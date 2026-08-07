test_that("default latest-year selection prefers adequate macro coverage", {
  cy <- make_overview_fixture()

  expect_equal(choose_overview_default_year(cy), 2022L)
})

test_that("reporter filter choices exclude aggregates", {
  cy <- make_overview_fixture()
  ch <- overview_reporter_choices(cy)
  expect_true("__GLOBAL__" %in% ch)
  expect_false("EUR" %in% unname(ch))
  expect_false("WLD" %in% unname(ch))
  expect_true("DEU" %in% unname(ch))
})

test_that("global KPI calculation reconciles imports+exports and balance", {
  cy <- make_overview_fixture()
  k <- overview_kpi_global(cy, 2023L)
  expect_equal(k$imports + k$exports, k$total_trade)
  expect_equal(k$exports - k$imports, k$trade_balance)
  expect_true(k$checks$total_equals_sum)
  expect_true(k$checks$balance_equals_diff)

  expect_equal(k$n_reporters, 3L)
  expect_equal(k$total_trade, 240 + 145 + 155)
})

test_that("reporter KPI calculation is reporter-scoped", {
  cy <- make_overview_fixture()
  k <- overview_kpi_reporter(cy, 2023L, "DEU")
  expect_equal(k$total_trade, 240)
  expect_equal(k$imports, 110)
  expect_equal(k$exports, 130)
  expect_equal(k$trade_balance, 20)
  expect_true(k$checks$total_equals_sum)
  expect_true(k$checks$balance_equals_diff)
})

test_that("import/export composition reconciles and handles zero total", {
  cy <- make_overview_fixture()
  c <- overview_composition(cy, 2023L, "__GLOBAL__")
  expect_equal(c$imports + c$exports, c$total)
  expect_equal(c$imports_share_pct + c$exports_share_pct, 100, tolerance = 1e-8)
  empty <- overview_composition(
    data.table::data.table(
      reporter_iso3 = "ZZZ", reporter_name = "Z", year = 2023L,
      imports_value_usd = 0, exports_value_usd = 0,
      total_trade_value_usd = 0, trade_balance_usd = 0,
      gdp_current_usd = NA_real_, population_total = NA_real_,
      total_trade_pct_gdp = NA_real_
    ),
    2023L, "ZZZ"
  )
  expect_true(is.na(empty$imports_share_pct) || identical(empty$total, 0))
})

test_that("consecutive-year comparison and missing previous year", {
  cy <- make_overview_fixture()
  k23 <- overview_kpi_global(cy, 2023L)
  expect_false(is.na(k23$yoy_total_pct))
  k22 <- overview_kpi_global(cy, 2022L)
  expect_true(is.na(k22$yoy_total_pct))
})

test_that("top-10 ranking is deterministic with stable tie-breaking", {
  cy <- make_tie_fixture()
  r <- overview_top_economies(cy, 2023L, "total", top_n = 3L)
  expect_equal(r$reporter_iso3, c("AAA", "BBB", "CCC"))
  expect_equal(r$value, c(100, 100, 100))
})

test_that("aggregate exclusion from rankings", {
  cy <- make_overview_fixture()
  r <- overview_top_economies(cy, 2023L, "total", top_n = 10L)
  expect_false("EUR" %in% r$reporter_iso3)
})

test_that("trade-balance signed ranking and lowest mode", {
  cy <- make_overview_fixture()
  high <- overview_top_economies(cy, 2023L, "balance", top_n = 3L, lowest_balance = FALSE)
  low <- overview_top_economies(cy, 2023L, "balance", top_n = 3L, lowest_balance = TRUE)
  expect_true(high$value[1] >= high$value[nrow(high)])
  expect_true(low$value[1] <= low$value[nrow(low)])
  expect_equal(low$reporter_iso3[1], "IND")
})

test_that("trade-balance distribution calculation", {
  cy <- make_overview_fixture()
  d <- overview_balance_distribution(cy, 2023L)
  expect_equal(d$n_surplus + d$n_deficit + d$n_zero, d$n)
  expect_true(d$n_surplus >= 1)
  expect_true(d$n_deficit >= 1)
  expect_false(is.na(d$median_balance))
})

test_that("macro scatter preparation and log-axis exclusion", {
  cy <- make_overview_fixture()
  prep <- overview_macro_scatter(cy, 2023L, use_log = TRUE)
  expect_true(prep$excluded_log >= 1)
  expect_true(all(prep$data$gdp > 0))
  expect_true(all(prep$data$total_trade > 0))
  expect_false(any(is.infinite(prep$data$gdp)))
})

test_that("no infinite chart values in trend series", {
  cy <- make_overview_fixture()
  s <- overview_trend_series(cy, "__GLOBAL__")
  expect_false(any(is.infinite(unlist(s[, .(imports, exports, total, balance)]))))
})

test_that("filtered download contents and KPI summary", {
  cy <- make_overview_fixture()
  k <- overview_kpi_global(cy, 2023L)
  tab <- kpi_summary_table(k)
  expect_true(all(c("metric", "value", "year", "mode") %in% names(tab)))
  expect_false(any(grepl("path|secret|COMTRADE", names(tab), ignore.case = TRUE)))
  filtered <- filter_overview_data(cy, year = 2023L, reporter = "DEU")
  expect_equal(nrow(filtered), 1L)
  expect_equal(filtered$reporter_iso3, "DEU")
})

test_that("coverage status reads global complete and detailed partial", {
  snap <- list(
    trade_global = data.table::data.table(x = 1),
    country_year_analytics = make_overview_fixture(),
    wdi_production_wide = data.table::data.table(year = 2020:2023, ingested_at = "2024-01-01"),
    production_manifest = list(
      production_status = "partial",
      selected_reporter_count = 20L,
      represented_reporter_count = 6L,
      universe_version = "uv_262deb46e00d2f216a5a"
    ),
    macro_profile = list(detailed_represented_reporters = c("DEU", "IND")),
    phase3_validation = data.table::data.table(status = c("pass", "warning")),
    production_validation = data.table::data.table(status = "pass"),
    pipeline_status = list(detailed_trade = "partial")
  )
  cov <- overview_coverage_status(snap)
  expect_equal(cov$global_trade_status, "complete")
  expect_equal(cov$macro_status, "complete")
  expect_equal(cov$detailed_status, "partial")
  expect_equal(cov$detailed_coverage_label, "6/20")
  expect_equal(cov$validation_warnings, 1L)
})
