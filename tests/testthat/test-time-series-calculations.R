test_that("year choices, defaults, aggregate exclusion", {
  cy <- make_ts_global_fixture()
  expect_equal(ts_year_choices(cy), 2019:2022)
  expect_equal(ts_default_year_range(cy), c(2019L, 2022L))
  prep <- prepare_ts_global(cy)
  expect_false("EUR" %in% prep$reporter_iso3)
})

test_that("global aggregation and single/compare filtering", {
  cy <- make_ts_global_fixture()
  g <- aggregate_global_series(cy, 2019, 2022)
  expect_true(all(abs(g$imports + g$exports - g$total) < 1e-6))
  expect_equal(g$balance, g$exports - g$imports)
  s <- economy_metric_series(cy, "DEU", "total_trade", 2019, 2022)
  expect_true(all(s$reporter_iso3 == "DEU"))
  expect_equal(nrow(s), 4L)
  cmp <- comparison_metric_series(cy, c("DEU", "USA", "DEU", "CHN", "IND", "FRA"), "imports", 2022, 2022, 5L)
  expect_lte(data.table::uniqueN(cmp$reporter_iso3), 5L)
  expect_false(any(duplicated(cmp$reporter_iso3)))
  expect_false("EUR" %in% cmp$reporter_iso3)
})

test_that("metrics including GDP/population invalidation", {
  cy <- prepare_ts_global(make_ts_global_fixture())
  chn <- economy_metric_series(cy, "CHN", "total_trade_pct_gdp", 2021, 2021)
  expect_true(is.na(chn$value[1]))
  ind <- economy_metric_series(cy, "IND", "trade_per_capita", 2021, 2021)
  expect_true(is.na(ind$value[1]))
  bal <- economy_metric_series(cy, "USA", "trade_balance", 2019, 2022)
  expect_true(any(bal$value < 0))
})

test_that("YoY consecutive, missing year, zero baseline, balance exclusion", {
  y <- c(2019L, 2020L, 2022L)
  v <- c(100, 110, 120)
  yoy <- calc_yoy_pct(y, v, allow = TRUE)
  expect_equal(yoy[2], 10)
  expect_true(is.na(yoy[3]))
  expect_true(is.na(calc_yoy_pct(c(2019L, 2020L), c(0, 10))[2]))
  expect_true(all(is.na(calc_yoy_pct(c(2019L, 2020L), c(-20, -10), allow = FALSE))))
  ch <- calc_balance_change(c(2019L, 2020L), c(-20, -35))
  expect_equal(ch[2], -15)
})

test_that("index baseline and CAGR", {
  years <- 2019:2022
  vals <- c(100, 110, 121, 133.1)
  ix <- calc_index(years, vals)
  expect_equal(ix$baseline_year, 2019L)
  expect_equal(ix$index[1], 100)
  expect_true(abs(ix$index[2] - 110) < 1e-8)
  expect_true(is.na(calc_index(years, c(NA, NA, 10, 20))$index[1]))
  expect_true(abs(calc_cagr(100, 133.1, 2019, 2022) - 10) < 0.1)
  expect_true(is.na(calc_cagr(-10, 20, 2019, 2022)))
  expect_true(is.na(calc_cagr(100, 120, 2022, 2019)))
})

test_that("latest-year ranking stable tie-break", {
  cy <- make_ts_global_fixture()

  cy2 <- prepare_ts_global(cy)
  cy2[year == 2022 & reporter_iso3 %in% c("DEU", "USA"), total_trade_value_usd := 500]
  r <- latest_year_ranking(cy2, "total_trade", 2022, 5L)

  top_equal <- r[value == 500]
  expect_equal(top_equal$reporter_iso3[1:2], c("DEU", "USA"))
})

test_that("share-of-total and transform safety", {
  cmp <- comparison_metric_series(make_ts_global_fixture(), c("DEU", "USA"), "total_trade", 2022, 2022)
  sh <- apply_series_transform(cmp, "total_trade", "share")
  expect_true(abs(sum(sh$display_value, na.rm = TRUE) - 100) < 1e-6)
  bal <- economy_metric_series(make_ts_global_fixture(), "USA", "trade_balance", 2019, 2022)
  tr <- apply_series_transform(bal, "trade_balance", "yoy")
  expect_equal(unique(tr$transform), "balance_change")
  expect_false(any(is.infinite(tr$display_value), na.rm = TRUE))
})

test_that("accessibility summary and filenames", {
  s <- economy_metric_series(make_ts_global_fixture(), "DEU", "total_trade")
  expect_true(grepl("DEU|single|total", ts_accessibility_summary(s, "total_trade", "single"), ignore.case = TRUE))
  fn <- ts_download_filename("trade_timeseries", "DEU", "2019_2024")
  expect_equal(fn, "trade_timeseries_DEU_2019_2024.csv")
})
