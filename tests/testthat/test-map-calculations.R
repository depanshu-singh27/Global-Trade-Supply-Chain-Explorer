test_that("default year and year choices exclude empty sets", {
  cy <- make_map_analytics_fixture()
  expect_equal(map_year_choices(cy), c(2023L, 2024L))
  expect_equal(choose_map_default_year(cy), 2024L)
})

test_that("aggregate reporters excluded and one row per ISO3-year", {
  cy <- prepare_map_analytics(make_map_analytics_fixture())
  expect_false("EUR" %in% cy$reporter_iso3)
  y <- filter_map_year(cy, 2024L)
  expect_equal(nrow(y), data.table::uniqueN(y$reporter_iso3))
  expect_true(all(c("DEU", "USA", "CHN", "IND") %in% y$reporter_iso3))
})

test_that("region filtering uses geometry metadata", {
  skip_if_not_installed("sf")
  cy <- make_map_analytics_fixture()
  g <- make_synthetic_map_geometry()
  y <- filter_map_year(cy, 2024L, region = "Europe & Central Asia", geometry = g)
  expect_equal(y$reporter_iso3, "DEU")
})

test_that("metric columns and GDP/population invalidation", {
  cy <- prepare_map_analytics(make_map_analytics_fixture())
  y <- filter_map_year(cy, 2024L)
  expect_equal(map_metric_values(y, "imports"), y$imports_value_usd)
  expect_equal(map_metric_values(y, "exports"), y$exports_value_usd)
  expect_equal(map_metric_values(y, "total_trade"), y$total_trade_value_usd)
  expect_equal(map_metric_values(y, "trade_balance"), y$trade_balance_usd)

  chn <- y[reporter_iso3 == "CHN"]
  ind <- y[reporter_iso3 == "IND"]
  expect_true(is.na(chn$total_trade_pct_gdp))
  expect_true(is.na(ind$total_trade_per_capita_usd))
  vals <- map_metric_values(y, "total_trade_pct_gdp")
  expect_false(any(is.infinite(vals), na.rm = TRUE))
  expect_false(any(is.nan(vals), na.rm = TRUE))
})

test_that("trade identity reconciliation", {
  y <- filter_map_year(make_map_analytics_fixture(), 2024L)
  expect_equal(y$imports_value_usd + y$exports_value_usd, y$total_trade_value_usd)
  expect_equal(y$exports_value_usd - y$imports_value_usd, y$trade_balance_usd)
})

test_that("signed and sequential palette domains", {
  dom <- signed_palette_domain(c(-10, 5, 2))
  expect_equal(dom[1], -dom[2])
  expect_true(0 >= dom[1] && 0 <= dom[2])
  sdom <- sequential_palette_domain(c(3, 8, 1))
  expect_equal(sdom[1], 0)
  br <- quantile_breaks(c(1, 1, 1, 2, 3), 4L)
  expect_true(length(br) >= 2)
  expect_equal(br, unique(br))
  meta_s <- build_map_color_meta(c(-5, 0, 4), "trade_balance", "continuous")
  expect_true(meta_s$signed)
  expect_true(meta_s$domain[1] < 0 && meta_s$domain[2] > 0)
  meta_q <- build_map_color_meta(c(1, 2, 3, 4), "imports", "quantile")
  expect_false(meta_q$signed)
})

test_that("geometry ISO3 join does not multiply rows", {
  skip_if_not_installed("sf")
  cy <- filter_map_year(make_map_analytics_fixture(), 2024L)
  g <- make_synthetic_map_geometry()
  j <- join_map_data(cy, g, "trade_balance")
  expect_equal(nrow(j$sf), nrow(g))
  expect_equal(sum(j$crosswalk$geometry_match_status == "matched"), 4L)

  cy2 <- rbind(cy, data.table::data.table(
    reporter_iso3 = "ZZZ", reporter_name = "Zed", year = 2024L,
    imports_value_usd = 1, exports_value_usd = 1, total_trade_value_usd = 2,
    trade_balance_usd = 0, gdp_current_usd = 1, population_total = 1,
    total_trade_pct_gdp = 1, trade_balance_pct_gdp = 0, total_trade_per_capita_usd = 1,
    total_trade_yoy_pct = NA_real_, inflation_annual_pct = NA_real_,
    gdp_per_capita_usd = 1, latest_ingested_at = "t"
  ), fill = TRUE)
  j2 <- join_map_data(cy2, g, "total_trade")
  expect_true("ZZZ" %in% j2$crosswalk[geometry_match_status == "unmatched"]$source_iso3)
})

test_that("mapped-value coverage uses abs for signed metrics", {
  skip_if_not_installed("sf")
  cy <- filter_map_year(make_map_analytics_fixture(), 2024L)
  g <- make_synthetic_map_geometry()
  j <- join_map_data(cy, g, "trade_balance")
  cov <- mapped_value_coverage(cy, j$sf, "trade_balance")
  expect_equal(cov$method, "absolute_value_ratio")
  expect_true(is.finite(cov$coverage_pct))
  cov2 <- mapped_value_coverage(cy, j$sf, "total_trade")
  expect_equal(cov2$method, "value_ratio")
})

test_that("KPI, profile, trend, rankings", {
  cy <- make_map_analytics_fixture()
  y <- filter_map_year(cy, 2024L)
  k <- map_kpi_summary(y)
  expect_equal(k$imports + k$exports, k$total_trade)
  expect_equal(k$n_surplus + k$n_deficit + k$n_zero, k$n_economies)
  p <- selected_country_profile(cy, "DEU", 2024L)
  expect_true(p$available)
  expect_equal(p$total_trade, 240)
  tr <- prepare_country_trend(cy, "DEU")
  expect_equal(tr$year, c(2023L, 2024L))
  sur <- map_surplus_ranking(y, 10L)
  expect_true(all(sur$trade_balance_usd >= 0))
  def <- map_deficit_ranking(y, 10L)
  expect_true(all(def$trade_balance_usd < 0))
})

test_that("surplus ranking tie-break is ISO3 ascending among equals", {
  dt <- data.table::data.table(
    reporter_iso3 = c("BBB", "AAA", "CCC"),
    reporter_name = c("B", "A", "C"),
    trade_balance_usd = c(10, 10, 5)
  )
  r <- map_surplus_ranking(dt, 2L)
  expect_equal(r$reporter_iso3, c("AAA", "BBB"))
})

test_that("filename sanitisation and download column safety", {
  fn <- map_download_filename("trade_balance_map", 2024, "trade balance!")
  expect_equal(fn, "trade_balance_map_2024_trade_balance.csv")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write_map_csv(data.table::data.table(a = 1, raw_file = "/secret", geometry = "x"), tmp)
  got <- data.table::fread(tmp)
  expect_false(any(grepl("raw_file|geometry|secret", names(got), ignore.case = TRUE)))
})

test_that("accessibility summary generation", {
  y <- filter_map_year(make_map_analytics_fixture(), 2024L)
  s <- map_accessibility_summary(y, "trade_balance", "DEU")
  expect_true(grepl("Selected country: DEU", s))
  expect_true(grepl("Missing values", s))
})
