test_that("detailed filtering preserves flows and excludes World", {
  det <- prepare_detailed_trade(make_ts_detailed_fixture())
  expect_false("WLD" %in% det$partner_iso3)
  f <- filter_detailed_trade(det, reporters = "DEU", flows = "M", hs_codes = "8542")
  expect_true(all(f$flow_code == "M"))
  expect_true(all(f$hs_code == "8542"))
})

test_that("detailed trend, top-series limit and omitted coverage", {
  det <- make_ts_detailed_fixture()
  tr <- aggregate_detailed_trend(det, reporters = "DEU", group_by = c("year", "flow_code"))
  expect_true(nrow(tr) > 0)
  expect_true("series" %in% names(tr))
  sel <- select_top_detailed_series(tr, "series", top_n = 1L)
  expect_equal(sel$n_visible, 1L)
  expect_true(is.finite(sel$coverage_pct) || is.na(sel$coverage_pct))
})

test_that("treemap reconciliation and Other grouping", {
  det <- make_ts_detailed_fixture()
  prep <- prepare_commodity_treemap(det, reporters = "DEU", top_n = 1L, include_other = TRUE)
  expect_true(prep$total > 0)
  expect_equal(sum(prep$data$value), prep$total)
  expect_true("OTHER" %in% prep$data$hs_code || prep$other_value == 0)
  empty <- prepare_commodity_treemap(data.table::data.table(), top_n = 5L)
  expect_equal(empty$total, 0)
})

test_that("commodity movers preserve missing endpoints", {
  det <- make_ts_detailed_fixture()
  m <- commodity_movers(det, 2019, 2022, reporters = "DEU", top_n = 5L)
  expect_equal(m$start_year, 2019L)
  expect_equal(m$end_year, 2022L)

  expect_true(nrow(m$absolute_increase) >= 1)

  expect_true(all(!is.na(m$absolute_increase$start_value)))
  expect_true(all(!is.na(m$absolute_increase$end_value)))
})

test_that("detailed KPI and download safety", {
  det <- prepare_detailed_trade(make_ts_detailed_fixture())
  k <- ts_detailed_kpis(det, list(represented_reporter_count = 6L, selected_reporter_count = 20L))
  expect_equal(k$coverage_label, "6/20")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write_ts_csv(det, tmp)
  got <- names(data.table::fread(tmp, nrows = 1))
  expect_false(any(grepl("raw_file|secret|path", got, ignore.case = TRUE)))
})
