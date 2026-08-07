test_that("filter choices use represented reporters and keep missing in coverage", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  uni <- make_trade_flow_universe()
  ch <- trade_flow_filter_choices(det, uni)
  expect_true(all(c("DEU", "IND", "KOR", "ITA") %in% ch$reporters))
  expect_false("CHN" %in% ch$reporters)
  expect_true("CHN" %in% ch$missing_reporters)
  expect_false("WLD" %in% ch$partners)
})

test_that("aggregate partners excluded during prepare", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  expect_false("WLD" %in% det$partner_iso3)
})

test_that("reporter/partner/year/flow/commodity filters and combinations", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  f <- filter_detailed_trade(det, year_min = 2023, year_max = 2023, reporters = "DEU",
                             partners = "CHN", flows = "M", hs_codes = "8517")
  expect_equal(nrow(f), 1L)
  expect_equal(f$trade_value_usd, 120)
  empty <- filter_detailed_trade(det, reporters = "ZZZ")
  expect_equal(nrow(empty), 0L)
  rng <- filter_detailed_trade(det, year_min = 2022, year_max = 2024, reporters = "DEU")
  expect_true(all(rng$year >= 2022 & rng$year <= 2024))
  expect_true(all(rng$reporter_iso3 == "DEU"))
})

test_that("KPI reconciliation and import/export separation", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  f <- filter_detailed_trade(det, year_min = 2023, year_max = 2023)
  k <- trade_flow_kpis(f)
  expect_equal(k$filtered_trade_value, sum(f$trade_value_usd))
  expect_equal(k$imports_value, sum(f[flow_code == "M"]$trade_value_usd))
  expect_equal(k$exports_value, sum(f[flow_code == "X"]$trade_value_usd))
  expect_equal(k$imports_value + k$exports_value, k$filtered_trade_value)
})

test_that("top-N deterministic ordering and stable tie-breaking", {
  paths <- data.table::data.table(
    reporter_iso3 = c("A", "B", "C"),
    partner_iso3 = c("X", "X", "X"),
    hs_code = c("1", "1", "1"),
    flow_code = c("M", "M", "M"),
    value = c(100, 100, 50),
    reporter_name = c("A", "B", "C"),
    partner_name = c("X", "X", "X"),
    commodity_description = c("d", "d", "d")
  )
  data.table::setorderv(paths, c("value", "reporter_iso3", "partner_iso3", "hs_code", "flow_code"),
                        order = c(-1L, 1L, 1L, 1L, 1L))
  sel <- select_top_n_paths(paths, top_n = 2L)
  expect_equal(sel$visible$reporter_iso3, c("A", "B"))
  expect_equal(sel$coverage_pct, 200 / 250 * 100)
})

test_that("Sankey node roles unique and links valid with coverage", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  f <- filter_detailed_trade(det, year_min = 2023, year_max = 2023, reporters = "DEU")
  paths <- trade_flow_path_aggregates(f, "reporter_partner_commodity")
  sel <- select_top_n_paths(paths, top_n = 10L)
  sk <- build_sankey_data(sel$visible, "reporter_partner_commodity", both_flows = TRUE)
  expect_true(nrow(sk$nodes) > 0)
  expect_true(all(sk$links$source >= 0 & sk$links$target >= 0))
  expect_true(all(sk$links$source < nrow(sk$nodes)))
  expect_true(all(sk$links$value >= 0))
  expect_false(any(duplicated(sk$nodes$id)))

  expect_true(any(grepl("^reporter_", sk$nodes$id)))
  expect_true(any(grepl("^partner_", sk$nodes$id)))
  expect_true(abs(sel$coverage_pct - 100) < 1e-8 || sel$n_visible <= sel$n_total)
})

test_that("Sankey grouping modes work", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  f <- filter_detailed_trade(det, reporters = "DEU")
  for (g in c("reporter_partner", "reporter_commodity",
              "reporter_partner_commodity", "reporter_commodity_partner")) {
    paths <- trade_flow_path_aggregates(f, g)
    sel <- select_top_n_paths(paths, top_n = 20L)
    sk <- build_sankey_data(sel$visible, g)
    expect_true(nrow(sk$links) >= 1)
    expect_equal(sum(sk$links$value >= 0), nrow(sk$links))
  }
})

test_that("zero-total coverage handling", {
  sel <- select_top_n_paths(data.table::data.table(), top_n = 5L)
  expect_true(is.na(sel$coverage_pct))
  expect_equal(sel$n_visible, 0L)
})

test_that("time-series aggregation, missing years, series limiting", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  f <- filter_detailed_trade(det, reporters = "DEU")
  ts <- prepare_trade_flow_timeseries(f, max_series = 2L, mode = "top_partners")
  expect_true(nrow(ts$data) > 0)
  expect_lte(data.table::uniqueN(ts$data$series), 2L)
  expect_false(any(is.infinite(ts$data$value)))
  expect_false(any(is.nan(ts$data$value)))
})

test_that("commodity composition and Other aggregation", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  f <- filter_detailed_trade(det)
  comp <- prepare_commodity_composition(f, top_n = 1L, include_other = TRUE)
  expect_true("OTHER" %in% comp$data$hs_code)
  expect_equal(sum(comp$data$value), comp$total)
})

test_that("matrix missing-versus-zero semantics", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  f <- filter_detailed_trade(det, year_min = 2023, year_max = 2023)
  mat <- prepare_reporter_partner_matrix(f, "trade_value")

  miss <- mat$long[observed == FALSE]
  if (nrow(miss)) expect_true(all(is.na(miss$value)))
  obs <- mat$long[observed == TRUE]
  expect_true(all(!is.na(obs$value)))
})

test_that("filename sanitisation and download safety", {
  fn <- trade_flow_filename("trade_flows", "2024", "DEU", "all partners!", "both")
  expect_false(grepl(" ", fn))
  expect_true(grepl("^trade_flows_2024_DEU_all_partners_both\\.csv$", fn))
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  tab <- trade_flow_table_display(det, technical = TRUE)
  expect_false(any(grepl("raw_file|path|secret", names(tab), ignore.case = TRUE)))
  expect_equal(nrow(tab), nrow(det))
})

test_that("partial and complete coverage status reading", {
  snap <- make_trade_flow_snap(FALSE)
  cov <- trade_flow_coverage_status(snap)
  expect_equal(cov$production_status, "partial")
  expect_gt(cov$missing_reporter_count, 0)
  snap2 <- make_trade_flow_snap(TRUE)
  cov2 <- trade_flow_coverage_status(snap2)
  expect_equal(cov2$production_status, "complete")
  expect_equal(cov2$missing_reporter_count, 0)
})

test_that("stale checksum warning flag", {
  snap <- make_trade_flow_snap(FALSE)
  snap$analytical_universe$universe_checksum <- "uv_other"
  cov <- trade_flow_coverage_status(snap, expected_checksum = "uv_262deb46e00d2f216a5a")
  expect_true(isTRUE(cov$checksum_stale))
})

test_that("Sankey visible path values reconcile to selection", {
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  f <- filter_detailed_trade(det, reporters = "DEU")
  paths <- trade_flow_path_aggregates(f, "reporter_partner")
  sel <- select_top_n_paths(paths, top_n = 2L)
  expect_equal(sum(sel$visible$value) + sel$other_value, sel$total_value)
})
