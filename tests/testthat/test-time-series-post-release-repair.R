make_movers_duplicate_desc_fixture <- function() {
  data.table::data.table(
    year = c(2019L, 2019L, 2024L, 2024L, 2019L, 2024L, 2019L, 2024L, 2019L, 2024L),
    reporter_iso3 = rep("DEU", 10),
    reporter_name = "Germany",
    partner_iso3 = c("CHN", "USA", "CHN", "USA", "CHN", "CHN", "USA", "USA", "JPN", "JPN"),
    partner_name = c("China", "USA", "China", "USA", "China", "China", "USA", "USA", "Japan", "Japan"),
    flow_code = c("M", "M", "M", "M", "X", "X", "M", "M", "M", "M"),
    flow_name = c("Import", "Import", "Import", "Import", "Export", "Export", "Import", "Import", "Import", "Import"),
    hs_code = c("8542", "8542", "8542", "8542", "8517", "8517", "8507", "8507", "8501", "8501"),

    commodity_description = c(
      "Electronic integrated circuits",
      "Electronic integrated circuits — other",
      "Electronic integrated circuits",
      "Electronic integrated circuits — other",
      "Telephone sets truncated label collision",
      "Telephone sets truncated label collision",
      "Electric accumulators",
      "Electric accumulators",
      "Electric motors and generators",
      "Electric motors and generators"
    ),
    trade_value_usd = c(100, 50, 200, 80, 40, 10, 90, 30, 25, 25),
    ingested_at = "2024-01-01T00:00:00Z",
    universe_checksum = "uv_262deb46e00d2f216a5a",
    production_status = "complete"
  )
}

test_that("zero-length primary economy does not throw data.table RHS errors", {
  cy <- prepare_ts_global(make_ts_global_fixture())
  expect_equal(normalize_primary_economy(character(0)), NA_character_)
  expect_equal(normalize_primary_economy(NULL), NA_character_)
  expect_equal(normalize_primary_economy(""), NA_character_)
  expect_equal(normalize_primary_economy("CHN"), "CHN")

  raw_err <- tryCatch({
    cy[reporter_iso3 == character(0)]
    ""
  }, error = function(e) conditionMessage(e))
  expect_match(raw_err, "RHS of == is length 0")

  expect_error(
    economy_metric_series(cy, character(0), "total_trade", 2019L, 2022L),
    NA
  )
  empty <- economy_metric_series(cy, character(0), "total_trade", 2019L, 2022L)
  expect_equal(nrow(empty), 0L)

  invalid <- economy_metric_series(cy, "ZZZ", "total_trade", 2019L, 2022L)
  expect_equal(nrow(invalid), 0L)

  expect_error(
    import_export_decomposition(cy, "single", character(0), 2019L, 2022L),
    NA
  )
})

test_that("valid scalar primary economy renders single-economy series", {
  cy <- prepare_ts_global(make_ts_global_fixture())
  for (iso in c("CHN", "USA", "DEU", "IND")) {
    s <- economy_metric_series(cy, iso, "total_trade", 2019L, 2022L)
    expect_true(nrow(s) >= 1L, info = iso)
    expect_true(all(s$reporter_iso3 == iso), info = iso)
    ix <- apply_series_transform(s, "total_trade", "index")
    expect_true(nrow(ix) >= 1L, info = iso)
    expect_true(all(is.finite(ix$display_value) | is.na(ix$display_value)), info = iso)
  }
})

test_that("commodity movers aggregate duplicate descriptions and partners to one HS4", {
  d <- make_movers_duplicate_desc_fixture()
  mv <- commodity_movers(d, 2019L, 2024L, top_n = 10L)
  expect_equal(anyDuplicated(mv$absolute_increase$hs_code), 0L)
  expect_equal(anyDuplicated(mv$absolute_decrease$hs_code), 0L)
  expect_true(is.character(mv$absolute_increase$hs_code) || nrow(mv$absolute_increase) == 0L)
  if (nrow(mv$absolute_increase)) {
    expect_true(all(mv$absolute_increase$abs_change > 0))
    expect_equal(anyDuplicated(mv$absolute_increase$commodity_key), 0L)
    expect_error(
      ts_movers_plotly(mv$absolute_increase, "abs_change", "up", "#0a0"),
      NA
    )
  }
  if (nrow(mv$absolute_decrease)) {
    expect_true(all(mv$absolute_decrease$abs_change < 0))
    expect_equal(anyDuplicated(mv$absolute_decrease$commodity_key), 0L)
    expect_error(
      ts_movers_plotly(mv$absolute_decrease, "abs_change", "down", "#a00"),
      NA
    )
  }

  row_8542 <- rbind(mv$absolute_increase, mv$absolute_decrease, fill = TRUE)[hs_code == "8542"]
  expect_equal(nrow(row_8542), 1L)
  expect_equal(row_8542$abs_change, 130)
  expect_equal(row_8542$start_value, 150)
  expect_equal(row_8542$end_value, 280)
})

test_that("commodity movers deterministic ranking, ties, top-N, empty sides", {
  d <- make_movers_duplicate_desc_fixture()

  d_neg <- data.table::copy(d)
  d_neg[hs_code == "8542" & year == 2024L, trade_value_usd := 10]
  d_neg[hs_code == "8517" & year == 2024L, trade_value_usd := 1]
  mv_neg <- commodity_movers(d_neg, 2019L, 2024L, top_n = 5L)
  expect_equal(nrow(mv_neg$absolute_increase), 0L)
  expect_match(
    commodity_movers_empty_message(mv_neg, detailed = d_neg, start_year = 2019L, end_year = 2024L),
    "No common HS4|No "
  )
  expect_gt(nrow(mv_neg$absolute_decrease), 0L)

  d_pos <- data.table::copy(d)
  d_pos[year == 2024L, trade_value_usd := trade_value_usd + 1000]
  mv_pos <- commodity_movers(d_pos, 2019L, 2024L, top_n = 2L)
  expect_lte(nrow(mv_pos$absolute_increase), 2L)
  expect_true(all(mv_pos$absolute_increase$abs_change > 0))

  tie <- data.table::data.table(
    year = c(2019L, 2024L, 2019L, 2024L),
    reporter_iso3 = "DEU",
    reporter_name = "Germany",
    partner_iso3 = "CHN",
    partner_name = "China",
    flow_code = "M",
    flow_name = "Import",
    hs_code = c("8501", "8501", "8502", "8502"),
    commodity_description = c("A", "A", "B", "B"),
    trade_value_usd = c(10, 20, 10, 20)
  )
  mv_tie <- commodity_movers(tie, 2019L, 2024L, top_n = 2L)
  expect_equal(mv_tie$absolute_increase$hs_code, c("8501", "8502"))

  mv_m <- commodity_movers(d, 2019L, 2024L, flows = "M", top_n = 10L)
  expect_true(nrow(mv_m$absolute_increase) + nrow(mv_m$absolute_decrease) >= 1L)
  mv_x <- commodity_movers(d, 2019L, 2024L, flows = "X", top_n = 10L)
  expect_true(is.list(mv_x))

  d_miss <- data.table::copy(d)
  d_miss <- d_miss[!(hs_code == "8501" & year == 2024L)]
  mv_miss <- commodity_movers(d_miss, 2019L, 2024L)
  expect_false("8501" %in% c(mv_miss$absolute_increase$hs_code, mv_miss$absolute_decrease$hs_code))
})

test_that("20/20 coverage message is dynamic and drops remaining-request wording", {
  complete <- list(
    production_status = "complete",
    represented_reporter_count = 20L,
    selected_reporter_count = 20L,
    missing_reporter_count = 0L,
    request_summary = list(planned = 0L, active = 0L, quota_blocked = 0L)
  )
  partial <- list(
    production_status = "partial",
    represented_reporter_count = 6L,
    selected_reporter_count = 20L,
    missing_reporter_count = 14L,
    request_summary = list(planned = 10L, active = 0L, quota_blocked = 5L)
  )
  expect_true(coverage_is_selected_universe_complete(complete))
  expect_false(coverage_is_selected_universe_complete(partial))
  msg_c <- detailed_coverage_notice(complete, "time_series")
  msg_p <- detailed_coverage_notice(partial, "time_series")
  expect_match(msg_c, "complete for the selected 20-reporter")
  expect_match(msg_c, "not complete bilateral trade coverage")
  expect_false(grepl("remaining .*Comtrade requests", msg_c, ignore.case = TRUE))
  expect_false(grepl("partial detailed observations", msg_c, ignore.case = TRUE))
  expect_match(msg_p, "6 of 20")
  expect_match(msg_p, "remaining selected-universe Comtrade")

  expect_match(detailed_coverage_notice(complete, "bilateral"), "complete for the selected 20")
  expect_match(detailed_coverage_notice(complete, "network"), "completed selected-universe")
  expect_match(detailed_coverage_notice(complete, "dependency"), "completed selected-universe")
  expect_match(detailed_coverage_notice(complete, "shock"), "completed selected-universe")
  expect_match(detailed_coverage_notice(complete, "forecast"), "completed selected-universe")
  ft <- paste(readLines(file.path(TEST_ROOT, "R/forecast_formatters.R")), collapse = "\n")
  expect_true(grepl("fixture_synthetic|production_forecast_available", ft))
})

test_that("fresh-session time-series module tolerates unset economy inputs", {
  skip_if_not_installed("shiny")
  cy <- prepare_ts_global(make_ts_global_fixture())
  det <- prepare_detailed_trade(make_movers_duplicate_desc_fixture())
  shiny::testServer(mod_time_series_server, args = list(
    snap = shiny::reactiveVal(list(
      map_analytics = cy,
      country_year_analytics = cy,
      trade_detailed_enriched = det,
      analytical_universe = list(
        top_reporters = data.frame(reporter_iso3 = c("DEU", "IND", "CHN", "USA")),
        universe_checksum = "uv_262deb46e00d2f216a5a"
      ),
      detailed_coverage = list(
        production_status = "complete",
        represented_reporter_count = 20L,
        selected_reporter_count = 20L,
        missing_reporter_count = 0L,
        represented_reporters = c("DEU", "IND", "CHN", "USA"),
        missing_reporters = character(),
        universe_checksum = "uv_262deb46e00d2f216a5a",
        checksum_stale = FALSE,
        validation_warnings = 0L,
        latest_ingested_at = "t",
        request_summary = list(planned = 0L, active = 0L, quota_blocked = 0L)
      ),
      pipeline_status = list(global_trade = "complete", detailed_trade = "complete", macro = "complete")
    )),
    cfg = shiny::reactive(list())
  ), {

    expect_error(output$main_chart, NA)
    expect_error(output$growth_chart, NA)

    session$setInputs(
      scope = "single", year_min = "2019", year_max = "2022",
      economy = "CHN", compare = c("CHN", "USA"), metric = "total_trade",
      transform = "absolute", flow = "both", partner = "__ALL__", hs4 = "__ALL__",
      top_n = "10", det_reporter = "DEU", table_technical = FALSE
    )
    ms <- main_series()
    expect_true(nrow(ms) >= 1L)
    expect_true(all(ms$reporter_iso3 == "CHN"))
    expect_error(output$growth_chart, NA)

    session$setInputs(scope = "global")
    expect_true(any(main_series()$series == "Global aggregate"))

    session$setInputs(scope = "compare", compare = c("CHN", "USA", "DEU"))
    expect_gte(data.table::uniqueN(compare_series()$reporter_iso3), 2L)

    session$setInputs(scope = "detailed", det_reporter = "DEU", year_min = "2019", year_max = "2024")
    mv <- movers()
    expect_equal(anyDuplicated(mv$absolute_increase$hs_code), 0L)
    expect_equal(anyDuplicated(mv$absolute_decrease$hs_code), 0L)
    expect_error(output$movers_up, NA)
    expect_error(output$movers_down, NA)

    notice <- detailed_coverage_notice(coverage(), "time_series")
    expect_match(notice, "complete for the selected 20-reporter")
    expect_false(grepl("remaining .*Comtrade requests are completed", notice))
  })
})

test_that("time-series repair sources make no external API calls", {
  files <- c(
    "time_series_calculations.R", "time_series_formatters.R", "time_series_charts.R",
    "commodity_analysis.R", "mod_time_series.R", "trade_flow_calculations.R"
  )
  txt <- paste(vapply(files, function(f) {
    paste(readLines(file.path(TEST_ROOT, "R", f)), collapse = "\n")
  }, character(1)), collapse = "\n")
  expect_false(grepl("httr2::request|comtrade_get\\(|wdi_get\\(", txt))
})
