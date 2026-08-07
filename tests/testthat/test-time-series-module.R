test_that("absent global-data state", {
  skip_if_not_installed("shiny")
  shiny::testServer(mod_time_series_server, args = list(
    snap = shiny::reactiveVal(list(
      map_analytics = NULL, country_year_analytics = NULL,
      trade_detailed_enriched = NULL,
      detailed_coverage = list(production_status = "partial",
                               represented_reporter_count = 0L, selected_reporter_count = 20L,
                               represented_reporters = character(), missing_reporters = character()),
      pipeline_status = list(global_trade = "absent", detailed_trade = "partial")
    )),
    cfg = shiny::reactive(list())
  ), {
    expect_null(cy_data())
  })
})

test_that("scope switching and filters update series", {
  skip_if_not_installed("shiny")
  cy <- prepare_ts_global(make_ts_global_fixture())
  det <- prepare_detailed_trade(make_ts_detailed_fixture())
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
        production_status = "partial",
        represented_reporter_count = 2L,
        selected_reporter_count = 20L,
        represented_reporters = c("DEU", "IND"),
        missing_reporters = c("CHN", "USA"),
        universe_checksum = "uv_262deb46e00d2f216a5a",
        checksum_stale = FALSE,
        validation_warnings = 1L,
        latest_ingested_at = "t"
      ),
      pipeline_status = list(global_trade = "complete", detailed_trade = "partial", macro = "complete")
    )),
    cfg = shiny::reactive(list())
  ), {
    session$setInputs(
      scope = "single", year_min = "2019", year_max = "2022",
      economy = "DEU", compare = c("DEU", "USA"), metric = "total_trade",
      transform = "absolute", flow = "both", partner = "__ALL__", hs4 = "__ALL__",
      top_n = "10", det_reporter = "DEU", table_technical = FALSE
    )
    ms <- main_series()
    expect_true(all(ms$reporter_iso3 == "DEU"))
    k <- ts_kpi_single_or_global(ms, "total_trade")
    expect_equal(k$latest_year, 2022L)

    session$setInputs(scope = "global")
    gs <- main_series()
    expect_true(any(gs$series == "Global aggregate"))

    session$setInputs(scope = "detailed")
    expect_gt(nrow(detailed_filtered()), 0)

    expect_equal(coverage()$production_status, "partial")
    expect_equal(coverage()$represented_reporter_count, 2L)
  })
})

test_that("future complete status reading", {
  cov <- trade_flow_coverage_status(list(
    trade_detailed_enriched = prepare_detailed_trade(make_ts_detailed_fixture()),
    analytical_universe = list(
      top_reporters = data.frame(reporter_iso3 = c("DEU", "IND")),
      universe_checksum = "uv_262deb46e00d2f216a5a"
    ),
    production_manifest = list(production_status = "complete", selected_reporter_count = 2L,
                               represented_reporter_count = 2L, universe_version = "uv_262deb46e00d2f216a5a"),
    production_validation = NULL, phase3_validation = NULL,
    pipeline_status = list(detailed_trade = "complete")
  ))
  expect_equal(cov$production_status, "complete")
  expect_equal(cov$missing_reporter_count, 0)
})

test_that("stale checksum warning", {
  cov <- trade_flow_coverage_status(list(
    trade_detailed_enriched = prepare_detailed_trade(make_ts_detailed_fixture()),
    analytical_universe = list(
      top_reporters = data.frame(reporter_iso3 = c("DEU", "IND")),
      universe_checksum = "uv_other"
    ),
    production_manifest = list(production_status = "partial"),
    production_validation = NULL, phase3_validation = NULL
  ), expected_checksum = "uv_262deb46e00d2f216a5a")
  expect_true(isTRUE(cov$checksum_stale))
})

test_that("no API calls in time-series sources", {
  files <- c(
    "time_series_calculations.R", "time_series_formatters.R", "time_series_charts.R",
    "time_series_downloads.R", "commodity_analysis.R", "mod_time_series.R"
  )
  txt <- paste(vapply(files, function(f) paste(readLines(file.path(TEST_ROOT, "R", f)), collapse = "\n"),
                      character(1)), collapse = "\n")
  expect_false(grepl("httr2::|comtrade_get|wdi_get|fetch_", txt))
})

test_that("source row counts preserved for global analytics", {
  skip_if_not_installed("arrow")
  path <- file.path(TEST_ROOT, "data", "processed", "country_year_analytics.parquet")
  skip_if_not(file.exists(path), "analytics absent")
  n_disk <- nrow(arrow::read_parquet(path))
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(TEST_ROOT)
  snap <- load_processed_snapshot(load_config())
  expect_equal(nrow(snap$country_year_analytics), n_disk)
})
