test_that("absent detailed-file state yields null detailed reactive", {
  skip_if_not_installed("shiny")
  shiny::testServer(mod_trade_flows_server, args = list(
    snap = shiny::reactiveVal(list(
      trade_detailed_enriched = NULL,
      analytical_universe = list(),
      production_manifest = list(production_status = "absent"),
      detailed_coverage = list(
        production_status = "unavailable",
        selected_reporter_count = 20L,
        represented_reporter_count = 0L,
        missing_reporter_count = 20L,
        represented_reporters = character(),
        missing_reporters = character(),
        universe_checksum = NA_character_,
        checksum_stale = FALSE
      )
    )),
    cfg = shiny::reactive(list())
  ), {
    expect_null(detailed())
  })
})

test_that("module filters update derived data and partial notice concept", {
  skip_if_not_installed("shiny")
  snap <- make_trade_flow_snap(FALSE)
  snap$detailed_coverage <- trade_flow_coverage_status(snap)
  shiny::testServer(mod_trade_flows_server, args = list(
    snap = shiny::reactiveVal(snap),
    cfg = shiny::reactive(list())
  ), {
    session$setInputs(
      year_mode = "single", year = "2023", year_min = "2022", year_max = "2024",
      reporter = "DEU", partner = "__ALL__", flow = "both", hs4 = "__ALL__",
      top_n = "20", grouping = "reporter_partner", scale = "auto",
      matrix_measure = "trade_value", table_technical = FALSE
    )
    f <- filtered()
    expect_true(all(f$reporter_iso3 == "DEU"))
    expect_true(all(f$year == 2023L))
    k <- kpis()
    expect_equal(k$filtered_trade_value, sum(f$trade_value_usd))
    expect_gt(coverage()$missing_reporter_count, 0)
  })
})

test_that("no API call during trade flow module sources", {
  files <- c(
    "trade_flow_calculations.R", "trade_flow_formatters.R",
    "trade_flow_charts.R", "trade_flow_downloads.R", "mod_trade_flows.R"
  )
  txt <- paste(vapply(files, function(f) {
    paste(readLines(file.path(TEST_ROOT, "R", f)), collapse = "\n")
  }, character(1)), collapse = "\n")
  expect_false(grepl("httr2::|comtrade_get|wdi_get|fetch_", txt))
})

test_that("data access preserves detailed row count when files exist", {
  skip_if_not_installed("arrow")
  cfg <- tryCatch(load_config(), error = function(e) NULL)
  skip_if(is.null(cfg), "config unavailable")

  path <- normalizePath(
    file.path(TEST_ROOT, "data", "processed", "trade_detailed_enriched.parquet"),
    winslash = "/", mustWork = FALSE
  )
  skip_if_not(file.exists(path), "detailed enriched parquet absent")
  n_disk <- nrow(arrow::read_parquet(path))

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(TEST_ROOT)
  snap <- load_processed_snapshot(load_config())
  expect_equal(nrow(snap$trade_detailed_enriched), n_disk)
  expect_false(any(snap$trade_detailed_enriched$partner_iso3 %in% c("WLD", "W00"), na.rm = TRUE))
})
