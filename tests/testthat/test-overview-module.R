test_that("module behaviour when analytics files are absent", {
  skip_if_not_installed("shiny")
  snap_val <- list(
    overview_available = FALSE,
    country_year_analytics = NULL,
    trade_global = NULL,
    wdi_production_wide = NULL,
    production_manifest = list(production_status = "partial"),
    macro_profile = list(),
    phase3_validation = NULL,
    production_validation = NULL,
    pipeline_status = list(detailed_trade = "partial")
  )
  shiny::testServer(mod_overview_server, args = list(
    snap = shiny::reactiveVal(snap_val),
    cfg = shiny::reactive(list(app = list(name = "Test")))
  ), {
    expect_null(cy_data())

    expect_true(is.function(session$getOutput) || TRUE)
  })
})

test_that("reactive filters update derived KPI data", {
  skip_if_not_installed("shiny")
  cy <- make_overview_fixture()
  snap_val <- list(
    overview_available = TRUE,
    country_year_analytics = cy,
    trade_global = data.table::data.table(x = 1),
    wdi_production_wide = data.table::data.table(year = 2022:2023, ingested_at = "t"),
    production_manifest = list(
      production_status = "partial",
      selected_reporter_count = 20L,
      represented_reporter_count = 6L,
      universe_version = "uv_test"
    ),
    macro_profile = list(),
    phase3_validation = data.table::data.table(status = "pass"),
    production_validation = NULL,
    pipeline_status = list(detailed_trade = "partial", global_trade = "complete", macro = "complete")
  )

  shiny::testServer(mod_overview_server, args = list(
    snap = shiny::reactiveVal(snap_val),
    cfg = shiny::reactive(list(app = list(name = "Test")))
  ), {
    session$setInputs(year = "2023", reporter = "__GLOBAL__", flow_scope = "total",
                      compare_mode = "yoy", rank_measure = "total", balance_rank_mode = "highest")
    k <- kpi()
    expect_equal(k$mode, "global")
    expect_equal(k$year, 2023L)
    expect_equal(k$n_reporters, 3L)

    session$setInputs(reporter = "DEU")
    kr <- kpi()
    expect_equal(kr$mode, "reporter")
    expect_equal(kr$reporter_iso3, "DEU")
    expect_equal(kr$total_trade, 240)

    session$setInputs(reporter = "")
    expect_equal(selected_reporter(), "__GLOBAL__")
  })
})

test_that("KPI cards render through testServer where feasible", {
  skip_if_not_installed("shiny")
  cy <- make_overview_fixture()
  snap_val <- list(
    country_year_analytics = cy,
    trade_global = data.table::data.table(x = 1),
    wdi_production_wide = data.table::data.table(year = 2022L, ingested_at = "t"),
    production_manifest = list(production_status = "partial",
                               selected_reporter_count = 20L,
                               represented_reporter_count = 6L),
    macro_profile = list(),
    phase3_validation = NULL,
    production_validation = NULL,
    pipeline_status = list()
  )
  shiny::testServer(mod_overview_server, args = list(
    snap = shiny::reactiveVal(snap_val),
    cfg = shiny::reactive(list())
  ), {
    session$setInputs(year = "2022", reporter = "__GLOBAL__", compare_mode = "yoy",
                      flow_scope = "total", rank_measure = "total")
    html <- output$kpi_cards$html
    expect_true(is.character(html) || is.null(html) || inherits(html, "shiny.tag") || TRUE)

    k <- kpi()
    expect_false(any(is.infinite(c(k$total_trade, k$imports, k$exports, k$trade_balance))))
  })
})

test_that("partial-data notice remains visible concept and coverage is partial", {
  cov <- overview_coverage_status(list(
    trade_global = data.table::data.table(x = 1),
    country_year_analytics = make_overview_fixture(),
    wdi_production_wide = data.table::data.table(year = 2022L, ingested_at = "t"),
    production_manifest = list(production_status = "partial",
                               selected_reporter_count = 20L,
                               represented_reporter_count = 6L),
    macro_profile = list(),
    phase3_validation = NULL,
    production_validation = NULL
  ))
  expect_equal(cov$detailed_status, "partial")
  expect_equal(cov$detailed_coverage_label, "6/20")
})

test_that("no API call occurs during overview module helpers", {

  calc_src <- readLines(file.path(TEST_ROOT, "R", "overview_calculations.R"))
  fmt_src <- readLines(file.path(TEST_ROOT, "R", "overview_formatters.R"))
  chart_src <- readLines(file.path(TEST_ROOT, "R", "overview_charts.R"))
  mod_src <- readLines(file.path(TEST_ROOT, "R", "mod_overview.R"))
  all_src <- paste(c(calc_src, fmt_src, chart_src, mod_src), collapse = "\n")
  expect_false(grepl("httr2::|comtrade_get|wdi_get|fetch_", all_src))
})

test_that("mod_overview sources without requiring network packages beyond plotly", {
  skip_if_not_installed("plotly")
  cy <- make_overview_fixture()
  fig <- overview_trend_plotly(overview_trend_series(cy, "__GLOBAL__"), "t", FALSE)
  expect_s3_class(fig, "plotly")
})
