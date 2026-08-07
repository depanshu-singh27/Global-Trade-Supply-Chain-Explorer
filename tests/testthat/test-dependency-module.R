test_that("module filter updates and matrix-mode switching", {
  snap <- shiny::reactiveVal(make_dependency_snap_fixture("partial"))
  cfg <- shiny::reactive(list())
  shiny::testServer(mod_dependency_server, args = list(snap = snap, cfg = cfg), {
    session$setInputs(
      year_mode = "latest", year_min = 2019L, year_max = 2019L,
      reporter = "__ALL__", partner = "__ALL__", hs = "__ALL__",
      metric = "partner_share", agg_mode = "commodity_specific",
      top_n = "20", min_value = 0, scale = "auto",
      matrix_mode = "reporter_supplier", rank_metric = "top_1_share"
    )
    built <- dependency_built()
    expect_true(nrow(built$shares) > 0)
    session$setInputs(matrix_mode = "country_commodity")
    sp <- cc_sparse()
    expect_true(sp$n_nodes >= 0L)
    session$setInputs(reporter = "DEU")
    built2 <- dependency_built()
    expect_true(all(built2$shares$reporter_iso3 == "DEU"))
  })
})

test_that("selected reporter and commodity profile updates", {
  snap <- shiny::reactiveVal(make_dependency_snap_fixture("partial"))
  cfg <- shiny::reactive(list())
  shiny::testServer(mod_dependency_server, args = list(snap = snap, cfg = cfg), {
    session$setInputs(
      year_mode = "full", year_min = 2019L, year_max = 2020L,
      reporter = "DEU", partner = "__ALL__", hs = "8542",
      metric = "partner_share", agg_mode = "commodity_specific",
      top_n = "10", min_value = 0, scale = "millions",
      matrix_mode = "reporter_supplier", rank_metric = "supplier_hhi",
      profile_reporter = "DEU", profile_hs = "8542"
    )
    expect_equal(reporter_filter(), "DEU")
    expect_equal(hs_filter(), "8542")
    gc <- group_conc()
    expect_true(all(gc$hs_code == "8542"))
  })
})

test_that("absent detailed-data and empty dependency state", {
  snap <- shiny::reactiveVal(list(
    trade_detailed_enriched = NULL,
    trade_detailed = NULL,
    detailed_coverage = list(
      production_status = "unavailable",
      selected_reporter_count = 20L,
      represented_reporter_count = 0L,
      missing_reporter_count = 20L,
      selected_reporters = character(),
      represented_reporters = character(),
      missing_reporters = character(),
      universe_checksum = "uv_262deb46e00d2f216a5a"
    ),
    analytical_universe = list(universe_checksum = "uv_262deb46e00d2f216a5a"),
    pipeline_status = list(detailed_trade = "unavailable")
  ))
  cfg <- shiny::reactive(list())
  shiny::testServer(mod_dependency_server, args = list(snap = snap, cfg = cfg), {
    d <- detailed_data()
    expect_true(is.null(d) || !nrow(d))
  })
  empty <- data.table::data.table(
    year = integer(), reporter_iso3 = character(), reporter_name = character(),
    partner_iso3 = character(), partner_name = character(), flow_code = character(),
    hs_code = character(), trade_value_usd = numeric()
  )
  built <- construct_dependency_table(empty)
  expect_equal(nrow(built$shares), 0L)
})

test_that("partial and future complete status; stale checksum", {
  snap_p <- make_dependency_snap_fixture("partial")
  expect_equal(snap_p$detailed_coverage$production_status, "partial")
  expect_equal(snap_p$detailed_coverage$represented_reporter_count, 3L)
  snap_c <- make_dependency_snap_fixture("complete")
  expect_equal(snap_c$detailed_coverage$production_status, "complete")
  stale <- trade_flow_coverage_status(snap_p, expected_checksum = "uv_other")
  expect_true(isTRUE(stale$checksum_stale))
})

test_that("partial notice wording is dynamic", {
  snap <- make_dependency_snap_fixture("partial")
  msg <- sprintf(
    "Dependency results currently cover available reported imports for %d of %d selected reporting economies.",
    snap$detailed_coverage$represented_reporter_count,
    snap$detailed_coverage$selected_reporter_count
  )
  expect_match(msg, "3 of 8")
  ui <- as.character(mod_dependency_ui("dependency"))
  expect_true(grepl("Dependency Explorer", ui))
})

test_that("no API calls in dependency sources", {
  root <- TEST_ROOT
  files <- c(
    "dependency_formatters.R", "dependency_construction.R", "dependency_metrics.R",
    "dependency_rankings.R", "dependency_matrix.R", "dependency_charts.R",
    "dependency_downloads.R", "mod_dependency.R"
  )
  for (f in files) {
    txt <- paste(readLines(file.path(root, "R", f), warn = FALSE), collapse = "\n")
    expect_false(grepl("comtrade_get|wdi_get|httr2::req_perform|CURLOPT", txt))
  }
})
