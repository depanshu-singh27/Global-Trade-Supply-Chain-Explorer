test_that("absent analytics yields empty reactive", {
  skip_if_not_installed("shiny")
  shiny::testServer(mod_trade_balance_map_server, args = list(
    snap = shiny::reactiveVal(list(
      map_analytics = NULL,
      country_year_analytics = NULL,
      map_geometry = NULL,
      pipeline_status = list(global_trade = "absent", detailed_trade = "partial", macro = "absent")
    )),
    cfg = shiny::reactive(list())
  ), {
    expect_null(analytics())
  })
})

test_that("selector updates selected country and clear works", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  cy <- prepare_map_analytics(make_map_analytics_fixture())
  g <- make_synthetic_map_geometry()
  shiny::testServer(mod_trade_balance_map_server, args = list(
    snap = shiny::reactiveVal(list(
      map_analytics = cy,
      country_year_analytics = cy,
      map_geometry = g,
      geographic_crosswalk = build_geographic_crosswalk(cy$reporter_iso3, g),
      pipeline_status = list(global_trade = "complete", detailed_trade = "partial", macro = "complete")
    )),
    cfg = shiny::reactive(list())
  ), {
    session$setInputs(year = "2024", metric = "trade_balance", classify = "continuous",
                      country = "DEU", region = "__ALL__")
    expect_equal(selected_iso(), "DEU")
    expect_true(all(year_dt()$year == 2024L))
    k <- map_kpi_summary(year_dt())
    expect_equal(k$total_trade, sum(year_dt()$total_trade_value_usd))
    session$setInputs(country = "")

    session$flushReact()
    expect_true(is.null(selected_iso()) || identical(selected_iso(), ""))
  })
})

test_that("global complete and detailed partial remain honest", {
  cov <- list(global_trade = "complete", detailed_trade = "partial")
  expect_equal(cov$global_trade, "complete")
  expect_equal(cov$detailed_trade, "partial")
})

test_that("no API call in map module sources", {
  files <- c(
    "map_calculations.R", "map_geometry.R", "map_formatters.R",
    "map_palettes.R", "map_downloads.R", "mod_trade_balance_map.R"
  )
  txt <- paste(vapply(files, function(f) {
    paste(readLines(file.path(TEST_ROOT, "R", f)), collapse = "\n")
  }, character(1)), collapse = "\n")
  expect_false(grepl("httr2::|comtrade_get|wdi_get|ne_download", txt))
})

test_that("snapshot can load map analytics row count when files exist", {
  skip_if_not_installed("arrow")
  path <- normalizePath(
    file.path(TEST_ROOT, "data", "processed", "country_year_analytics.parquet"),
    winslash = "/", mustWork = FALSE
  )
  skip_if_not(file.exists(path), "analytics parquet absent")
  raw <- data.table::as.data.table(arrow::read_parquet(path))
  prepared_n <- nrow(prepare_map_analytics(raw))
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(TEST_ROOT)
  snap <- load_processed_snapshot(load_config())
  expect_equal(nrow(snap$map_analytics), prepared_n)
  expect_equal(nrow(snap$country_year_analytics), nrow(raw))
  expect_false(any(snap$map_analytics$reporter_iso3 %in% AGGREGATE_ISO3_MAP))
})
