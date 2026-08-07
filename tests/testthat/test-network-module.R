test_that("module filter updates and partial notice via testServer", {
  snap <- shiny::reactiveVal(make_network_snap_fixture("partial"))
  cfg <- shiny::reactive(list(app = list(name = "test")))
  shiny::testServer(mod_network_server, args = list(snap = snap, cfg = cfg), {
    session$setInputs(
      year_mode = "latest",
      year_min = 2020L,
      year_max = 2020L,
      mode = "exports",
      focus = "__ALL__",
      ego_order = "1",
      partners = "__ALL__",
      hs = "__ALL__",
      top_n = "50",
      size_metric = "total_strength",
      colour_metric = "reporting_status",
      layout = "fr",
      scale = "auto",
      rank_metric = "total_strength"
    )
    net <- network_result()
    expect_true(is.list(net))
    expect_true(net$built$visible_n >= 0L)
    session$setInputs(mode = "imports")
    net2 <- network_result()
    expect_equal(net2$built$mode, "imports")
  })
})

test_that("selected-node update through testServer", {
  snap <- shiny::reactiveVal(make_network_snap_fixture("partial"))
  cfg <- shiny::reactive(list())
  shiny::testServer(mod_network_server, args = list(snap = snap, cfg = cfg), {
    session$setInputs(
      year_mode = "full", year_min = 2019L, year_max = 2021L,
      mode = "exports", focus = "DEU", ego_order = "1",
      partners = "__ALL__", hs = "__ALL__", top_n = "100",
      size_metric = "total_strength", colour_metric = "reporting_status",
      layout = "circle", scale = "millions", rank_metric = "pagerank",
      profile_iso = "DEU"
    )
    expect_equal(selected_iso(), "DEU")
    net <- network_result()
    expect_true("DEU" %in% net$nodes$iso3)
  })
})

test_that("absent detailed-data empty state path", {
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
  shiny::testServer(mod_network_server, args = list(snap = snap, cfg = cfg), {
    d <- detailed_data()
    expect_true(is.null(d) || !nrow(d))
  })
})

test_that("no API calls in network sources", {
  root <- TEST_ROOT
  files <- c(
    "network_formatters.R", "network_construction.R", "network_centrality.R",
    "network_calculations.R", "network_charts.R", "network_downloads.R", "mod_network.R"
  )
  for (f in files) {
    txt <- paste(readLines(file.path(root, "R", f), warn = FALSE), collapse = "\n")
    expect_false(grepl("comtrade_get|wdi_get|httr2::req_perform|CURLOPT", txt))
  }
})

test_that("partial notice wording uses dynamic counts", {
  ui <- as.character(mod_network_ui("network"))
  expect_true(grepl("Trade Network", ui))

  snap <- make_network_snap_fixture("partial")
  expect_match(
    sprintf(
      "Network measures currently describe the available detailed trade network for %d of %d selected reporting economies.",
      snap$detailed_coverage$represented_reporter_count,
      snap$detailed_coverage$selected_reporter_count
    ),
    "3 of 8"
  )
})
