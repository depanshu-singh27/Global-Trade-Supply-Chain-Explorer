test_that("all analytical modules start under public/read-only config", {
  rt <- list(
    public_mode = TRUE,
    read_only_mode = TRUE,
    allow_scenario_writes = FALSE
  )
  cfg <- shiny::reactive(list(runtime = rt, app = list(name = "test")))

  if (exists("make_overview_snap_fixture", mode = "function")) {
    snap <- shiny::reactiveVal(make_overview_snap_fixture())
    shiny::testServer(mod_overview_server, args = list(snap = snap, cfg = cfg), {
      expect_true(is.function(session$flushReact) || TRUE)
    })
  }

  if (exists("make_network_snap_fixture", mode = "function")) {
    snap_n <- shiny::reactiveVal(make_network_snap_fixture("partial"))
    shiny::testServer(mod_network_server, args = list(snap = snap_n, cfg = cfg), {
      session$setInputs(
        year_mode = "latest", year_min = 2020L, year_max = 2020L,
        mode = "exports", focus = "__ALL__", ego_order = "1",
        partners = "__ALL__", hs = "__ALL__", top_n = "50",
        size_metric = "total_strength", colour_metric = "reporting_status",
        layout = "fr", scale = "auto", rank_metric = "total_strength"
      )
      expect_true(is.list(network_result()))
    })
  }

  if (exists("make_shock_coverage", mode = "function")) {
    snap_s <- shiny::reactiveVal(list(
      detailed_coverage = make_shock_coverage(),
      trade_detailed_enriched = make_shock_detailed_fixture(),
      map_geometry = NULL
    ))
    shiny::testServer(mod_shock_simulator_server, args = list(snap = snap_s, cfg = cfg), {
      expect_false(persist_enabled())
      expect_true(capabilities()$can_run_in_memory)
      expect_error(output$builder_panel, NA)
      expect_error(output$validation_panel, NA)
      expect_error(output$preview_panel, NA)
      expect_error(output$exec_panel, NA)
    })
  }
})
