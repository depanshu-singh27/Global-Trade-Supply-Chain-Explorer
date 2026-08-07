test_that("shock capabilities keep analysis on in read-only mode", {
  rt <- list(allow_scenario_writes = FALSE, read_only_mode = TRUE, public_mode = TRUE)
  caps <- shock_ui_capabilities(rt, history_n = 3L)
  expect_true(caps$can_view)
  expect_true(caps$can_validate)
  expect_true(caps$can_preview)
  expect_true(caps$can_run_in_memory)
  expect_true(caps$can_load_history)
  expect_true(caps$can_download_safe_outputs)
  expect_false(caps$can_persist)
  expect_false(caps$can_delete)
  expect_true(grepl("in memory", caps$read_only_notice, ignore.case = TRUE))

  rt2 <- list(allow_scenario_writes = TRUE, read_only_mode = FALSE)
  expect_true(shock_ui_capabilities(rt2)$can_persist)
  rt3 <- list(allow_scenario_writes = TRUE, read_only_mode = TRUE)
  expect_false(shock_ui_capabilities(rt3)$can_persist)
})

test_that("module initialises and builder renders in read-only mode", {
  snap <- shiny::reactiveVal(list(
    detailed_coverage = make_shock_coverage(),
    trade_detailed_enriched = make_shock_detailed_fixture(),
    map_geometry = NULL
  ))
  cfg <- shiny::reactive(list(
    runtime = list(
      public_mode = TRUE,
      read_only_mode = TRUE,
      allow_scenario_writes = FALSE
    )
  ))
  shiny::testServer(mod_shock_simulator_server, args = list(snap = snap, cfg = cfg), {
    expect_false(persist_enabled())
    expect_true(capabilities()$can_run_in_memory)
    expect_true(is.character(output$builder_panel) || is.list(output$builder_panel) ||
                  !is.null(output$builder_panel))

    expect_error(output$validation_panel, NA)
    expect_error(output$preview_panel, NA)
    expect_error(output$exec_panel, NA)
    expect_false(isTRUE(validation()$ok))
  })
})

test_that("in-memory run works and writes no files in read-only mode", {
  snap <- shiny::reactiveVal(list(
    detailed_coverage = make_shock_coverage(),
    trade_detailed_enriched = make_shock_detailed_fixture(),
    map_geometry = NULL
  ))
  cfg <- shiny::reactive(list(
    runtime = list(
      public_mode = TRUE,
      read_only_mode = TRUE,
      allow_scenario_writes = FALSE
    )
  ))
  root <- find_project_root()
  before <- list.dirs(shock_scenario_dirs(root)$results, full.names = TRUE, recursive = FALSE)
  shiny::testServer(mod_shock_simulator_server, args = list(snap = snap, cfg = cfg), {
    session$setInputs(
      scenario_name = "readonly inmem",
      baseline_year_start = 2024L,
      baseline_year_end = 2024L,
      target_supplier_iso3 = "CHN",
      target_hs_codes = "8542",
      shock_type = "commodity_specific_supplier_reduction",
      shock_size_pct = 30,
      substitution_mode = "capacity_constrained",
      substitution_capacity_pct = 25,
      maximum_substitute_supplier_share = 100,
      enable_max_share = FALSE,
      propagation_mode = "direct_only",
      maximum_propagation_steps = 1L,
      propagation_decay = 1,
      minimum_propagated_value_usd = 0,
      minimum_dependency_share = 0,
      include_macro_normalisation = TRUE,
      acknowledge_partial_coverage = TRUE,
      rep_metric = "residual_unmet_value_usd",
      rep_top_n = 10,
      com_metric = "residual_unmet_value_usd",
      map_metric = "residual_unmet_value_usd"
    )
    expect_true(isTRUE(validation()$ok))
    expect_true(preview()$n_target_edges >= 1)
    session$setInputs(run_scenario = 1L)
    expect_true(isTRUE(rv$active_result$ok))
    expect_equal(rv$active_result_source, "run_in_memory")
    expect_false(isTRUE(rv$active_result$loaded_from_disk))
    k <- shock_prepare_kpis(rv$active_result)
    expect_true(k$available)
    expect_true(nrow(ranked_reporters()) >= 1)
  })
  after <- list.dirs(shock_scenario_dirs(root)$results, full.names = TRUE, recursive = FALSE)
  expect_equal(length(after), length(before))
})

test_that("history View Result hydrates active_result by scenario_id", {
  snap <- shiny::reactiveVal(list(
    detailed_coverage = make_shock_coverage(),
    trade_detailed_enriched = make_shock_detailed_fixture(),
    map_geometry = NULL
  ))
  cfg <- shiny::reactive(list(
    runtime = list(read_only_mode = TRUE, allow_scenario_writes = FALSE, public_mode = TRUE)
  ))
  shiny::testServer(mod_shock_simulator_server, args = list(snap = snap, cfg = cfg), {
    hist <- history()
    skip_if(nrow(hist) < 1L, "No persisted scenario history available")
    show <- history_display()

    i <- which(show$scenario_id == "example_chn_8542_30pct")[1]
    if (is.na(i)) i <- 1L
    session$setInputs(history_table_rows_selected = i)
    expect_true(!is.null(selected_history_dir()))
    session$setInputs(hist_view = 1L)
    expect_true(isTRUE(rv$active_result$ok))
    expect_equal(rv$active_result_source, "history")
    expect_true(nrow(rv$active_result$reporter_impacts) >= 1)
    expect_true(shock_prepare_kpis(rv$active_result)$available)
  })
})

test_that("delete is blocked in read-only mode", {
  snap <- shiny::reactiveVal(list(
    detailed_coverage = make_shock_coverage(),
    trade_detailed_enriched = make_shock_detailed_fixture(),
    map_geometry = NULL
  ))
  cfg <- shiny::reactive(list(
    runtime = list(read_only_mode = TRUE, allow_scenario_writes = FALSE)
  ))
  shiny::testServer(mod_shock_simulator_server, args = list(snap = snap, cfg = cfg), {
    hist <- history()
    skip_if(nrow(hist) < 1L, "No history")
    session$setInputs(history_table_rows_selected = 1L, hist_delete = 1L)
    expect_true(grepl("read-only|disabled", rv$last_error %||% "", ignore.case = TRUE))
  })
})

test_that("no-result empty state message is explicit", {
  expect_true(grepl("No active scenario result", shock_no_active_result_message()))
})

test_that("direct-only path empty state", {
  snap <- shiny::reactiveVal(list(
    detailed_coverage = make_shock_coverage(),
    trade_detailed_enriched = make_shock_detailed_fixture(),
    map_geometry = NULL
  ))
  cfg <- shiny::reactive(list(
    runtime = list(read_only_mode = TRUE, allow_scenario_writes = FALSE)
  ))
  shiny::testServer(mod_shock_simulator_server, args = list(snap = snap, cfg = cfg), {
    session$setInputs(
      scenario_name = "direct",
      baseline_year_start = 2024L, baseline_year_end = 2024L,
      target_supplier_iso3 = "CHN", target_hs_codes = "8542",
      shock_type = "commodity_specific_supplier_reduction",
      shock_size_pct = 30, substitution_mode = "none",
      substitution_capacity_pct = 0, maximum_substitute_supplier_share = 100,
      enable_max_share = FALSE, propagation_mode = "direct_only",
      maximum_propagation_steps = 1L, propagation_decay = 1,
      minimum_propagated_value_usd = 0, minimum_dependency_share = 0,
      include_macro_normalisation = TRUE, acknowledge_partial_coverage = TRUE,
      rep_metric = "residual_unmet_value_usd", rep_top_n = 10,
      com_metric = "residual_unmet_value_usd", map_metric = "residual_unmet_value_usd"
    )
    session$setInputs(run_scenario = 1L)
    expect_true(isTRUE(rv$active_result$ok))
    expect_equal(rv$active_result$scenario$propagation_mode, "direct_only")
    pe <- output$path_empty
    expect_true(!is.null(pe))
  })
})
