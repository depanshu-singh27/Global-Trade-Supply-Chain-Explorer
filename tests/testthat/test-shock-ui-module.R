test_that("module renders engine status and blocks invalid run", {
  snap <- shiny::reactiveVal(list(
    detailed_coverage = make_shock_coverage(),
    trade_detailed_enriched = make_shock_detailed_fixture(),
    map_geometry = NULL
  ))
  cfg <- shiny::reactive(list())
  shiny::testServer(mod_shock_simulator_server, args = list(snap = snap, cfg = cfg), {
    expect_equal(coverage()$represented_reporter_count, 3L)
    expect_true(nrow(detailed()) > 0)
    session$setInputs(
      scenario_name = "module test",
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
      acknowledge_partial_coverage = FALSE,
      rep_metric = "residual_unmet_value_usd",
      rep_top_n = 10,
      com_metric = "residual_unmet_value_usd",
      map_metric = "residual_unmet_value_usd",
      rep_select = ""
    )
    expect_false(isTRUE(validation()$ok))

    session$setInputs(acknowledge_partial_coverage = TRUE)
    expect_true(isTRUE(validation()$ok))
    expect_true(preview()$n_target_edges >= 1)

    session$setInputs(run_scenario = 1L)
    expect_true(!is.null(rv$active_result))
    expect_true(isTRUE(rv$active_result$ok))
    prior_hash <- rv$active_result$result_hash

    session$setInputs(shock_size_pct = 55)
    expect_true(isTRUE(stale()$inputs_changed))

    k <- shock_prepare_kpis(rv$active_result)
    expect_true(k$available)
    expect_identical(rv$active_result$result_hash, prior_hash)
  })
})

test_that("invalid run attempt preserves prior valid result", {
  snap <- shiny::reactiveVal(list(
    detailed_coverage = make_shock_coverage(),
    trade_detailed_enriched = make_shock_detailed_fixture(),
    map_geometry = NULL
  ))
  cfg <- shiny::reactive(list())
  shiny::testServer(mod_shock_simulator_server, args = list(snap = snap, cfg = cfg), {
    session$setInputs(
      scenario_name = "prior",
      baseline_year_start = 2024L,
      baseline_year_end = 2024L,
      target_supplier_iso3 = "CHN",
      target_hs_codes = "8542",
      shock_type = "commodity_specific_supplier_reduction",
      shock_size_pct = 30,
      substitution_mode = "none",
      substitution_capacity_pct = 0,
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
    session$setInputs(run_scenario = 1L)
    expect_true(isTRUE(rv$active_result$ok))
    prior_hash <- rv$active_result$result_hash

    session$setInputs(target_supplier_iso3 = "ZZZ")
    expect_false(isTRUE(validation()$ok))
    session$setInputs(run_scenario = 2L)
    expect_identical(rv$active_result$result_hash, prior_hash)
  })
})
