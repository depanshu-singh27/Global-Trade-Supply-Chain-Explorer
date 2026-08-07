make_shock_ui_inputs <- function(...) {
  base <- list(
    scenario_name = "UI test scenario",
    scenario_description = "offline",
    baseline_year_start = 2024L,
    baseline_year_end = 2024L,
    target_supplier_iso3 = "CHN",
    target_reporter_iso3 = character(),
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
    universe_version = "uv_262deb46e00d2f216a5a"
  )
  dots <- list(...)
  for (nm in names(dots)) base[[nm]] <- dots[[nm]]
  base
}

make_shock_ui_result <- function(...) {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  sc <- make_base_scenario(...)
  run_shock_scenario(det, sc, coverage = cov)
}
