shock_comparison_compatibility <- function(result_a, result_b) {
  warnings <- character()
  errors <- character()
  if (is.null(result_a) || !isTRUE(result_a$ok) || is.null(result_b) || !isTRUE(result_b$ok)) {
    errors <- c(errors, "Both scenarios must have successful results to compare.")
    return(list(ok = FALSE, errors = errors, warnings = warnings))
  }
  uv_a <- result_a$scenario$universe_version %||% result_a$universe_version
  uv_b <- result_b$scenario$universe_version %||% result_b$universe_version
  if (!identical(as.character(uv_a), as.character(uv_b))) {
    warnings <- c(
      warnings,
      "Universe checksums differ; comparison may be misleading across analytical universes."
    )
  }
  ya <- c(result_a$scenario$baseline_year_start, result_a$scenario$baseline_year_end)
  yb <- c(result_b$scenario$baseline_year_start, result_b$scenario$baseline_year_end)
  if (!identical(as.integer(ya), as.integer(yb))) {
    warnings <- c(warnings, "Baseline periods differ between the compared scenarios.")
  }
  eng_a <- result_a$engine_version %||% result_a$scenario$engine_version
  eng_b <- result_b$engine_version %||% result_b$scenario$engine_version
  if (!identical(as.character(eng_a), as.character(eng_b))) {
    warnings <- c(warnings, "Engine versions differ between the compared scenarios.")
  }
  list(
    ok = !length(errors),
    errors = errors,
    warnings = warnings,
    universe_a = uv_a,
    universe_b = uv_b,
    engine_a = eng_a,
    engine_b = eng_b
  )
}

shock_prepare_comparison_ui <- function(result_a, result_b, id_a = "A", id_b = "B") {
  compat <- shock_comparison_compatibility(result_a, result_b)
  if (!isTRUE(compat$ok)) {
    return(list(ok = FALSE, compatibility = compat, comparison = NULL))
  }
  cmp <- compare_shock_scenarios(result_a, result_b, id_a = id_a, id_b = id_b)
  ka <- shock_prepare_kpis(result_a)
  kb <- shock_prepare_kpis(result_b)
  kpi_diff <- data.table::data.table(
    metric = c(
      "direct_disrupted_imports_usd", "substitution_allocated_usd",
      "residual_unmet_imports_usd", "affected_reporters", "affected_commodities"
    ),
    value_a = c(
      ka$direct_disrupted_imports_usd, ka$substitution_allocated_usd,
      ka$residual_unmet_imports_usd, ka$affected_reporters, ka$affected_commodities
    ),
    value_b = c(
      kb$direct_disrupted_imports_usd, kb$substitution_allocated_usd,
      kb$residual_unmet_imports_usd, kb$affected_reporters, kb$affected_commodities
    )
  )
  kpi_diff[, abs_diff := value_b - value_a]
  kpi_diff[, pct_diff := data.table::fifelse(
    is.finite(value_a) & abs(value_a) > 0,
    100 * abs_diff / value_a,
    NA_real_
  )]

  params <- data.table::data.table(
    parameter = c(
      "scenario_name", "shock_size_pct", "substitution_mode", "propagation_mode",
      "baseline_year_start", "baseline_year_end", "engine_version", "universe_version"
    ),
    scenario_a = c(
      result_a$scenario$scenario_name, result_a$scenario$shock_size_pct,
      result_a$scenario$substitution_mode, result_a$scenario$propagation_mode,
      result_a$scenario$baseline_year_start, result_a$scenario$baseline_year_end,
      compat$engine_a, compat$universe_a
    ),
    scenario_b = c(
      result_b$scenario$scenario_name, result_b$scenario$shock_size_pct,
      result_b$scenario$substitution_mode, result_b$scenario$propagation_mode,
      result_b$scenario$baseline_year_start, result_b$scenario$baseline_year_end,
      compat$engine_b, compat$universe_b
    )
  )

  list(
    ok = TRUE,
    compatibility = compat,
    comparison = cmp,
    kpi_diff = kpi_diff,
    parameters = params,
    newly_affected = cmp$aligned_reporters[isTRUE(newly_affected)]$reporter_iso3,
    no_longer_affected = cmp$aligned_reporters[isTRUE(no_longer_affected)]$reporter_iso3
  )
}
