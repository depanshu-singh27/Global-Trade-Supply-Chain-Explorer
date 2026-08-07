build_shock_diagnostics <- function(baseline_obj,
                                      scenario,
                                      direct,
                                      substituted,
                                      propagated,
                                      reporter_impacts,
                                      timing = NULL,
                                      coverage = NULL) {
  sc <- normalize_shock_scenario(scenario)
  edges <- data.table::as.data.table(propagated$edges %||% substituted$edges)
  groups <- data.table::as.data.table(substituted$group_summary)
  paths <- data.table::as.data.table(propagated$paths %||% empty_impact_paths())
  excl <- baseline_obj$diagnostics$excluded %||% data.table::data.table()

  direct_tot <- sum(edges$direct_disrupted_value_usd, na.rm = TRUE)
  subst_tot <- sum(edges$substitution_allocated_usd, na.rm = TRUE)
  resid_tot <- sum(edges$residual_unmet_value_usd, na.rm = TRUE)
  recon_ok <- abs(direct_tot - (subst_tot + resid_tot)) <= max(SHOCK_VALUE_TOLERANCE, 1e-6 * max(1, direct_tot))

  baseline_reporter_n <- if (nrow(baseline_obj$baseline)) {
    data.table::uniqueN(baseline_obj$baseline$reporter_iso3)
  } else {
    0L
  }
  represented_n <- coverage$represented_reporter_count %||% baseline_reporter_n
  selected_n <- coverage$selected_reporter_count %||% NA_integer_

  diag <- data.table::data.table(
    metric = c(
      "engine_version", "scenario_id", "scenario_hash",
      "baseline_eligible_rows", "targeted_rows", "unaffected_rows",
      "direct_disrupted_value_usd", "substitution_allocated_value_usd",
      "residual_unmet_value_usd", "unused_substitution_capacity_usd",
      "constraint_binding_groups", "propagation_paths", "max_propagation_depth",
      "reconciliation_ok", "finite_value_ok", "negative_value_ok",
      "production_status", "universe_version",
      "baseline_reporter_count",
      "represented_reporter_count", "selected_reporter_count",
      "elapsed_ms"
    ),
    value = as.character(c(
      SHOCK_ENGINE_VERSION,
      sc$scenario_id,
      shock_scenario_hash(sc),
      baseline_obj$diagnostics$eligible_rows %||% nrow(baseline_obj$baseline),
      sum(edges$is_targeted, na.rm = TRUE),
      sum(!edges$is_targeted, na.rm = TRUE),
      direct_tot, subst_tot, resid_tot,
      sum(groups$unused_substitution_capacity_usd, na.rm = TRUE),
      sum(groups$unused_substitution_capacity_usd > 0 &
            groups$residual_unmet_value_usd > 0, na.rm = TRUE),
      nrow(paths),
      if (nrow(paths)) max(paths$depth, na.rm = TRUE) else 0L,
      recon_ok,
      !any(!is.finite(edges$residual_unmet_value_usd) &
             !is.na(edges$residual_unmet_value_usd)),
      !any(edges$residual_unmet_value_usd < -SHOCK_VALUE_TOLERANCE, na.rm = TRUE) &&
        !any(edges$direct_disrupted_value_usd < -SHOCK_VALUE_TOLERANCE, na.rm = TRUE),
      baseline_obj$production_status %||% "unknown",
      sc$universe_version,
      baseline_reporter_n,
      represented_n,
      selected_n,
      if (!is.null(timing$elapsed_ms)) timing$elapsed_ms else NA_real_
    ))
  )
  if (anyDuplicated(diag$metric)) {
    stop("Shock diagnostics contain duplicate metric keys.", call. = FALSE)
  }
  diag
}

reconcile_shock_result <- function(edges, reporter_impacts, commodity_impacts, paths = NULL) {
  ed <- data.table::as.data.table(edges)
  checks <- list()
  if (nrow(ed)) {
    checks$direct_equals_subst_plus_residual <- abs(
      sum(ed$direct_disrupted_value_usd, na.rm = TRUE) -
        (sum(ed$substitution_allocated_usd, na.rm = TRUE) +
           sum(ed$residual_unmet_value_usd, na.rm = TRUE))
    ) <= max(1e-4, 1e-8 * sum(ed$direct_disrupted_value_usd, na.rm = TRUE))

    checks$no_negative <- !any(
      ed$direct_disrupted_value_usd < -SHOCK_VALUE_TOLERANCE |
        ed$residual_unmet_value_usd < -SHOCK_VALUE_TOLERANCE |
        ed$substitution_allocated_usd < -SHOCK_VALUE_TOLERANCE,
      na.rm = TRUE
    )
    checks$finite <- all(is.finite(ed$direct_disrupted_value_usd) |
                           is.na(ed$direct_disrupted_value_usd)) &&
      all(is.finite(ed$residual_unmet_value_usd) | is.na(ed$residual_unmet_value_usd))
  } else {
    checks$direct_equals_subst_plus_residual <- TRUE
    checks$no_negative <- TRUE
    checks$finite <- TRUE
  }
  if (!is.null(reporter_impacts) && nrow(reporter_impacts) && nrow(ed)) {
    checks$reporter_residual_matches_edges <- abs(
      sum(reporter_impacts$residual_unmet_value_usd, na.rm = TRUE) -
        sum(ed$residual_unmet_value_usd, na.rm = TRUE)
    ) <= max(1e-4, 1e-8 * sum(ed$residual_unmet_value_usd, na.rm = TRUE))
  } else {
    checks$reporter_residual_matches_edges <- TRUE
  }
  if (!is.null(commodity_impacts) && nrow(commodity_impacts) && nrow(ed)) {
    checks$commodity_residual_matches_edges <- abs(
      sum(commodity_impacts$residual_unmet_value_usd, na.rm = TRUE) -
        sum(ed$residual_unmet_value_usd, na.rm = TRUE)
    ) <= max(1e-4, 1e-8 * sum(ed$residual_unmet_value_usd, na.rm = TRUE))
  } else {
    checks$commodity_residual_matches_edges <- TRUE
  }
  checks$ok <- all(unlist(checks))
  checks
}

run_shock_scenario <- function(detailed,
                                 scenario,
                                 coverage = NULL,
                                 enable_timing = FALSE,
                                 force_stale_universe = FALSE,
                                 validate_only = FALSE) {
  t0 <- if (isTRUE(enable_timing)) proc.time()[["elapsed"]] else NULL
  sc_raw <- scenario

  sc_try <- tryCatch(normalize_shock_scenario(sc_raw), error = function(e) NULL)
  year_min <- sc_try$baseline_year_start %||% 2024L
  year_max <- sc_try$baseline_year_end %||% 2024L
  universe_version <- sc_try$universe_version %||%
    coverage$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM

  baseline_obj <- build_shock_baseline(
    detailed,
    year_min = year_min,
    year_max = year_max,
    coverage = coverage,
    universe_version = universe_version
  )

  validation <- validate_shock_scenario(
    sc_raw,
    baseline = baseline_obj$baseline,
    coverage = coverage,
    force_stale_universe = force_stale_universe
  )
  if (!isTRUE(validation$ok)) {
    return(list(
      ok = FALSE,
      validation = validation,
      scenario = validation$scenario,
      errors = validation$errors
    ))
  }
  if (isTRUE(validate_only)) {
    return(list(ok = TRUE, validation = validation, scenario = validation$scenario))
  }

  sc <- validation$scenario
  if (!nrow(baseline_obj$baseline)) {
    return(list(
      ok = FALSE,
      validation = validation,
      scenario = sc,
      errors = "No eligible baseline edges for the requested period."
    ))
  }
  targets <- select_shock_target_edges(baseline_obj$baseline, sc)
  if (!nrow(targets)) {
    return(list(
      ok = FALSE,
      validation = validation,
      scenario = sc,
      errors = "No eligible target edges matched the scenario filters."
    ))
  }

  direct <- apply_direct_shock(baseline_obj$baseline, sc)
  substituted <- apply_substitution(direct, sc)
  propagated <- apply_shock_propagation(substituted, baseline_obj$baseline, sc)
  edges <- propagated$edges
  paths <- propagated$paths

  reporter_impacts <- aggregate_shock_reporter_impacts(edges, sc)
  commodity_impacts <- aggregate_shock_commodity_impacts(edges)
  supplier_impacts <- aggregate_shock_supplier_impacts(edges)
  concentration <- post_shock_concentration(edges)
  rankings <- rank_shock_impacts(reporter_impacts)

  recon <- reconcile_shock_result(edges, reporter_impacts, commodity_impacts, paths)
  timing <- list(
    elapsed_ms = if (!is.null(t0)) {
      1000 * (proc.time()[["elapsed"]] - t0)
    } else NA_real_
  )
  diagnostics <- build_shock_diagnostics(
    baseline_obj, sc, direct, substituted, propagated, reporter_impacts,
    timing = timing,
    coverage = coverage
  )

  result_hash <- shock_result_hash(list(
    scenario_hash = validation$scenario_hash,
    residual = sum(edges$residual_unmet_value_usd, na.rm = TRUE),
    direct = sum(edges$direct_disrupted_value_usd, na.rm = TRUE),
    subst = sum(edges$substitution_allocated_usd, na.rm = TRUE),
    n_paths = nrow(paths)
  ))

  list(
    ok = TRUE,
    validation = validation,
    scenario = sc,
    scenario_hash = validation$scenario_hash,
    result_hash = result_hash,
    baseline = baseline_obj$baseline,
    baseline_targets = targets,
    edge_impacts = edges,
    reporter_impacts = reporter_impacts,
    commodity_impacts = commodity_impacts,
    supplier_impacts = supplier_impacts,
    post_shock_dependency = concentration,
    impact_paths = paths,
    rankings = rankings,
    diagnostics = diagnostics,
    reconciliation = recon,
    group_summary = substituted$group_summary,
    production_status = baseline_obj$production_status,
    engine_version = SHOCK_ENGINE_VERSION,
    methodology_notice = shock_methodology_notice(),
    partial_notice = shock_partial_status_notice(
      coverage$represented_reporter_count,
      coverage$selected_reporter_count,
      coverage = coverage
    )
  )
}

shock_result_hash <- function(x) {
  payload <- jsonlite::toJSON(x, auto_unbox = TRUE, digits = 10)
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(as.character(payload), algo = "xxhash64"))
  }
  sprintf("%08x", sum(utf8ToInt(as.character(payload))) %% (2^31 - 1))
}

shock_engine_status <- function(snap = NULL) {
  cov <- NULL
  if (!is.null(snap)) {
    cov <- snap$detailed_coverage %||% trade_flow_coverage_status(snap)
  }
  list(
    engine_version = SHOCK_ENGINE_VERSION,
    ui_phase = "interactive_phase11",
    ready = TRUE,
    production_status = cov$production_status %||% "unknown",
    represented_reporter_count = cov$represented_reporter_count %||% NA_integer_,
    selected_reporter_count = cov$selected_reporter_count %||% NA_integer_,
    universe_checksum = cov$universe_checksum %||% NA_character_,
    methodology_notice = shock_methodology_notice(),
    partial_notice = shock_partial_status_notice(
      cov$represented_reporter_count, cov$selected_reporter_count, coverage = cov
    ),
    phase11_note = "Interactive Shock Simulator UI arrives in Phase 11."
  )
}
