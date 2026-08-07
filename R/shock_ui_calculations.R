shock_target_preview <- function(baseline, scenario) {
  sc <- normalize_shock_scenario(scenario)
  bl <- data.table::as.data.table(baseline)
  tg <- select_shock_target_edges(bl, sc)
  shock_size <- sc$shock_size_pct / 100
  targeted_value <- sum(tg$baseline_import_value_usd, na.rm = TRUE)
  potential_direct <- targeted_value * shock_size

  groups <- unique(tg[, .(reporter_iso3, hs_code)])
  n_subst <- 0L
  capacity <- 0
  if (nrow(groups) && nrow(bl)) {
    for (i in seq_len(nrow(groups))) {
      g <- groups[i]
      subs <- bl[
        reporter_iso3 == g$reporter_iso3 &
          hs_code == g$hs_code &
          !(supplier_iso3 %in% sc$target_supplier_iso3) &
          baseline_import_value_usd > 0
      ]
      n_subst <- n_subst + nrow(subs)
      if (sc$substitution_mode %in% c("capacity_constrained", "diversification", "proportional")) {
        capacity <- capacity + sum(
          subs$baseline_import_value_usd * (sc$substitution_capacity_pct / 100),
          na.rm = TRUE
        )
      }
    }
  }

  list(
    n_target_edges = nrow(tg),
    n_reporters = data.table::uniqueN(tg$reporter_iso3),
    n_commodities = data.table::uniqueN(tg$hs_code),
    n_suppliers = data.table::uniqueN(tg$supplier_iso3),
    target_suppliers = sort(unique(tg$supplier_iso3)),
    target_reporters = sort(unique(tg$reporter_iso3)),
    target_hs_codes = sort(unique(tg$hs_code)),
    targeted_baseline_value_usd = targeted_value,
    potential_direct_disruption_usd = potential_direct,
    eligible_substitute_supplier_rows = as.integer(n_subst),
    indicative_substitution_capacity_usd = capacity,
    baseline_year_start = sc$baseline_year_start,
    baseline_year_end = sc$baseline_year_end,
    shock_size_pct = sc$shock_size_pct,
    neutral_control = isTRUE(identical(as.numeric(sc$shock_size_pct), 0)),
    propagation_mode = sc$propagation_mode,
    maximum_propagation_steps = sc$maximum_propagation_steps,
    n_baseline_edges = nrow(bl),
    n_reporter_commodity_groups = nrow(unique(bl[, .(reporter_iso3, hs_code)])),
    complexity = list(
      edge_count = nrow(bl),
      reporter_commodity_group_count = nrow(unique(bl[, .(reporter_iso3, hs_code)])),
      propagation_mode = sc$propagation_mode,
      maximum_depth = sc$maximum_propagation_steps
    )
  )
}

shock_prepare_kpis <- function(result) {
  if (is.null(result) || !isTRUE(result$ok)) {
    return(list(available = FALSE))
  }
  edges <- data.table::as.data.table(result$edge_impacts)
  rep <- data.table::as.data.table(result$reporter_impacts)
  com <- data.table::as.data.table(result$commodity_impacts)
  paths <- data.table::as.data.table(result$impact_paths %||% data.table::data.table())

  direct <- sum(edges$direct_disrupted_value_usd, na.rm = TRUE)
  subst <- sum(edges$substitution_allocated_usd, na.rm = TRUE)
  resid <- sum(edges$residual_unmet_value_usd, na.rm = TRUE)
  targeted <- sum(edges$baseline_import_value_usd[edges$is_targeted %||% FALSE], na.rm = TRUE)
  if (!targeted && "baseline_targets" %in% names(result)) {
    targeted <- sum(result$baseline_targets$baseline_import_value_usd, na.rm = TRUE)
  }
  prop <- if (nrow(paths) && "propagated_value_usd" %in% names(paths)) {
    sum(paths$propagated_value_usd, na.rm = TRUE)
  } else {
    0
  }

  list(
    available = TRUE,
    targeted_baseline_imports_usd = sanitize_chart_numeric(targeted),
    direct_disrupted_imports_usd = sanitize_chart_numeric(direct),
    substitution_allocated_usd = sanitize_chart_numeric(subst),
    residual_unmet_imports_usd = sanitize_chart_numeric(resid),
    substitution_rate = if (direct > 0) sanitize_chart_numeric(subst / direct) else NA_real_,
    residual_pct_targeted = if (targeted > 0) sanitize_chart_numeric(100 * resid / targeted) else NA_real_,
    residual_pct_total_hs85 = if (nrow(rep)) {
      sanitize_chart_numeric(mean(rep$residual_unmet_pct_total_hs85_imports, na.rm = TRUE))
    } else {
      NA_real_
    },
    affected_reporters = data.table::uniqueN(rep$reporter_iso3[rep$residual_unmet_value_usd > 0]),
    affected_commodities = data.table::uniqueN(com$hs_code[com$residual_unmet_value_usd > 0]),
    affected_suppliers = data.table::uniqueN(edges$supplier_iso3[edges$is_targeted %||% FALSE]),
    max_reporter_residual_usd = if (nrow(rep)) max(rep$residual_unmet_value_usd, na.rm = TRUE) else 0,
    propagation_contribution_usd = sanitize_chart_numeric(prop),
    engine_version = result$engine_version %||% SHOCK_ENGINE_VERSION,
    universe_version = result$scenario$universe_version,
    production_status = result$production_status,
    reconciliation_ok = isTRUE(result$reconciliation$ok)
  )
}

shock_rank_reporters_ui <- function(reporter_impacts, metric = "residual_unmet_value_usd", top_n = 15L) {
  rank_shock_impacts(reporter_impacts, metric = metric, top_n = top_n)
}

shock_rank_commodities_ui <- function(commodity_impacts,
                                        metric = "residual_unmet_value_usd",
                                        top_n = 15L) {
  dt <- data.table::as.data.table(commodity_impacts)
  if (!nrow(dt) || !metric %in% names(dt)) return(dt[0])
  dt <- dt[is.finite(get(metric))]
  data.table::setorderv(dt, c(metric, "hs_code"), order = c(-1L, 1L))
  dt[, scenario_rank := seq_len(.N)]
  utils::head(dt, as.integer(top_n))
}

shock_supplier_allocation_summary <- function(supplier_impacts, edges = NULL) {
  dt <- data.table::as.data.table(supplier_impacts)
  if (!nrow(dt)) {
    return(list(
      shocked = data.table::data.table(),
      substitutes = data.table::data.table(),
      totals = list(removed = 0, received = 0)
    ))
  }
  shocked <- if ("role" %in% names(dt)) {
    dt[role %in% c("shocked_supplier", "shocked supplier")]
  } else {
    dt[0]
  }
  subs <- if ("role" %in% names(dt)) {
    dt[role %in% c("substitute_supplier", "substitute supplier", "shocked and substitute")]
  } else {
    dt[0]
  }
  rem_col <- intersect(
    c("direct_value_removed_usd", "direct_disrupted_value_usd"),
    names(dt)
  )
  recv_col <- intersect(
    c("additional_substitution_received_usd", "substitution_received_usd", "substitution_allocated_usd"),
    names(dt)
  )
  list(
    shocked = shocked,
    substitutes = subs,
    totals = list(
      removed = if (length(rem_col)) sum(dt[[rem_col[1]]], na.rm = TRUE) else 0,
      received = if (length(recv_col)) sum(dt[[recv_col[1]]], na.rm = TRUE) else 0
    )
  )
}

shock_prepare_concentration_changes <- function(post_shock_dependency, top_n = 50L) {
  dt <- data.table::as.data.table(post_shock_dependency)
  if (!nrow(dt)) return(dt)

  keep <- intersect(
    c(
      "reporter_iso3", "hs_code", "supplier_hhi", "post_shock_hhi",
      "hhi_change", "effective_supplier_count", "post_shock_effective_supplier_count",
      "top_1_share", "post_shock_top_1_share", "residual_unmet_value_usd",
      "post_shock_import_value_usd"
    ),
    names(dt)
  )
  out <- dt[, keep, with = FALSE]
  if ("post_shock_import_value_usd" %in% names(out)) {
    out <- out[is.na(post_shock_import_value_usd) | post_shock_import_value_usd >= 0]
  }
  if ("hhi_change" %in% names(out)) {
    data.table::setorderv(out, c("hhi_change", "reporter_iso3", "hs_code"), c(-1L, 1L, 1L))
  }
  utils::head(out, as.integer(top_n))
}

shock_builder_result_equal <- function(builder_scenario, result_scenario) {
  if (is.null(builder_scenario) || is.null(result_scenario)) return(FALSE)
  identical(
    shock_scenario_hash(builder_scenario),
    shock_scenario_hash(result_scenario)
  )
}

shock_stale_result_state <- function(builder_scenario,
                                       active_result,
                                       coverage = NULL,
                                       engine_version = SHOCK_ENGINE_VERSION) {
  reasons <- character()
  if (is.null(active_result) || !isTRUE(active_result$ok)) {
    return(list(stale = FALSE, reasons = character(), inputs_changed = FALSE))
  }
  res_sc <- active_result$scenario
  inputs_changed <- !shock_builder_result_equal(builder_scenario, res_sc)
  if (inputs_changed) reasons <- c(reasons, "Builder inputs differ from the active result.")

  cur_uv <- coverage$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM
  if (!identical(as.character(res_sc$universe_version), as.character(cur_uv))) {
    reasons <- c(reasons, "Universe checksum differs from the active analytical universe.")
  }
  if (!identical(as.character(active_result$engine_version %||% res_sc$engine_version),
                 as.character(engine_version))) {
    reasons <- c(reasons, "Engine version differs from the current shock engine.")
  }
  if (!is.null(active_result$scenario_hash) && !is.null(active_result$result_hash)) {

    if (!is.null(active_result$manifest_scenario_hash) &&
        !identical(active_result$scenario_hash, active_result$manifest_scenario_hash)) {
      reasons <- c(reasons, "Scenario manifest hash disagrees with the loaded definition.")
    }
  }
  list(
    stale = length(reasons) > 0L,
    inputs_changed = inputs_changed,
    reasons = reasons,
    message = if (length(reasons)) {
      paste(c("Inputs changed since last run / stale result:", reasons), collapse = " ")
    } else {
      ""
    }
  )
}

shock_safe_display_numeric <- function(x) {
  x <- sanitize_chart_numeric(x)
  ifelse(!is.finite(x), NA_real_, x)
}

shock_kpi_text_summary <- function(kpis) {
  if (!isTRUE(kpis$available)) return("No scenario results available.")
  sprintf(
    paste(
      "Targeted baseline imports %s; direct disrupted imports %s;",
      "substitution allocated %s; residual unmet imports %s.",
      "Affected reporters %d; affected HS4 commodities %d.",
      "Results are scenario sensitivities, not forecasts."
    ),
    format_shock_usd(kpis$targeted_baseline_imports_usd),
    format_shock_usd(kpis$direct_disrupted_imports_usd),
    format_shock_usd(kpis$substitution_allocated_usd),
    format_shock_usd(kpis$residual_unmet_imports_usd),
    as.integer(kpis$affected_reporters %||% 0L),
    as.integer(kpis$affected_commodities %||% 0L)
  )
}
