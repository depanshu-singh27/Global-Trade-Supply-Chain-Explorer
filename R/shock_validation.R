validate_shock_scenario <- function(scenario,
                                      baseline = NULL,
                                      coverage = NULL,
                                      force_stale_universe = FALSE) {
  errors <- character()
  warnings <- character()
  sc <- tryCatch(normalize_shock_scenario(scenario), error = function(e) {
    errors <<- c(errors, conditionMessage(e))
    NULL
  })
  if (is.null(sc)) {
    return(list(ok = FALSE, errors = errors, warnings = warnings, scenario = NULL))
  }

  if (!sc$shock_type %in% shock_types()) {
    errors <- c(errors, paste("Unsupported shock_type:", sc$shock_type))
  }
  if (!sc$substitution_mode %in% shock_substitution_modes()) {
    errors <- c(errors, paste("Unsupported substitution_mode:", sc$substitution_mode))
  }
  if (!sc$propagation_mode %in% shock_propagation_modes()) {
    errors <- c(errors, paste("Unsupported propagation_mode:", sc$propagation_mode))
  }
  if (!is.finite(sc$shock_size_pct) || sc$shock_size_pct < 0 || sc$shock_size_pct > 100) {
    errors <- c(errors, "shock_size_pct must be between 0 and 100.")
  }
  if (!is.finite(sc$substitution_capacity_pct) ||
      sc$substitution_capacity_pct < 0 || sc$substitution_capacity_pct > 100) {
    errors <- c(errors, "substitution_capacity_pct must be between 0 and 100.")
  }
  if (!is.finite(sc$maximum_substitute_supplier_share) ||
      sc$maximum_substitute_supplier_share < 0 ||
      sc$maximum_substitute_supplier_share > 1) {
    errors <- c(errors, "maximum_substitute_supplier_share must be between 0 and 1.")
  }
  if (!is.finite(sc$propagation_decay) || sc$propagation_decay < 0 || sc$propagation_decay > 1) {
    errors <- c(errors, "propagation_decay must be between 0 and 1.")
  }
  if (!is.finite(sc$propagation_pass_through) ||
      sc$propagation_pass_through < 0 || sc$propagation_pass_through > 1) {
    errors <- c(errors, "propagation_pass_through must be between 0 and 1.")
  }
  if (is.na(sc$maximum_propagation_steps) || sc$maximum_propagation_steps < 0L) {
    errors <- c(errors, "maximum_propagation_steps must be a non-negative integer.")
  }
  if (!is.finite(sc$minimum_propagated_value_usd) || sc$minimum_propagated_value_usd < 0) {
    errors <- c(errors, "minimum_propagated_value_usd must be non-negative.")
  }
  if (!is.finite(sc$minimum_dependency_share) ||
      sc$minimum_dependency_share < 0 || sc$minimum_dependency_share > 1) {
    errors <- c(errors, "minimum_dependency_share must be between 0 and 1.")
  }
  if (!length(sc$target_supplier_iso3)) {
    errors <- c(errors, "At least one target_supplier_iso3 is required.")
  }

  if (!is.null(coverage)) {
    status <- as.character(coverage$production_status %||% "unknown")
    if (identical(status, "partial") && !isTRUE(sc$acknowledge_partial_coverage)) {
      errors <- c(
        errors,
        "Detailed coverage is partial; set acknowledge_partial_coverage=true to proceed."
      )
    }
    checksum <- as.character(
      coverage$universe_checksum %||% coverage$universe_version %||% NA_character_
    )
    if (!is.na(checksum) && nzchar(checksum) &&
        !identical(checksum, as.character(sc$universe_version))) {
      if (isTRUE(force_stale_universe)) {
        warnings <- c(warnings, "Executing with stale universe_version (forced).")
      } else {
        errors <- c(
          errors,
          sprintf(
            "Scenario universe_version (%s) differs from active universe (%s).",
            sc$universe_version, checksum
          )
        )
      }
    }
  }

  if (!is.null(baseline) && is.data.frame(baseline) && nrow(baseline)) {
    bl <- data.table::as.data.table(baseline)
    if (!any(sc$target_supplier_iso3 %in% bl$supplier_iso3)) {
      errors <- c(errors, "No target supplier exists in the active baseline.")
    }
    if (length(sc$target_hs_codes) &&
        !any(sc$target_hs_codes %in% bl$hs_code)) {
      errors <- c(errors, "No target HS4 code exists in the active baseline.")
    }
    if (length(sc$target_reporter_iso3) &&
        !any(sc$target_reporter_iso3 %in% bl$reporter_iso3)) {
      errors <- c(errors, "No target reporter exists in the active baseline.")
    }
  } else if (!is.null(baseline) && (!is.data.frame(baseline) || !nrow(baseline))) {
    errors <- c(errors, "Baseline detailed import relationships are absent.")
  }

  nm <- names(sc)
  if (any(grepl("secret|token|password|COMTRADE|api_key|raw_file", nm, ignore.case = TRUE))) {
    errors <- c(errors, "Scenario contains forbidden secret-bearing field names.")
  }

  list(
    ok = !length(errors),
    errors = errors,
    warnings = warnings,
    scenario = sc,
    scenario_hash = if (!is.null(sc)) shock_scenario_hash(sc) else NA_character_
  )
}
