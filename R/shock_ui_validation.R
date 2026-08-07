shock_ui_validate_inputs <- function(inputs, baseline = NULL, coverage = NULL) {
  scenario <- shock_ui_to_scenario(inputs, coverage = coverage)
  errors <- character()
  warnings <- character()

  name <- trimws(as.character(scenario$scenario_name %||% ""))
  if (!nzchar(name)) errors <- c(errors, "Scenario name is required.")
  if (nchar(name) > 120) errors <- c(errors, "Scenario name must be at most 120 characters.")

  if (!length(scenario$target_supplier_iso3)) {
    errors <- c(errors, "Select at least one target supplier.")
  }
  vis <- shock_control_visibility(
    scenario$substitution_mode, scenario$propagation_mode, scenario$shock_type
  )
  if (isTRUE(vis$require_hs) && !length(scenario$target_hs_codes)) {
    errors <- c(errors, "Select at least one HS4 commodity for this shock type.")
  }
  if (isTRUE(vis$require_reporters) && !length(scenario$target_reporter_iso3)) {
    errors <- c(errors, "Select at least one reporter for bilateral reduction.")
  }

  if (isTRUE(shock_partial_ack_required(coverage)) &&
      !isTRUE(scenario$acknowledge_partial_coverage)) {
    errors <- c(errors, "Acknowledge partial detailed coverage before running.")
  }

  if (!is.null(baseline) && nrow(baseline) && length(scenario$target_supplier_iso3)) {
    tg <- select_shock_target_edges(baseline, scenario)
    if (!nrow(tg)) {
      errors <- c(errors, "No eligible target edges match the current builder settings.")
    }
  } else if (is.null(baseline) || !nrow(baseline)) {
    errors <- c(errors, "No eligible shock baseline is available for the selected period.")
  }

  eng <- validate_shock_scenario(
    scenario,
    baseline = baseline,
    coverage = coverage,
    force_stale_universe = FALSE
  )
  errors <- unique(c(errors, eng$errors %||% character()))
  warnings <- unique(c(warnings, eng$warnings %||% character()))

  if (isTRUE(is.finite(scenario$shock_size_pct)) && scenario$shock_size_pct >= 100) {
    warnings <- c(warnings, "100% shock removes the full targeted supply edge value.")
  }
  if (isTRUE(is.finite(scenario$shock_size_pct)) && identical(scenario$shock_size_pct, 0)) {
    warnings <- c(warnings, "0% shock is a neutral control scenario.")
  }

  list(
    ok = !length(errors),
    status = if (!length(errors) && !length(warnings)) {
      "Valid"
    } else if (!length(errors)) {
      "Warning"
    } else {
      "Error"
    },
    errors = errors,
    warnings = warnings,
    scenario = eng$scenario %||% normalize_shock_scenario(scenario),
    scenario_hash = eng$scenario_hash %||% shock_scenario_hash(scenario)
  )
}

shock_ui_validation_groups <- function(validation) {
  list(
    status = validation$status %||% "Error",
    errors = validation$errors %||% character(),
    warnings = validation$warnings %||% character(),
    can_run = isTRUE(validation$ok),
    accessible_summary = shock_ui_validation_accessible_summary(validation)
  )
}

shock_ui_validation_accessible_summary <- function(validation) {
  status <- validation$status %||% "Error"
  errs <- validation$errors %||% character()
  warns <- validation$warnings %||% character()
  parts <- c(sprintf("Validation status: %s.", status))
  if (length(errs)) {
    parts <- c(parts, sprintf("%d error(s): %s", length(errs), paste(errs, collapse = "; ")))
  }
  if (length(warns)) {
    parts <- c(parts, sprintf("%d warning(s): %s", length(warns), paste(warns, collapse = "; ")))
  }
  if (!length(errs) && !length(warns)) {
    parts <- c(parts, "Scenario is valid and ready to run.")
  }
  paste(parts, collapse = " ")
}
