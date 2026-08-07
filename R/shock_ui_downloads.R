shock_ui_download_table <- function(dt, meta = list()) {
  out <- data.table::as.data.table(dt)
  drop <- grep(
    "path|url|header|secret|token|key|raw_file|cache|COMTRADE|request_id|absolute",
    names(out),
    ignore.case = TRUE,
    value = TRUE
  )
  if (length(drop)) out[, (drop) := NULL]
  char_cols <- names(out)[vapply(out, is.character, logical(1))]
  for (cc in char_cols) {
    if (any(grepl("^(/|[A-Za-z]:\\\\|file://)", out[[cc]] %||% ""), na.rm = TRUE)) {
      out[, (cc) := NULL]
    }
  }

  if (length(meta)) {
    for (nm in names(meta)) {
      out[, (nm) := meta[[nm]]]
    }
  }
  out
}

shock_result_download_meta <- function(result) {
  list(
    engine_version = result$engine_version %||% SHOCK_ENGINE_VERSION,
    universe_version = result$scenario$universe_version %||% NA_character_,
    production_status = result$production_status %||% NA_character_,
    baseline_year_start = result$scenario$baseline_year_start %||% NA_integer_,
    baseline_year_end = result$scenario$baseline_year_end %||% NA_integer_,
    scenario_id = result$scenario$scenario_id %||% NA_character_,
    scenario_name = result$scenario$scenario_name %||% NA_character_,
    units = "current_usd"
  )
}

shock_prepare_scenario_report_md <- function(result) {
  if (is.null(result) || !isTRUE(result$ok)) {
    return("# Scenario report unavailable\n\nNo successful scenario result is loaded.\n")
  }
  k <- shock_prepare_kpis(result)
  sc <- result$scenario
  paste0(
    "# Shock scenario report\n\n",
    "## Interpretation\n\n",
    shock_ui_methodology_notice(), "\n\n",
    "## Scenario definition\n\n",
    "- Name: ", sc$scenario_name, "\n",
    "- ID: ", sc$scenario_id, "\n",
    "- Supplier: ", paste(sc$target_supplier_iso3, collapse = ", "), "\n",
    "- HS4: ", paste(sc$target_hs_codes %||% "all selected", collapse = ", "), "\n",
    "- Shock: ", sc$shock_size_pct, "%\n",
    "- Substitution: ", sc$substitution_mode, "\n",
    "- Propagation: ", sc$propagation_mode, "\n",
    "- Baseline: ", sc$baseline_year_start, "–", sc$baseline_year_end, "\n",
    "- Engine: ", result$engine_version, "\n",
    "- Universe: ", sc$universe_version, "\n",
    "- Production status: ", result$production_status, "\n\n",
    "## KPI summary\n\n",
    "- Targeted baseline imports: ", format_shock_usd(k$targeted_baseline_imports_usd), "\n",
    "- Direct disrupted imports: ", format_shock_usd(k$direct_disrupted_imports_usd), "\n",
    "- Substitution allocated: ", format_shock_usd(k$substitution_allocated_usd), "\n",
    "- Residual unmet imports: ", format_shock_usd(k$residual_unmet_imports_usd), "\n",
    "- Affected reporters: ", k$affected_reporters, "\n",
    "- Affected HS4: ", k$affected_commodities, "\n\n",
    "## Coverage\n\n",
    result$partial_notice %||% "", "\n\n",
    "Residual unmet imports are scenario exposures, not predicted economic losses.\n"
  )
}

shock_contains_forbidden_download_content <- function(text_or_dt) {
  txt <- if (is.data.frame(text_or_dt)) {
    paste(unlist(text_or_dt), collapse = " ")
  } else {
    as.character(text_or_dt)
  }
  secret_token <- paste0("COMTRADE", "_", "PRIMARY")
  grepl(
    paste0(secret_token, "|file://|[A-Za-z]:\\\\|/home/|/Users/.*/\\.Renviron"),
    txt
  )
}
