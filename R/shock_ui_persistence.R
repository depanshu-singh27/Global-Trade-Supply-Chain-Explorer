shock_index_scenario_history <- function(root = find_project_root()) {
  dirs <- shock_scenario_dirs(root)
  res_dirs <- list.dirs(dirs$results, full.names = TRUE, recursive = FALSE)
  if (!length(res_dirs)) {
    return(data.table::data.table(
      scenario_id = character(),
      scenario_name = character(),
      result_dir = character(),
      created_at = character(),
      status = character()
    ))
  }
  rows <- lapply(res_dirs, function(d) {
    man_path <- file.path(d, "scenario_manifest.json")
    def_path <- file.path(d, "scenario_definition.json")
    if (!file.exists(man_path)) {
      return(data.table::data.table(
        scenario_id = basename(d),
        scenario_name = basename(d),
        result_dir = d,
        created_at = as.character(file.info(d)$mtime),
        status = "incomplete",
        residual_unmet_value_usd = NA_real_,
        engine_version = NA_character_,
        universe_version = NA_character_,
        baseline_year_start = NA_integer_,
        baseline_year_end = NA_integer_,
        shock_size_pct = NA_real_,
        substitution_mode = NA_character_,
        propagation_mode = NA_character_,
        supplier = NA_character_,
        hs_scope = NA_character_
      ))
    }
    man <- tryCatch(jsonlite::fromJSON(man_path), error = function(e) NULL)
    def <- if (file.exists(def_path)) {
      tryCatch(jsonlite::fromJSON(def_path), error = function(e) NULL)
    } else {
      NULL
    }
    if (is.null(man)) {
      return(data.table::data.table(
        scenario_id = basename(d), scenario_name = basename(d), result_dir = d,
        created_at = NA_character_, status = "corrupt"
      ))
    }
    data.table::data.table(
      scenario_id = man$scenario_id %||% basename(d),
      scenario_name = man$scenario_name %||% man$scenario_id,
      result_dir = d,
      created_at = man$created_at %||% as.character(file.info(d)$mtime),
      status = if (isTRUE(man$reconciliation_ok)) "ok" else "warning",
      residual_unmet_value_usd = man$residual_unmet_value_usd %||% NA_real_,
      engine_version = man$engine_version %||% NA_character_,
      universe_version = man$universe_version %||% NA_character_,
      production_status = man$production_status %||% NA_character_,
      baseline_year_start = def$baseline_year_start %||% NA_integer_,
      baseline_year_end = def$baseline_year_end %||% NA_integer_,
      shock_size_pct = def$shock_size_pct %||% NA_real_,
      substitution_mode = def$substitution_mode %||% NA_character_,
      propagation_mode = def$propagation_mode %||% NA_character_,
      supplier = paste(def$target_supplier_iso3 %||% character(), collapse = ","),
      hs_scope = {
        hs <- def$target_hs_codes %||% character()
        if (!length(hs)) "all" else paste(hs, collapse = ",")
      },
      scenario_hash = man$scenario_hash %||% NA_character_,
      result_hash = man$result_hash %||% NA_character_
    )
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  if (nrow(out)) data.table::setorderv(out, "created_at", -1L)
  out
}

shock_read_table_flexible <- function(result_dir, name) {
  pq <- file.path(result_dir, paste0(name, ".parquet"))
  csv <- file.path(result_dir, paste0(name, ".csv"))
  if (file.exists(pq) && requireNamespace("arrow", quietly = TRUE)) {
    return(data.table::as.data.table(arrow::read_parquet(pq)))
  }
  if (file.exists(csv)) return(data.table::fread(csv))
  data.table::data.table()
}

shock_load_persisted_result <- function(result_dir) {
  if (!nzchar(result_dir %||% "") || !dir.exists(result_dir)) {
    return(list(ok = FALSE, errors = "Result directory not found."))
  }
  man_path <- file.path(result_dir, "scenario_manifest.json")
  def_path <- file.path(result_dir, "scenario_definition.json")
  if (!file.exists(man_path) || !file.exists(def_path)) {
    return(list(ok = FALSE, errors = "Incomplete scenario result (missing manifest or definition)."))
  }
  man <- tryCatch(jsonlite::fromJSON(man_path), error = function(e) NULL)
  def <- tryCatch(jsonlite::fromJSON(def_path), error = function(e) NULL)
  if (is.null(man) || is.null(def)) {
    return(list(ok = FALSE, errors = "Corrupt scenario result JSON."))
  }
  sc <- normalize_shock_scenario(def)
  list(
    ok = TRUE,
    scenario = sc,
    scenario_hash = man$scenario_hash,
    result_hash = man$result_hash,
    manifest_scenario_hash = man$scenario_hash,
    engine_version = man$engine_version %||% sc$engine_version,
    production_status = man$production_status %||% "unknown",
    methodology_notice = man$methodology_notice %||% shock_ui_methodology_notice(),
    partial_notice = man$partial_notice %||% "",
    baseline_targets = shock_read_table_flexible(result_dir, "baseline_targets"),
    edge_impacts = shock_read_table_flexible(result_dir, "edge_impacts"),
    reporter_impacts = shock_read_table_flexible(result_dir, "reporter_impacts"),
    commodity_impacts = shock_read_table_flexible(result_dir, "commodity_impacts"),
    supplier_impacts = shock_read_table_flexible(result_dir, "supplier_impacts"),
    post_shock_dependency = shock_read_table_flexible(result_dir, "post_shock_dependency"),
    impact_paths = shock_read_table_flexible(result_dir, "impact_paths"),
    diagnostics = shock_read_table_flexible(result_dir, "scenario_diagnostics"),
    reconciliation = list(ok = isTRUE(man$reconciliation_ok)),
    result_dir = result_dir,
    loaded_from_disk = TRUE
  )
}

shock_safe_delete_result <- function(result_dir, root = find_project_root()) {
  dirs <- shock_scenario_dirs(root)
  if (!nzchar(result_dir %||% "")) {
    return(list(ok = FALSE, error = "Empty result path."))
  }
  target <- normalizePath(result_dir, winslash = "/", mustWork = FALSE)
  root_res <- normalizePath(dirs$results, winslash = "/", mustWork = FALSE)
  examples <- normalizePath(dirs$examples, winslash = "/", mustWork = FALSE)
  if (!startsWith(target, paste0(root_res, "/")) && !identical(dirname(target), root_res)) {

    if (!grepl(paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", root_res)), target)) {
      return(list(ok = FALSE, error = "Deletion rejected: path is outside scenario results."))
    }
  }
  if (startsWith(target, paste0(examples, "/")) || identical(target, examples)) {
    return(list(ok = FALSE, error = "Deletion rejected: tracked example definitions cannot be deleted."))
  }
  if (!dir.exists(target)) {
    return(list(ok = FALSE, error = "Result directory already missing."))
  }
  unlink(target, recursive = TRUE, force = FALSE)
  list(ok = !dir.exists(target), error = if (dir.exists(target)) "Delete failed." else NULL)
}

run_shock_scenario_orchestrated <- function(detailed,
                                              scenario,
                                              coverage = NULL,
                                              root = find_project_root(),
                                              persist = TRUE) {
  result <- run_shock_scenario(
    detailed,
    scenario,
    coverage = coverage,
    enable_timing = FALSE,
    validate_only = FALSE
  )
  if (!isTRUE(result$ok)) return(result)
  if (isTRUE(persist)) {
    persisted <- tryCatch(
      persist_shock_result(result, root = root),
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(persisted$error)) {
      result$persist_warning <- persisted$error
    } else {
      result$result_dir <- persisted$result_dir
      result$manifest <- persisted$manifest
    }
  }
  result
}
