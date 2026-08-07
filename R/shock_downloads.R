shock_scenario_dirs <- function(root = find_project_root()) {
  list(
    root = file.path(root, "data", "scenarios"),
    definitions = file.path(root, "data", "scenarios", "definitions"),
    examples = file.path(root, "data", "scenarios", "examples"),
    results = file.path(root, "data", "scenarios", "results")
  )
}

ensure_shock_scenario_dirs <- function(root = find_project_root()) {
  dirs <- shock_scenario_dirs(root)
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  invisible(dirs)
}

safe_shock_result_dir <- function(scenario, root = find_project_root()) {
  sc <- normalize_shock_scenario(scenario)
  dirs <- ensure_shock_scenario_dirs(root)
  name <- paste(
    sanitize_shock_token(sc$scenario_id, "scn"),
    sanitize_shock_token(sc$engine_version, "eng"),
    sep = "_"
  )
  path <- file.path(dirs$results, name)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

write_shock_table <- function(dt, path) {
  out <- data.table::as.data.table(dt)
  drop <- grep("path|url|header|secret|token|key|raw_file|cache|COMTRADE|request_id",
               names(out), ignore.case = TRUE, value = TRUE)
  if (length(drop)) out[, (drop) := NULL]

  char_cols <- names(out)[vapply(out, is.character, logical(1))]
  for (cc in char_cols) {
    if (any(grepl("^(/|[A-Za-z]:\\\\|file://)", out[[cc]] %||% ""), na.rm = TRUE)) {
      out[, (cc) := NULL]
    }
  }
  data.table::fwrite(out, path, bom = TRUE)
  invisible(TRUE)
}

persist_shock_result <- function(result, root = find_project_root()) {
  if (!isTRUE(result$ok)) {
    stop("Cannot persist failed shock result: ",
         paste(result$errors %||% result$validation$errors, collapse = "; "),
         call. = FALSE)
  }
  out_dir <- safe_shock_result_dir(result$scenario, root)
  sc <- result$scenario

  jsonlite::write_json(
    sc,
    file.path(out_dir, "scenario_definition.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  manifest <- list(
    scenario_id = sc$scenario_id,
    scenario_name = sc$scenario_name,
    scenario_hash = result$scenario_hash,
    result_hash = result$result_hash,
    engine_version = result$engine_version,
    universe_version = sc$universe_version,
    production_status = result$production_status,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    methodology_notice = result$methodology_notice,
    partial_notice = result$partial_notice,
    reconciliation_ok = isTRUE(result$reconciliation$ok),
    n_edge_impacts = nrow(result$edge_impacts),
    n_reporter_impacts = nrow(result$reporter_impacts),
    n_paths = nrow(result$impact_paths),
    residual_unmet_value_usd = sum(result$edge_impacts$residual_unmet_value_usd, na.rm = TRUE),
    direct_disrupted_value_usd = sum(result$edge_impacts$direct_disrupted_value_usd, na.rm = TRUE),
    substitution_allocated_value_usd = sum(result$edge_impacts$substitution_allocated_usd, na.rm = TRUE)
  )
  jsonlite::write_json(
    manifest,
    file.path(out_dir, "scenario_manifest.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )

  write_out <- function(dt, name) {
    pq <- file.path(out_dir, paste0(name, ".parquet"))
    csv <- file.path(out_dir, paste0(name, ".csv"))
    if (requireNamespace("arrow", quietly = TRUE)) {
      arrow::write_parquet(data.table::as.data.table(dt), pq)
    } else {
      write_shock_table(dt, csv)
    }
  }
  write_out(result$baseline_targets, "baseline_targets")
  write_out(result$edge_impacts, "edge_impacts")
  write_out(result$reporter_impacts, "reporter_impacts")
  write_out(result$commodity_impacts, "commodity_impacts")
  write_out(result$supplier_impacts, "supplier_impacts")
  write_out(result$post_shock_dependency, "post_shock_dependency")
  write_out(result$impact_paths, "impact_paths")
  write_out(result$diagnostics, "scenario_diagnostics")
  write_out(
    data.table::data.table(
      check = names(result$reconciliation),
      value = unname(unlist(lapply(result$reconciliation, as.character)))
    ),
    "scenario_validation"
  )

  list(result_dir = out_dir, manifest = manifest)
}
