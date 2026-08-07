options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)
for (f in c(
  "shock_formatters.R", "shock_scenario.R", "shock_validation.R",
  "shock_direct.R", "shock_substitution.R", "shock_propagation.R",
  "shock_aggregation.R", "shock_comparison.R", "shock_diagnostics.R",
  "shock_downloads.R"
)) {
  source(file.path(root, "R", f), local = FALSE)
}

cfg <- load_config()
ensure_data_dirs(cfg)
ensure_shock_scenario_dirs(root)

scenario_file <- Sys.getenv("GTSC_SCENARIO_FILE", unset = "")
validate_only <- identical(tolower(Sys.getenv("GTSC_VALIDATE_SCENARIO_ONLY", "")), "true")
force_stale <- identical(tolower(Sys.getenv("GTSC_FORCE_STALE_UNIVERSE", "")), "true")

if (!nzchar(scenario_file)) {
  stop("Set GTSC_SCENARIO_FILE to a scenario JSON path.", call. = FALSE)
}
if (!file.exists(scenario_file)) {
  stop("Scenario file not found: ", scenario_file, call. = FALSE)
}

scenario <- read_shock_scenario_file(scenario_file)
pm <- Sys.getenv("GTSC_PROPAGATION_MODE", unset = "")
if (nzchar(pm)) scenario$propagation_mode <- pm
mps <- Sys.getenv("GTSC_MAX_PROPAGATION_STEPS", unset = "")
if (nzchar(mps)) scenario$maximum_propagation_steps <- as.integer(mps)

snap <- load_processed_snapshot(cfg)
coverage <- snap$detailed_coverage %||% trade_flow_coverage_status(snap)
detailed <- snap$trade_detailed_enriched %||% snap$trade_detailed
if (is.null(detailed) || !nrow(detailed)) {
  stop("No detailed trade data available for shock baseline.", call. = FALSE)
}

if (!validate_only && identical(coverage$production_status, "partial")) {
  scenario$acknowledge_partial_coverage <- TRUE
}
scenario$universe_version <- coverage$universe_checksum %||% scenario$universe_version

result <- run_shock_scenario(
  detailed,
  scenario,
  coverage = coverage,
  validate_only = validate_only,
  force_stale_universe = force_stale
)

if (!isTRUE(result$ok)) {
  cat("SCENARIO_FAILED\n")
  cat(paste(result$errors %||% result$validation$errors, collapse = "\n"), "\n")
  quit(status = 1)
}

if (validate_only) {
  cat("SCENARIO_VALID\n")
  cat("scenario_id=", result$scenario$scenario_id, "\n", sep = "")
  cat("scenario_hash=", result$validation$scenario_hash, "\n", sep = "")
  quit(status = 0)
}

persisted <- persist_shock_result(result, root)
cat("SCENARIO_OK\n")
cat("scenario_id=", result$scenario$scenario_id, "\n", sep = "")
cat("result_dir=", persisted$result_dir, "\n", sep = "")
cat("direct=", sum(result$edge_impacts$direct_disrupted_value_usd, na.rm = TRUE), "\n", sep = "")
cat("substitution=", sum(result$edge_impacts$substitution_allocated_usd, na.rm = TRUE), "\n", sep = "")
cat("residual=", sum(result$edge_impacts$residual_unmet_value_usd, na.rm = TRUE), "\n", sep = "")
cat("reporters=", nrow(result$reporter_impacts), "\n", sep = "")
cat("paths=", nrow(result$impact_paths), "\n", sep = "")
cat("recon_ok=", isTRUE(result$reconciliation$ok), "\n", sep = "")
