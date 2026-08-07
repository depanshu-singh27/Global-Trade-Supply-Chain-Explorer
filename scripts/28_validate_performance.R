options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)

cfg <- normalise_performance_config()
paths <- performance_paths(cfg)
phase <- cfg$phase_label

collect <- function(prefix) {
  f <- file.path(paths$results, paste0(prefix, "_", phase, ".parquet"))
  safe_read_parquet_dt(f)
}
runs <- data.table::rbindlist(list(
  collect("analytics"),
  collect("shock"),
  collect("shiny")
), fill = TRUE)

snap_prof <- safe_read_parquet_dt(file.path(paths$profiles, "snapshot_profile.parquet"))
if (!is.null(snap_prof) && nrow(snap_prof)) {
  snap_prof[, phase := phase]
  runs <- data.table::rbindlist(list(runs, snap_prof), fill = TRUE)
}

app_cfg <- load_config()
snap <- load_processed_snapshot(app_cfg)
mem <- profile_snapshot_memory(snap)
react <- profile_reactive_invalidations(cfg)
payload <- profile_payload_sizes(dt_rows = nrow(snap$trade_detailed_enriched %||% data.table::data.table()))
validation <- validate_performance_results(runs, cfg, snap$detailed_coverage)

shock_rows <- runs[module == "shock_engine"]
claim <- claim_250ms_supported(shock_rows)

base <- safe_read_parquet_dt(file.path(paths$results, "benchmark_runs_baseline.parquet"))
opt <- if (identical(phase, "optimised")) runs else {
  safe_read_parquet_dt(file.path(paths$results, "benchmark_runs_optimised.parquet"))
}
comparison <- if (!is.null(base) && nrow(base) && !is.null(opt) && nrow(opt)) {
  compare_benchmark_phases(base, opt)
} else {
  data.table::data.table()
}

persist_performance_outputs(
  runs, mem, react, payload, validation, comparison, cfg = cfg, claim_250 = claim
)
cat("PERF_VALIDATE_OK checks=", nrow(validation), " claim250=", claim$supported, "\n", sep = "")
