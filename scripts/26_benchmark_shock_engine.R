options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)

cfg <- normalise_performance_config()
app_cfg <- load_config()
if (!file.exists(file.path(performance_paths(cfg)$fixtures, "synthetic_detailed.parquet"))) {
  write_performance_fixtures(cfg)
}
runs <- run_shock_benchmark_suite(cfg, app_cfg)
paths <- ensure_performance_dirs(cfg)
atomic_write_parquet_dt(runs, file.path(paths$results, paste0("shock_", cfg$phase_label, ".parquet")))
claim <- claim_250ms_supported(runs)
write_json_atomic(claim, file.path(paths$results, paste0("shock_claim_", cfg$phase_label, ".json")))
cat(
  "SHOCK_BENCH_OK n=", nrow(runs),
  " claim250=", claim$supported,
  " p95=", claim$statistic, "\n", sep = ""
)
