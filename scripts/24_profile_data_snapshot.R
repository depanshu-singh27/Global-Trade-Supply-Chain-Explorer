options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)

cfg <- normalise_performance_config()
app_cfg <- load_config()
env <- capture_benchmark_environment(cfg)
paths <- ensure_performance_dirs(cfg)

rows <- data.table::rbindlist(list(
  benchmark_snapshot_load(cfg, env, app_cfg, cold = TRUE),
  benchmark_snapshot_load(cfg, env, app_cfg, cold = FALSE),
  benchmark_geometry_load(cfg, env, app_cfg, rebuild = FALSE)
), fill = TRUE)

snap <- load_processed_snapshot(app_cfg)
mem <- profile_snapshot_memory(snap)
atomic_write_parquet_dt(rows, file.path(paths$profiles, "snapshot_profile.parquet"))
atomic_write_parquet_dt(mem, file.path(paths$profiles, "snapshot_memory.parquet"))
write_json_atomic(strip_unsafe_metadata_paths(list(
  generated_at = utc_now(),
  git_head = env$git_head,
  universe = cfg$universe_version,
  rows = nrow(snap$trade_detailed_enriched %||% data.table::data.table()),
  note = "Central snapshot profile; forecast models not executed."
)), file.path(paths$profiles, "snapshot_profile.json"))
cat("SNAPSHOT_PROFILE_OK rows=", nrow(rows), "\n", sep = "")
