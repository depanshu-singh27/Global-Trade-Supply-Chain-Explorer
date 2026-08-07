if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R"); source("R/pipeline_state.R")
source("R/comtrade_request_planner.R"); source("R/wdi_client.R")
source("R/wdi_request_planner.R"); source("R/wdi_production_pipeline.R")
source("R/macro_country_universe.R"); source("R/macro_enrichment.R")

cfg <- load_config(); setwd(cfg$project_root)
cat("=== Phase 3: build macro data ===\n")

universe <- data.table::as.data.table(arrow::read_parquet(
  file.path(cfg[['paths']]$processed, "macro_country_universe.parquet")
))
included <- universe[included == TRUE]
plan <- data.table::as.data.table(arrow::read_parquet(wdi_plan_file(cfg)))

assembled <- assemble_wdi_production_long(cfg, plan_dt = plan, allowed_iso3 = included$iso3)
long_dt <- assembled$long
if (nrow(assembled$conflicts)) {
  atomic_write_parquet_dt(
    assembled$conflicts,
    file.path(cfg[['paths']]$interim, "wdi_production_conflicts.parquet")
  )
  cat("WDI_CONFLICTS rows=", nrow(assembled$conflicts), "\n", sep = "")
}

wide_dt <- wdi_long_to_wide_production(long_dt)
atomic_write_parquet_dt(long_dt, file.path(cfg[['paths']]$processed, "wdi_production_long.parquet"))
atomic_write_parquet_dt(wide_dt, file.path(cfg[['paths']]$processed, "wdi_production_wide.parquet"))

coverage <- build_macro_coverage_report(long_dt, universe)
atomic_write_parquet_dt(coverage, file.path(cfg[['paths']]$processed, "macro_coverage_report.parquet"))

man <- safe_read_json(file.path(cfg[['paths']]$processed, "production_pipeline_manifest.json"))
profile <- list(
  generated_at = utc_now(),
  country_universe_size = nrow(included),
  country_count_by_source_scope = as.list(table(included$source_scope)),
  year_range = if (nrow(long_dt)) range(long_dt$year, na.rm = TRUE) else c(NA, NA),
  indicator_list = sort(unique(as.character(long_dt$indicator_code))),
  wdi_long_row_count = nrow(long_dt),
  wdi_wide_row_count = nrow(wide_dt),
  missing_values_by_indicator = as.list(long_dt[, .(n_missing = sum(is.na(value))), by = indicator_code][
    , setNames(n_missing, indicator_code)]),
  duplicate_exact_removed = assembled$duplicate_exact,
  conflict_rows = nrow(assembled$conflicts),
  current_detailed_production_status = man$production_status %||% "partial",
  universe_checksum = man$universe_version %||% NA_character_,
  note = "CPI retained as context only; not used to invent real-USD trade"
)
write_json_atomic(profile, file.path(cfg[['paths']]$processed, "macro_data_profile.json"), pretty = TRUE)

cat(sprintf("MACRO_BUILD_OK long=%d wide=%d coverage_rows=%d\n",
            nrow(long_dt), nrow(wide_dt), nrow(coverage)))
quit(status = 0)
