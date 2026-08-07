if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R"); source("R/pipeline_state.R")
source("R/validation.R"); source("R/wdi_client.R")
source("R/wdi_request_planner.R"); source("R/wdi_production_pipeline.R")
source("R/macro_country_universe.R"); source("R/macro_enrichment.R")
source("R/macro_validation.R")

cfg <- load_config(); setwd(cfg$project_root)
cat("=== Phase 3: enrich trade data ===\n")

proc <- cfg[['paths']]$processed
trade_global <- data.table::as.data.table(arrow::read_parquet(file.path(proc, "trade_global_hs85_annual.parquet")))
trade_detailed <- data.table::as.data.table(arrow::read_parquet(file.path(proc, "trade_detailed_top20.parquet")))
wdi_wide <- data.table::as.data.table(arrow::read_parquet(file.path(proc, "wdi_production_wide.parquet")))
wdi_long <- data.table::as.data.table(arrow::read_parquet(file.path(proc, "wdi_production_long.parquet")))
universe <- data.table::as.data.table(arrow::read_parquet(file.path(proc, "macro_country_universe.parquet")))

man <- safe_read_json(file.path(proc, "production_pipeline_manifest.json"))
production_status <- man$production_status %||% "partial"
universe_checksum <- if ("universe_checksum" %in% names(trade_detailed)) {
  trade_detailed$universe_checksum[1]
} else {
  man$universe_version %||% NA_character_
}

g <- enrich_trade_global(trade_global, wdi_wide)
atomic_write_parquet_dt(g$data, file.path(proc, "trade_global_enriched.parquet"))
cat(sprintf("GLOBAL_ENRICHED before=%d after=%d\n", g$n_before, g$n_after))

d <- enrich_trade_detailed(
  trade_detailed, wdi_wide,
  universe_checksum = universe_checksum,
  production_status = production_status
)
atomic_write_parquet_dt(d$data, file.path(proc, "trade_detailed_enriched.parquet"))
cat(sprintf("DETAILED_ENRICHED before=%d after=%d status=%s\n",
            d$n_before, d$n_after, production_status))

cy <- build_country_year_analytics(trade_global, wdi_wide)
atomic_write_parquet_dt(cy, file.path(proc, "country_year_analytics.parquet"))
cat(sprintf("COUNTRY_YEAR rows=%d countries=%d\n",
            nrow(cy), data.table::uniqueN(cy$reporter_iso3)))

rep_cov <- join_coverage(trade_global, wdi_wide, "reporter_iso3")
par_cov <- join_coverage(trade_detailed, wdi_wide, "partner_iso3")
rep_det_cov <- join_coverage(trade_detailed, wdi_wide, "reporter_iso3")

profile_path <- file.path(proc, "macro_data_profile.json")
profile <- if (file.exists(profile_path)) safe_read_json(profile_path) else list()
profile$country_year_analytical_row_count <- nrow(cy)
profile$global_enriched_row_count <- nrow(g$data)
profile$detailed_enriched_row_count <- nrow(d$data)
profile$reporter_macro_join_coverage <- rep_cov
profile$partner_macro_join_coverage <- par_cov
profile$detailed_reporter_macro_join_coverage <- rep_det_cov
profile$current_detailed_production_status <- production_status
profile$detailed_represented_reporters <- sort(unique(as.character(trade_detailed$reporter_iso3)))
profile$universe_checksum <- universe_checksum
profile$generated_at <- utc_now()
write_json_atomic(profile, profile_path, pretty = TRUE)

val <- run_phase3_validation(
  cfg = cfg,
  universe = universe,
  wdi_long = wdi_long,
  wdi_wide = wdi_wide,
  trade_global = trade_global,
  trade_global_enriched = g$data,
  trade_detailed = trade_detailed,
  trade_detailed_enriched = d$data,
  country_year = cy,
  production_status = production_status,
  universe_checksum = universe_checksum
)
atomic_write_parquet_dt(val, file.path(proc, "phase3_validation_results.parquet"))
vs <- summarise_validation(val)
cat(sprintf("PHASE3_VALIDATION pass=%d warn=%d error=%d\n",
            vs$n_pass, vs$n_warning, vs$n_error))
quit(status = if (vs$n_error > 0) 1 else 0)
