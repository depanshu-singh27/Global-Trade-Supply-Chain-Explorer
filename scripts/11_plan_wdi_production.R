if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R"); source("R/pipeline_state.R")
source("R/comtrade_reporters.R"); source("R/comtrade_request_planner.R")
source("R/wdi_client.R"); source("R/wdi_request_planner.R")
source("R/wdi_production_pipeline.R"); source("R/macro_country_universe.R")

cfg <- load_config(); setwd(cfg$project_root)
ensure_data_dirs(cfg)
cat("=== Phase 3: plan WDI production ===\n")

universe <- build_macro_country_universe(cfg)

wb_countries <- fetch_wdi_country_reference(cfg, refresh = FALSE)
filt_wb <- filter_world_bank_iso3(universe[included == TRUE]$iso3, wb_countries)
universe[iso3 %in% filt_wb$unmatched & included == TRUE, `:=`(
  included = FALSE,
  exclusion_reason = "no_world_bank_country_match"
)]
persist_macro_country_universe(universe, cfg)
included <- universe[included == TRUE]
write_json_atomic(
  list(
    unmatched_world_bank = as.list(filt_wb$unmatched),
    matched_count = length(filt_wb$matched),
    generated_at = utc_now()
  ),
  file.path(cfg[['paths']]$interim, "wdi_unmatched_countries.json"),
  pretty = TRUE
)
cat(sprintf("MACRO_UNIVERSE included=%d excluded=%d unmatched_wb=%s\n",
            nrow(included), nrow(universe) - nrow(included),
            paste(filt_wb$unmatched, collapse = ",")))

plan <- build_wdi_production_plan(
  cfg, country_iso3 = included$iso3,
  start_year = 2019L, end_year = 2024L, chunk_size = 60L
)
atomic_write_parquet_dt(plan, wdi_plan_file(cfg))

delay <- suppressWarnings(as.numeric(Sys.getenv("GTSC_WDI_REQUEST_DELAY_SECONDS", "0.4")))
if (is.na(delay)) delay <- 0.4
summary <- summarise_wdi_plan(plan, request_delay_seconds = delay)
summary$included_countries <- nrow(included)
summary$unmatched_world_bank <- filt_wb$unmatched
write_json_atomic(summary, wdi_plan_summary_file(cfg), pretty = TRUE)

st <- init_wdi_state_from_plan(plan, load_wdi_state(cfg))
st[!(request_id %in% plan$request_id) &
     !(status %in% c("succeeded", "skipped_cached", "empty")),
   `:=`(status = "invalid", error_category = "replanned",
        error_message = "Superseded by WB-reconciled plan")]

st[request_id %in% plan$request_id &
     status %in% c("retryable_failed", "permanently_failed"),
   `:=`(status = "planned", error_category = NA_character_, error_message = NA_character_)]
save_wdi_state(st, cfg)

cat(sprintf("WDI_PLAN_OK requests=%d indicators=%s countries=%d\n",
            summary$active_request_count,
            paste(summary$indicators, collapse = ","),
            summary$country_count))
quit(status = 0)
