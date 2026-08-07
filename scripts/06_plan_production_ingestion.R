if (file.exists("renv/activate.R")) source("renv/activate.R")

source("R/utilities.R")
source("R/config.R")
source("R/comtrade_client.R")
source("R/comtrade_reporters.R")
source("R/comtrade_availability.R")
source("R/comtrade_request_planner.R")
source("R/pipeline_state.R")
source("R/plan_migration.R")

cfg <- load_config()
setwd(cfg[['project_root']])
years <- 2019:2024
classification <- "HS"
request_delay <- suppressWarnings(as.numeric(Sys.getenv("GTSC_REQUEST_DELAY_SECONDS", "1.1")))
if (is.na(request_delay)) request_delay <- 1.1
refresh_ref <- tolower(Sys.getenv("GTSC_REFRESH_RAW", "false")) %in% c("1", "true", "yes")

cat("=== Phase 2: plan production ingestion (corrected) ===\n")

mig <- migrate_invalid_reporter_zero_state(cfg)
cat(sprintf("MIGRATION invalid_count=%d\n", length(mig$invalid_ids)))

reporters_raw <- fetch_comtrade_reporters_reference(cfg, refresh = refresh_ref)
filt <- filter_eligible_reporters(reporters_raw, years = years)
atomic_write_parquet_dt(filt$eligible, file.path(cfg[['paths']]$reference, "comtrade_reporters_eligible.parquet"))
write_reporter_exclusion_diagnostics(filt$excluded, cfg)
arrow::write_parquet(filt$excluded, file.path(cfg[['paths']]$interim, "comtrade_reporters_excluded.parquet"))
cat(sprintf("REPORTERS raw=%d eligible=%d excluded=%d\n",
            nrow(reporters_raw), nrow(filt$eligible), nrow(filt$excluded)))

avail <- fetch_comtrade_availability(cfg, classification = classification, years = years, refresh = refresh_ref)
inter <- intersect_reporters_with_availability(filt$eligible, avail, years = years)
arrow::write_parquet(inter$reporter_years, file.path(cfg[['paths']]$interim, "comtrade_reporter_year_availability.parquet"))
cat("AVAILABILITY by year:\n")
print(inter$coverage_by_year)

rank_choice <- choose_ranking_year(inter$coverage_by_year, prefer = 2024L)
cat(sprintf("RANKING_YEAR=%s n=%s rationale=%s\n",
            rank_choice$year, rank_choice$n_reporters, rank_choice$rationale))
write_json_atomic(rank_choice, file.path(cfg[['paths']]$interim, "ranking_year_choice.json"), pretty = TRUE)

rank_reporters <- unique(inter$reporter_years[year == rank_choice$year]$reporter_code)

all_available_reporters <- unique(inter$reporter_years$reporter_code)
cat(sprintf("ELIGIBLE ranking_year_reporters=%d all_available_reporters=%d\n",
            length(rank_reporters), length(all_available_reporters)))

ranking_plan <- build_ranking_year_global_plan(
  cfg, reporter_codes = rank_reporters, ranking_year = rank_choice$year,
  classification = classification
)
global_plan <- build_global_hs85_plan(
  cfg, reporter_codes = all_available_reporters, years = years,
  classification = classification
)

new_plan <- data.table::rbindlist(list(ranking_plan, global_plan), fill = TRUE)
new_plan <- unique(new_plan, by = "request_id")

plan_path <- request_plan_file(cfg)
if (file.exists(plan_path)) {
  existing <- data.table::as.data.table(arrow::read_parquet(plan_path))

  if ("plan_status" %in% names(existing)) {
    keep <- existing[plan_status == "superseded"]
  } else {
    keep <- existing[reporter_code == "0"]
    if (nrow(keep)) keep[, plan_status := "superseded"]
  }
  plan_dt <- data.table::rbindlist(list(keep, new_plan), fill = TRUE)
  plan_dt <- unique(plan_dt, by = "request_id")
} else {
  plan_dt <- new_plan
}
atomic_write_parquet_dt(plan_dt, plan_path)

st <- load_state(cfg)
st <- init_state_from_plan(new_plan, existing_state = st)
save_state(st, cfg)

summary <- summarise_request_plan(plan_dt, request_delay_seconds = request_delay)
summary$classification <- classification
summary$classification_decision <- paste(
  "Path classification HS (original reported). H5/H6 path probes returned 0 rows for known-valid USA World controls;",
  "availability shows H5 sparse in 2024 and H6 absent in 2019. Store classificationCode per row; do not claim converted H5."
)
summary$ranking_year <- rank_choice
summary$eligible_reporters_ranking_year <- length(rank_reporters)
summary$eligible_reporters_any_year <- length(all_available_reporters)
summary$eligible_reporter_years <- nrow(inter$reporter_years)
summary$active_global_requests <- nrow(global_plan)
summary$active_ranking_requests <- nrow(ranking_plan)
write_json_atomic(summary, request_plan_summary_file(cfg), pretty = TRUE)

cat(sprintf(
  "PLAN_OK active=%d ranking=%d global=%d superseded=%d delay=%.1fs est_min_runtime_s=%.1f\n",
  summary$active_request_count, nrow(ranking_plan), nrow(global_plan),
  summary$superseded_request_count, request_delay, summary$estimated_minimum_runtime_seconds
))
quit(status = 0)
