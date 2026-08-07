if (file.exists("renv/activate.R")) source("renv/activate.R")

source("R/utilities.R")
source("R/config.R")
source("R/pipeline_state.R")
source("R/validation.R")
source("R/comtrade_request_planner.R")
source("R/universe_selection.R")

cfg <- load_config()
setwd(cfg[['project_root']])
cat("=== Phase 2: build production outputs + validation ===\n")

trade_global_path <- file.path(cfg[['paths']]$processed, "trade_global_hs85_annual.parquet")
trade_detailed_path <- file.path(cfg[['paths']]$processed, "trade_detailed_top20.parquet")
if (!file.exists(trade_global_path)) stop("Missing trade_global_hs85_annual.parquet", call. = FALSE)
if (!file.exists(trade_detailed_path)) stop("Missing trade_detailed_top20.parquet", call. = FALSE)

trade_global <- data.table::as.data.table(arrow::read_parquet(trade_global_path))
trade_detailed <- data.table::as.data.table(arrow::read_parquet(trade_detailed_path))
top_reporters <- data.table::as.data.table(arrow::read_parquet(file.path(cfg[['paths']]$processed, "top_reporters.parquet")))
top_partners <- data.table::as.data.table(arrow::read_parquet(file.path(cfg[['paths']]$processed, "top_partners.parquet")))
top_hs4 <- data.table::as.data.table(arrow::read_parquet(file.path(cfg[['paths']]$processed, "top_hs4.parquet")))
st <- load_state(cfg)
if (is.null(st) || !nrow(st)) stop("Missing pipeline state.", call. = FALSE)

universe_path <- file.path(cfg[['paths']]$processed, "analytical_universe.json")
universe_obj <- if (file.exists(universe_path)) safe_read_json(universe_path) else NULL
universe_checksum <- top_reporters$universe_checksum[1] %||%
  universe_obj$universe_checksum %||% universe_obj$universe_version %||% NA_character_

plan <- if (file.exists(request_plan_file(cfg))) {
  data.table::as.data.table(arrow::read_parquet(request_plan_file(cfg)))
} else {
  NULL
}
active_det <- if (!is.null(plan) && nrow(plan)) {
  plan[dataset_type == "trade_detailed_top20" & (is.na(plan_status) | plan_status == "active")]
} else {
  data.table::data.table()
}
st_det <- if (nrow(active_det)) st[request_id %in% active_det$request_id] else data.table::data.table()

selected_reporter_count <- nrow(top_reporters)
represented_reporters <- sort(unique(as.character(trade_detailed$reporter_code)))
missing_reporters <- setdiff(as.character(top_reporters$reporter_code), represented_reporters)

count_status <- function(dt, statuses) {
  if (!nrow(dt)) return(0L)
  as.integer(nrow(dt[status %in% statuses]))
}

active_request_count <- nrow(active_det)
succeeded_request_count <- count_status(st_det, "succeeded")
valid_empty_request_count <- count_status(st_det, "empty")
skipped_cached_count <- count_status(st_det, "skipped_cached")
planned_request_count <- count_status(st_det, "planned")
running_request_count <- count_status(st_det, "running")
quota_blocked_count <- count_status(st_det, "quota_blocked")
retryable_failed_count <- count_status(st_det, "retryable_failed")
permanently_failed_count <- count_status(st_det, "permanently_failed")
terminal_request_count <- succeeded_request_count + valid_empty_request_count + skipped_cached_count
incomplete_remaining <- planned_request_count + running_request_count +
  quota_blocked_count + retryable_failed_count

declared_complete <- tolower(Sys.getenv("GTSC_DECLARE_COMPLETE", "false")) %in% c("1", "true", "yes")

detailed_checksum <- if ("universe_checksum" %in% names(trade_detailed) && nrow(trade_detailed)) {
  unique(as.character(trade_detailed$universe_checksum))
} else {
  NA_character_
}
checksum_mismatch <- !is.na(universe_checksum) && nzchar(universe_checksum) &&
  (length(detailed_checksum) != 1L || is.na(detailed_checksum[1]) ||
     !identical(detailed_checksum[1], as.character(universe_checksum)))
stale_detailed <- isTRUE(checksum_mismatch) ||
  any(trade_detailed$reporter_iso3 %in% c("EUR", "WLD", "W00", "ASE"), na.rm = TRUE)

aggregate_in_universe <- any(top_reporters$reporter_iso3 %in% c("EUR", "WLD", "W00", "ASE"), na.rm = TRUE) ||
  ("reporter_entity_type" %in% names(top_reporters) &&
     any(top_reporters$reporter_entity_type != "country_or_economy", na.rm = TRUE))

results <- list()
results[[length(results) + 1L]] <- .validation_row(
  "request_no_running", "production_pipeline_state",
  if (running_request_count > 0) "warning" else "pass",
  "No running requests remain",
  running_request_count
)
results[[length(results) + 1L]] <- .validation_row(
  "no_active_reporter_zero", "production_request_plan",
  {
    if (is.null(plan)) "warning" else {
      active <- if ("plan_status" %in% names(plan)) plan[plan_status == "active"] else plan
      if (any(active$reporter_code == "0", na.rm = TRUE)) "error" else "pass"
    }
  },
  "No active final-data requests use reporterCode=0",
  0L
)

expected_cols <- c(
  "year", "period", "frequency", "reporter_code", "reporter_iso3", "reporter_name",
  "flow_code", "flow_name", "partner_code", "partner_iso3", "partner_name",
  "hs_revision", "hs_code", "hs_level", "commodity_description",
  "trade_value_usd", "net_weight_kg", "quantity", "quantity_unit",
  "source_updated_at", "ingested_at", "request_id"
)
results[[length(results) + 1L]] <- validate_required_columns(trade_global, expected_cols, "trade_global_hs85_annual")
results[[length(results) + 1L]] <- validate_year_range(trade_global$year, 2019, 2024, "trade_global_hs85_annual")
results[[length(results) + 1L]] <- validate_non_negative(trade_global$trade_value_usd, "trade_global_hs85_annual")
results[[length(results) + 1L]] <- validate_hs_character(trade_global$hs_code, "trade_global_hs85_annual")
results[[length(results) + 1L]] <- .validation_row(
  "global_hs_chapter_85", "trade_global_hs85_annual",
  if (all(trade_global$hs_code == "85")) "pass" else "error",
  "HS chapter is 85",
  sum(trade_global$hs_code != "85", na.rm = TRUE)
)
results[[length(results) + 1L]] <- .validation_row(
  "global_world_partner", "trade_global_hs85_annual",
  if (all(trade_global$partner_iso3 == "W00" | trade_global$partner_code == "0")) "pass" else "warning",
  "World partner identified",
  sum(!(trade_global$partner_iso3 == "W00" | trade_global$partner_code == "0"), na.rm = TRUE)
)
results[[length(results) + 1L]] <- validate_unique_keys(
  trade_global, c("year", "reporter_code", "flow_code", "hs_code"),
  "trade_global_hs85_annual", "global_unique_keys"
)
results[[length(results) + 1L]] <- .validation_row(
  "global_nonempty", "trade_global_hs85_annual",
  if (nrow(trade_global) > 0) "pass" else "error",
  sprintf("rows=%d", nrow(trade_global)), nrow(trade_global)
)

results[[length(results) + 1L]] <- validate_required_columns(trade_detailed, expected_cols, "trade_detailed_top20")
results[[length(results) + 1L]] <- validate_year_range(trade_detailed$year, 2019, 2024, "trade_detailed_top20")
results[[length(results) + 1L]] <- validate_non_negative(trade_detailed$trade_value_usd, "trade_detailed_top20")
results[[length(results) + 1L]] <- validate_hs_character(trade_detailed$hs_code, "trade_detailed_top20")
results[[length(results) + 1L]] <- .validation_row(
  "detailed_hs4_length_and_prefix", "trade_detailed_top20",
  if (!nrow(trade_detailed) || all(nchar(trade_detailed$hs_code) == 4 & substr(trade_detailed$hs_code, 1, 2) == "85")) "pass" else "error",
  "HS4 codes length 4 starting with 85",
  sum(!(nchar(trade_detailed$hs_code) == 4 & substr(trade_detailed$hs_code, 1, 2) == "85"), na.rm = TRUE)
)
results[[length(results) + 1L]] <- validate_unique_keys(
  trade_detailed, c("year", "flow_code", "reporter_code", "partner_code", "hs_code"),
  "trade_detailed_top20", "detailed_unique_keys"
)
results[[length(results) + 1L]] <- .validation_row(
  "detailed_reporter_universe", "trade_detailed_top20",
  if (!nrow(trade_detailed) || all(trade_detailed$reporter_code %in% as.character(top_reporters$reporter_code))) "pass" else "error",
  "Reporters within top-20 universe",
  sum(!(trade_detailed$reporter_code %in% as.character(top_reporters$reporter_code)), na.rm = TRUE)
)
results[[length(results) + 1L]] <- .validation_row(
  "detailed_partner_universe", "trade_detailed_top20",
  if (!nrow(trade_detailed) || all(trade_detailed$partner_code %in% as.character(top_partners$partner_code))) "pass" else "error",
  "Partners within top-20 universe",
  sum(!(trade_detailed$partner_code %in% as.character(top_partners$partner_code)), na.rm = TRUE)
)
results[[length(results) + 1L]] <- .validation_row(
  "detailed_hs4_universe", "trade_detailed_top20",
  if (!nrow(trade_detailed) || all(trade_detailed$hs_code %in% as.character(top_hs4$hs_code))) "pass" else "error",
  "HS4 within top-20 universe",
  sum(!(trade_detailed$hs_code %in% as.character(top_hs4$hs_code)), na.rm = TRUE)
)
results[[length(results) + 1L]] <- .validation_row(
  "detailed_no_world_partner", "trade_detailed_top20",
  if (!any(trade_detailed$partner_iso3 == "W00", na.rm = TRUE)) "pass" else "warning",
  "No World partners in detailed cube",
  sum(trade_detailed$partner_iso3 == "W00", na.rm = TRUE)
)
results[[length(results) + 1L]] <- .validation_row(
  "classification_recorded", "trade_global_hs85_annual",
  if (all(!is.na(trade_global$hs_revision) & nzchar(trade_global$hs_revision))) "pass" else "warning",
  sprintf("Distinct hs_revision values: %s", paste(unique(trade_global$hs_revision), collapse = ",")),
  data.table::uniqueN(trade_global$hs_revision)
)

results[[length(results) + 1L]] <- .validation_row(
  "selected_reporters_count", "analytical_universe",
  if (selected_reporter_count == 20L) "pass" else "error",
  sprintf("selected_reporter_count=%d", selected_reporter_count),
  selected_reporter_count
)
results[[length(results) + 1L]] <- .validation_row(
  "no_aggregate_in_reporter_universe", "analytical_universe",
  if (aggregate_in_universe) "error" else "pass",
  "No aggregate/special reporters in country universe",
  as.integer(aggregate_in_universe)
)
results[[length(results) + 1L]] <- .validation_row(
  "no_aggregate_in_detailed_output", "trade_detailed_top20",
  if (any(trade_detailed$reporter_iso3 %in% c("EUR", "WLD", "W00", "ASE"), na.rm = TRUE)) "error" else "pass",
  "No aggregate reporters in detailed output",
  sum(trade_detailed$reporter_iso3 %in% c("EUR", "WLD", "W00", "ASE"), na.rm = TRUE)
)
results[[length(results) + 1L]] <- .validation_row(
  "universe_checksum_match", "trade_detailed_top20",
  if (stale_detailed) "error" else "pass",
  sprintf("detailed_checksum=%s current=%s", paste(detailed_checksum, collapse = ","), universe_checksum),
  as.integer(stale_detailed)
)
results[[length(results) + 1L]] <- .validation_row(
  "active_detailed_request_count", "production_request_plan",
  if (active_request_count == 120L) "pass" else "warning",
  sprintf("active_detailed=%d (expected 120)", active_request_count),
  active_request_count
)
results[[length(results) + 1L]] <- .validation_row(
  "incomplete_execution", "trade_detailed_top20",
  if (incomplete_remaining > 0) "warning" else "pass",
  sprintf("remaining planned/running/quota/retryable=%d", incomplete_remaining),
  incomplete_remaining
)
results[[length(results) + 1L]] <- .validation_row(
  "missing_selected_reporters", "trade_detailed_top20",
  {
    if (length(missing_reporters) == 0L) {
      "pass"
    } else if (declared_complete || (incomplete_remaining == 0L && permanently_failed_count == 0L)) {
      "error"
    } else {
      "warning"
    }
  },
  sprintf("missing=%s represented=%d/%d",
          paste(missing_reporters, collapse = ","),
          length(represented_reporters), selected_reporter_count),
  length(missing_reporters)
)
results[[length(results) + 1L]] <- .validation_row(
  "quota_blocked_requests", "production_pipeline_state",
  if (quota_blocked_count > 0) "warning" else "pass",
  sprintf("quota_blocked=%d", quota_blocked_count),
  quota_blocked_count
)

if (length(represented_reporters)) {
  year_cov <- trade_detailed[reporter_code %in% represented_reporters,
                             .(n_years = data.table::uniqueN(year)), by = reporter_code]
  incomplete_years <- year_cov[n_years < 6]
  results[[length(results) + 1L]] <- .validation_row(
    "detailed_year_coverage_represented", "trade_detailed_top20",
    if (nrow(incomplete_years) && incomplete_remaining > 0) "warning" else if (nrow(incomplete_years) && declared_complete) "error" else "pass",
    sprintf("reporters_with_<6_years=%d", nrow(incomplete_years)),
    nrow(incomplete_years)
  )
}

validation_dt <- data.table::rbindlist(Filter(function(x) {
  is.data.frame(x) || data.table::is.data.table(x)
}, results), fill = TRUE)
atomic_write_parquet_dt(validation_dt, file.path(cfg[['paths']]$processed, "production_validation_results.parquet"))

global_cov <- trade_global[, .(
  reporter_count = data.table::uniqueN(reporter_iso3),
  partner_count = data.table::uniqueN(partner_iso3),
  hs_code_count = data.table::uniqueN(hs_code),
  flow_count = data.table::uniqueN(flow_code),
  row_count = .N,
  total_trade_value = sum(trade_value_usd, na.rm = TRUE)
), by = year]
global_cov[, dataset := "global_hs85_annual"]
detailed_cov <- if (nrow(trade_detailed)) {
  trade_detailed[, .(
    reporter_count = data.table::uniqueN(reporter_iso3),
    partner_count = data.table::uniqueN(partner_iso3),
    hs_code_count = data.table::uniqueN(hs_code),
    flow_count = data.table::uniqueN(flow_code),
    row_count = .N,
    total_trade_value = sum(trade_value_usd, na.rm = TRUE)
  ), by = year]
} else {
  data.table::data.table(year = integer(), reporter_count = integer(), partner_count = integer(),
                         hs_code_count = integer(), flow_count = integer(), row_count = integer(),
                         total_trade_value = numeric())
}
if (nrow(detailed_cov)) detailed_cov[, dataset := "detailed_top20"]
coverage_dt <- data.table::rbindlist(list(global_cov, detailed_cov), fill = TRUE)
coverage_dt[, `:=`(
  successful_request_count = terminal_request_count,
  empty_request_count = valid_empty_request_count,
  failed_request_count = retryable_failed_count + permanently_failed_count + quota_blocked_count,
  invalid_request_count = nrow(st[status == "invalid"]),
  generated_at = utc_now()
)]
atomic_write_parquet_dt(coverage_dt, file.path(cfg[['paths']]$processed, "trade_coverage_report.parquet"))

vs <- summarise_validation(validation_dt)

production_status <- if (vs$n_error > 0 && aggregate_in_universe) {
  "failed"
} else if (quota_blocked_count > 0 && incomplete_remaining > 0) {
  "blocked_quota"
} else if (incomplete_remaining > 0 || length(missing_reporters) > 0 ||
           terminal_request_count < active_request_count) {
  "partial"
} else if (active_request_count == 120L && length(missing_reporters) == 0L &&
           terminal_request_count >= active_request_count && !stale_detailed) {
  "complete"
} else {
  "partial"
}

status_block <- list(
  production_status = production_status,
  active_request_count = active_request_count,
  terminal_request_count = terminal_request_count,
  succeeded_request_count = succeeded_request_count,
  valid_empty_request_count = valid_empty_request_count,
  skipped_cached_count = skipped_cached_count,
  planned_request_count = planned_request_count,
  running_request_count = running_request_count,
  quota_blocked_count = quota_blocked_count,
  retryable_failed_count = retryable_failed_count,
  permanently_failed_count = permanently_failed_count,
  selected_reporter_count = selected_reporter_count,
  represented_reporter_count = length(represented_reporters),
  missing_reporters = as.list(missing_reporters),
  universe_version = universe_checksum,
  generated_at = utc_now()
)

profile <- list(
  generated_at = utc_now(),
  classification_path = "HS",
  classification_note = "Original reported classification via HS path; hs_revision stored per row",
  production_status = production_status,
  universe_version = universe_checksum,
  status = status_block,
  trade_global = list(
    file = basename(trade_global_path), rows = nrow(trade_global), cols = ncol(trade_global),
    year_range = range(trade_global$year, na.rm = TRUE),
    reporters = data.table::uniqueN(trade_global$reporter_code),
    flows = sort(unique(trade_global$flow_code)),
    hs_revisions = sort(unique(as.character(trade_global$hs_revision)))
  ),
  trade_detailed = list(
    file = basename(trade_detailed_path), rows = nrow(trade_detailed), cols = ncol(trade_detailed),
    year_range = if (nrow(trade_detailed)) range(trade_detailed$year, na.rm = TRUE) else c(NA, NA),
    reporters = data.table::uniqueN(trade_detailed$reporter_code),
    partners = data.table::uniqueN(trade_detailed$partner_code),
    hs4 = data.table::uniqueN(trade_detailed$hs_code),
    universe_checksum = detailed_checksum
  ),
  validation = vs
)
write_json_atomic(profile, file.path(cfg[['paths']]$processed, "trade_data_profile.json"), pretty = TRUE)

manifest <- c(
  list(generated_at = utc_now(), request_counts = as.list(table(st$status)), no_secrets = TRUE),
  status_block
)
write_json_atomic(manifest, file.path(cfg[['paths']]$processed, "production_pipeline_manifest.json"), pretty = TRUE)

cat(sprintf(
  "PRODUCTION_OUTPUTS_OK status=%s pass=%d warn=%d error=%d represented=%d/%d\n",
  production_status, vs$n_pass, vs$n_warning, vs$n_error,
  length(represented_reporters), selected_reporter_count
))

quit(status = if (vs$n_error > 0) 1 else 0)
