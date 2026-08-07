if (file.exists("renv/activate.R")) source("renv/activate.R")

source("R/utilities.R")
source("R/config.R")
source("R/comtrade_client.R")
source("R/comtrade_request_planner.R")
source("R/pipeline_state.R")
source("R/production_trade_pipeline.R")

cfg <- load_config()
setwd(cfg[['project_root']])

if (tolower(Sys.getenv("GTSC_QUOTA_PREFLIGHT", "false")) %in% c("1", "true", "yes")) {
  run_quota_preflight(cfg)
  quit(status = 0)
}

dry_run <- tolower(Sys.getenv("GTSC_DRY_RUN", "false")) %in% c("1", "true", "yes")
max_requests <- suppressWarnings(as.integer(Sys.getenv("GTSC_MAX_REQUESTS", "")))
if (is.na(max_requests)) max_requests <- Inf
retry_failed_only <- tolower(Sys.getenv("GTSC_RETRY_FAILED_ONLY", "false")) %in% c("1", "true", "yes")
refresh_raw <- tolower(Sys.getenv("GTSC_REFRESH_RAW", "false")) %in% c("1", "true", "yes")
request_delay <- suppressWarnings(as.numeric(Sys.getenv("GTSC_REQUEST_DELAY_SECONDS", "1.1")))
if (is.na(request_delay)) request_delay <- 1.1
years <- 2019:2024

cat("=== Phase 2: fetch detailed top-20 ===\n")

top_reporters <- data.table::as.data.table(arrow::read_parquet(file.path(cfg[['paths']]$processed, "top_reporters.parquet")))
top_partners <- data.table::as.data.table(arrow::read_parquet(file.path(cfg[['paths']]$processed, "top_partners.parquet")))
top_hs4 <- data.table::as.data.table(arrow::read_parquet(file.path(cfg[['paths']]$processed, "top_hs4.parquet")))
uv <- top_reporters$universe_checksum[1] %||% NA_character_
universe <- list(
  top_reporters = top_reporters, top_partners = top_partners, top_hs4 = top_hs4,
  universe_checksum = uv
)

plan_new <- build_detailed_top20_plan(
  cfg = cfg, universe = universe, years = years,
  classification = "HS", cmd_code = "85*", universe_checksum = uv
)
plan_path <- request_plan_file(cfg)
if (file.exists(plan_path)) {
  existing <- data.table::as.data.table(arrow::read_parquet(plan_path))

  if ("plan_status" %in% names(existing)) {
    keep <- existing[plan_status == "superseded" | dataset_type != "trade_detailed_top20"]
    plan_dt <- unique(data.table::rbindlist(list(keep, plan_new), fill = TRUE), by = "request_id")
  } else {
    plan_dt <- unique(data.table::rbindlist(list(existing, plan_new), fill = TRUE), by = "request_id")
  }
} else {
  plan_dt <- plan_new
}
plan_dt[, request_id := as.character(request_id)]
atomic_write_parquet_dt(plan_dt, plan_path)

res <- run_production_fetch_stage(
  cfg = cfg,
  plan_dt = plan_new,
  stage_filter = "trade_detailed_top20",
  request_delay_seconds = request_delay,
  dry_run = dry_run,
  max_requests = max_requests,
  retry_failed_only = retry_failed_only,
  refresh_raw = refresh_raw
)
cat(sprintf("FETCH_DETAILED_DONE executed=%d skipped=%d\n", res$executed %||% 0L, res$skipped %||% 0L))
if (dry_run) quit(status = 0)

st <- load_state(cfg)
ok_ids <- st[dataset_type == "trade_detailed_top20" & status %in% c("succeeded", "skipped_cached")]$request_id
if (!length(ok_ids)) {
  cat("No succeeded detailed requests yet.\n")
  quit(status = 0)
}

parsed_rows <- list()
for (rid in ok_ids) {
  req <- plan_new[request_id == rid][1]
  parsed_path <- file.path(dirname(req$raw_file), paste0(rid, ".parquet"))
  if (!file.exists(parsed_path) && file.exists(req$raw_file)) {
    body <- paste(readLines(req$raw_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    parsed <- parse_comtrade_payload_production(body)
    dt <- comtrade_records_to_dt_production(parsed$records)
    if (nrow(dt)) {
      dt[, `:=`(request_id = rid, ingested_at = utc_now())]
      arrow::write_parquet(dt, parsed_path)
    }
  }
  if (file.exists(parsed_path)) {
    dt <- data.table::as.data.table(arrow::read_parquet(parsed_path))
    if (nrow(dt)) parsed_rows[[length(parsed_rows) + 1L]] <- dt
  }
}
if (!length(parsed_rows)) stop("No parsed detailed data.", call. = FALSE)

all_dt <- data.table::rbindlist(parsed_rows, fill = TRUE)
all_dt[, year := as.integer(ref_year %||% substr(period, 1, 4))]
all_dt[, frequency := "A"]
all_dt[, hs_code := as.character(cmd_code)]
all_dt[, hs_revision := as.character(classification_code %||% classification_search_code)]
all_dt[, commodity_description := as.character(cmd_desc)]
all_dt[, trade_value_usd := as.numeric(primary_value)]
all_dt[, net_weight_kg := as.numeric(net_wgt)]
all_dt[, quantity := as.numeric(qty)]
all_dt[, quantity_unit := as.character(qty_unit)]
all_dt[, hs_level := ifelse(is.na(aggr_level), nchar(as.character(cmd_code)), as.integer(aggr_level))]
all_dt[, source_updated_at := NA_character_]
all_dt[, period := as.character(year)]

hs4_set <- as.character(top_hs4$hs_code)
rep_set <- as.character(top_reporters$reporter_code)
par_set <- as.character(top_partners$partner_code)

detailed_dt <- all_dt[
  year %in% years &
    flow_code %in% c("M", "X") &
    reporter_code %in% rep_set &
    partner_code %in% par_set &
    hs_code %in% hs4_set &
    partner_iso3 != "W00" &
    nchar(hs_code) == 4 &
    substr(hs_code, 1, 2) == "85"
]

required_cols <- c(
  "year", "period", "frequency",
  "reporter_code", "reporter_iso3", "reporter_name",
  "partner_code", "partner_iso3", "partner_name",
  "flow_code", "flow_name",
  "hs_revision", "hs_code", "hs_level",
  "commodity_description",
  "trade_value_usd", "net_weight_kg", "quantity", "quantity_unit",
  "source_updated_at", "ingested_at", "request_id", "universe_checksum"
)
detailed_dt[, universe_checksum := as.character(uv)]
for (c in required_cols) if (!c %in% names(detailed_dt)) detailed_dt[, (c) := NA]
detailed_dt <- detailed_dt[, ..required_cols]
detailed_dt <- unique(detailed_dt, by = c("year", "flow_code", "reporter_code", "partner_code", "hs_code"))

if (!nrow(detailed_dt)) stop("Detailed dataset empty after filtering.", call. = FALSE)
out_path <- file.path(cfg[['paths']]$processed, "trade_detailed_top20.parquet")
atomic_write_parquet_dt(detailed_dt, out_path)
cat("DETAILED_PARQUET_OK rows=", nrow(detailed_dt),
    " reporters=", data.table::uniqueN(detailed_dt$reporter_code),
    " partners=", data.table::uniqueN(detailed_dt$partner_code),
    " hs4=", data.table::uniqueN(detailed_dt$hs_code), "\n", sep = "")
quit(status = 0)
