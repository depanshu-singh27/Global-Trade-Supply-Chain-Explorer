if (file.exists("renv/activate.R")) source("renv/activate.R")

source("R/utilities.R")
source("R/config.R")
source("R/comtrade_client.R")
source("R/comtrade_request_planner.R")
source("R/pipeline_state.R")
source("R/production_trade_pipeline.R")
source("R/reference_data.R")

cfg <- load_config()
setwd(cfg[['project_root']])

dry_run <- tolower(Sys.getenv("GTSC_DRY_RUN", "false")) %in% c("1", "true", "yes")
max_requests <- suppressWarnings(as.integer(Sys.getenv("GTSC_MAX_REQUESTS", "")))
if (is.na(max_requests)) max_requests <- Inf
retry_failed_only <- tolower(Sys.getenv("GTSC_RETRY_FAILED_ONLY", "false")) %in% c("1", "true", "yes")
refresh_raw <- tolower(Sys.getenv("GTSC_REFRESH_RAW", "false")) %in% c("1", "true", "yes")
request_delay <- suppressWarnings(as.numeric(Sys.getenv("GTSC_REQUEST_DELAY_SECONDS", "1.1")))
if (is.na(request_delay)) request_delay <- 1.1
stage <- Sys.getenv("GTSC_GLOBAL_STAGE", "ranking")

cat("=== Phase 2: fetch global HS-85 ===\n")
plan_path <- request_plan_file(cfg)
if (!file.exists(plan_path)) stop("Missing request plan. Run scripts/06_plan_production_ingestion.R first.", call. = FALSE)
plan_dt <- data.table::as.data.table(arrow::read_parquet(plan_path))
if ("plan_status" %in% names(plan_dt)) plan_dt <- plan_dt[plan_status == "active"]

stage_filter <- switch(
  stage,
  ranking = "trade_global_hs85_ranking_year",
  full = "trade_global_hs85_annual",
  both = c("trade_global_hs85_ranking_year", "trade_global_hs85_annual"),
  stop("GTSC_GLOBAL_STAGE must be ranking|full|both", call. = FALSE)
)

plan_stage <- plan_dt[dataset_type %in% stage_filter]
if (!nrow(plan_stage)) stop("No active global requests for stage=", stage, call. = FALSE)
if (any(plan_stage$reporter_code == "0")) stop("Plan still contains reporterCode=0", call. = FALSE)

res <- run_production_fetch_stage(
  cfg = cfg,
  plan_dt = plan_stage,
  stage_filter = stage_filter,
  request_delay_seconds = request_delay,
  dry_run = dry_run,
  max_requests = max_requests,
  retry_failed_only = retry_failed_only,
  refresh_raw = refresh_raw
)
cat(sprintf("FETCH_GLOBAL_DONE stage=%s executed=%d skipped=%d dry_run=%s\n",
            stage, res$executed %||% 0L, res$skipped %||% 0L, dry_run))
if (dry_run) quit(status = 0)

st <- load_state(cfg)
ok <- st[dataset_type %in% stage_filter & status %in% c("succeeded", "skipped_cached", "empty")]
ok_ids <- ok[status %in% c("succeeded", "skipped_cached")]$request_id
if (!length(ok_ids)) {
  cat("No succeeded global requests yet (may be limited batch). Skipping parquet assembly.\n")
  quit(status = 0)
}

parsed_rows <- list()
for (rid in ok_ids) {
  req <- plan_stage[request_id == rid][1]
  if (!nrow(req)) {

    req <- plan_dt[request_id == rid][1]
  }
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
if (!length(parsed_rows)) {
  cat("No parsed rows available yet.\n")
  quit(status = 0)
}

all_dt <- data.table::rbindlist(parsed_rows, fill = TRUE)
all_dt[, year := {
  y <- suppressWarnings(as.integer(ref_year))
  ifelse(is.na(y), suppressWarnings(as.integer(substr(as.character(period), 1, 4))), y)
}]
all_dt[, frequency := "A"]
all_dt[, hs_code := as.character(cmd_code)]
all_dt[, hs_revision := as.character(classification_code %||% classification_search_code)]
all_dt[, commodity_description := as.character(cmd_desc)]
all_dt[, trade_value_usd := as.numeric(primary_value)]
all_dt[, net_weight_kg := as.numeric(net_wgt)]
all_dt[, quantity := as.numeric(qty)]
all_dt[, quantity_unit := as.character(qty_unit)]
all_dt[, source_updated_at := NA_character_]
all_dt[, hs_level := ifelse(is.na(aggr_level), NA_integer_, as.integer(aggr_level))]

global_dt <- all_dt[
  hs_code == "85" & (is.na(hs_level) | hs_level == 2) &
    (partner_iso3 == "W00" | partner_code == "0")
]

keep_cols <- c(
  "year", "period", "frequency",
  "reporter_code", "reporter_iso3", "reporter_name",
  "flow_code", "flow_name",
  "partner_code", "partner_iso3", "partner_name",
  "hs_revision", "hs_code", "hs_level",
  "commodity_description",
  "trade_value_usd", "net_weight_kg", "quantity", "quantity_unit",
  "source_updated_at", "ingested_at", "request_id"
)
for (c in keep_cols) if (!c %in% names(global_dt)) global_dt[, (c) := NA]
global_dt <- global_dt[, ..keep_cols]
global_dt <- unique(global_dt, by = c("year", "reporter_code", "flow_code", "hs_code"))

out_path <- file.path(cfg[['paths']]$processed, "trade_global_hs85_annual.parquet")
if (file.exists(out_path) && stage == "ranking") {
  existing <- data.table::as.data.table(arrow::read_parquet(out_path))
  global_dt <- unique(data.table::rbindlist(list(existing, global_dt), fill = TRUE),
                      by = c("year", "reporter_code", "flow_code", "hs_code"))
}
ensure_dir(dirname(out_path))
atomic_write_parquet_dt(global_dt, out_path)
cat("GLOBAL_PARQUET_OK file=", out_path, " rows=", nrow(global_dt),
    " years=", paste(sort(unique(global_dt$year)), collapse = ","),
    " reporters=", data.table::uniqueN(global_dt$reporter_code), "\n", sep = "")
quit(status = 0)
