if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R"); source("R/pipeline_state.R")
source("R/comtrade_request_planner.R"); source("R/wdi_client.R")
source("R/wdi_request_planner.R"); source("R/wdi_production_pipeline.R")

cfg <- load_config(); setwd(cfg$project_root)
cat("=== Phase 3: fetch WDI production ===\n")

dry_run <- tolower(Sys.getenv("GTSC_WDI_DRY_RUN", "false")) %in% c("1", "true", "yes")
max_requests <- suppressWarnings(as.integer(Sys.getenv("GTSC_WDI_MAX_REQUESTS", "")))
if (is.na(max_requests)) max_requests <- Inf
retry_failed_only <- tolower(Sys.getenv("GTSC_WDI_RETRY_FAILED_ONLY", "false")) %in% c("1", "true", "yes")
refresh_raw <- tolower(Sys.getenv("GTSC_WDI_REFRESH_RAW", "false")) %in% c("1", "true", "yes")
delay <- suppressWarnings(as.numeric(Sys.getenv("GTSC_WDI_REQUEST_DELAY_SECONDS", "0.4")))
if (is.na(delay)) delay <- 0.4

if (!file.exists(wdi_plan_file(cfg))) {
  stop("Missing WDI plan. Run scripts/11_plan_wdi_production.R first.", call. = FALSE)
}
plan <- data.table::as.data.table(arrow::read_parquet(wdi_plan_file(cfg)))

remaining_budget <- max_requests
total_exec <- 0L
total_skip <- 0L

plan <- data.table::as.data.table(arrow::read_parquet(wdi_plan_file(cfg)))
res <- run_wdi_production_fetch(
  cfg = cfg,
  plan_dt = plan,
  dry_run = dry_run,
  max_requests = remaining_budget,
  retry_failed_only = retry_failed_only,
  refresh_raw = refresh_raw,
  request_delay_seconds = delay
)
total_exec <- total_exec + (res$executed %||% 0L)
total_skip <- total_skip + (res$skipped %||% 0L)

if (!dry_run) {
  if (is.finite(max_requests)) {
    remaining_budget <- remaining_budget - (res$executed %||% 0L)
  }
  plan2 <- data.table::as.data.table(arrow::read_parquet(wdi_plan_file(cfg)))
  new_pages <- plan2[page > 1L]
  if (nrow(new_pages) && (is.infinite(remaining_budget) || remaining_budget > 0)) {
    res2 <- run_wdi_production_fetch(
      cfg = cfg,
      plan_dt = new_pages,
      dry_run = FALSE,
      max_requests = remaining_budget,
      retry_failed_only = FALSE,
      refresh_raw = refresh_raw,
      request_delay_seconds = delay
    )
    total_exec <- total_exec + (res2$executed %||% 0L)
    total_skip <- total_skip + (res2$skipped %||% 0L)
  }
}

st <- load_wdi_state(cfg)
cat(sprintf("WDI_FETCH_DONE executed=%d skipped=%d\n", total_exec, total_skip))
print(st[, .N, by = status])
quit(status = 0)
