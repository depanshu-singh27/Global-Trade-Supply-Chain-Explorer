options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)

cfg <- load_config()
ensure_data_dirs(cfg)

if (identical(tolower(Sys.getenv("GTSC_MONTHLY_QUOTA_PREFLIGHT", "")), "true")) {
  run_monthly_quota_preflight(cfg)
  quit(status = 0)
}

max_req <- Sys.getenv("GTSC_MONTHLY_MAX_REQUESTS", unset = "")
max_requests <- if (nzchar(max_req)) as.numeric(max_req) else Inf
dry <- identical(tolower(Sys.getenv("GTSC_MONTHLY_DRY_RUN", "")), "true")
retry_failed <- identical(tolower(Sys.getenv("GTSC_RETRY_FAILED_ONLY", "")), "true")

if (!file.exists(monthly_plan_file(cfg))) {
  cat("Plan missing; run scripts/18_plan_monthly_forecasting.R first.\n")
  quit(status = 1)
}

res <- execute_monthly_forecast_fetch(
  cfg,
  max_requests = max_requests,
  dry_run = dry,
  retry_failed_only = retry_failed
)
cat("MONTHLY_FETCH_DONE executed=", res$executed, " quota_blocked=", res$quota_blocked, "\n", sep = "")
