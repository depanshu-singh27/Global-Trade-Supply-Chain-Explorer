options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")

run_script <- function(path) {
  cat("=== ", basename(path), " ===\n", sep = "")
  rc <- system2(
    file.path(R.home("bin"), "Rscript"),
    args = shQuote(path),
    stdout = "",
    stderr = ""
  )
  if (!identical(as.integer(rc), 0L)) {
    stop("Script failed: ", path, " status=", rc, call. = FALSE)
  }
}

if (identical(tolower(Sys.getenv("GTSC_MONTHLY_QUOTA_PREFLIGHT", "")), "true")) {
  run_script(file.path(root, "scripts/19_fetch_monthly_trade.R"))
  quit(status = 0)
}

use_fixtures <- identical(tolower(Sys.getenv("GTSC_FORECAST_USE_FIXTURES", "")), "true")
dry <- identical(tolower(Sys.getenv("GTSC_MONTHLY_DRY_RUN", "")), "true")

if (isTRUE(use_fixtures) || isTRUE(dry)) {

  run_script(file.path(root, "scripts/18_plan_monthly_forecasting.R"))
  if (isTRUE(dry) && !isTRUE(use_fixtures)) {
    cat("DRY_RUN complete after planning.\n")
    quit(status = 0)
  }
  Sys.setenv(GTSC_FORECAST_USE_FIXTURES = "true")
  run_script(file.path(root, "scripts/20_build_monthly_series.R"))
  run_script(file.path(root, "scripts/21_run_forecast_backtests.R"))
  run_script(file.path(root, "scripts/22_build_forecast_outputs.R"))
  cat("PHASE12_OFFLINE_PIPELINE_OK\n")
  quit(status = 0)
}

run_script(file.path(root, "scripts/18_plan_monthly_forecasting.R"))
run_script(file.path(root, "scripts/19_fetch_monthly_trade.R"))
run_script(file.path(root, "scripts/20_build_monthly_series.R"))

cfg_src <- file.path(root, "R/config.R")
source(file.path(root, "R/zzz_bootstrap.R"))
source_project_r(root)
cfg <- load_config()
ml <- safe_read_parquet_dt(file.path(cfg$paths$processed, "monthly_trade_long.parquet"))
if (is.null(ml) || !nrow(ml)) {
  cat("PHASE12_COMPLETE_WITH_WARNINGS no monthly rows after fetch; offline architecture remains.\n")

  Sys.setenv(GTSC_FORECAST_USE_FIXTURES = "true")
  run_script(file.path(root, "scripts/20_build_monthly_series.R"))
  run_script(file.path(root, "scripts/21_run_forecast_backtests.R"))
  run_script(file.path(root, "scripts/22_build_forecast_outputs.R"))
  cat("PHASE12_FIXTURE_FALLBACK_OK\n")
  quit(status = 0)
}
run_script(file.path(root, "scripts/21_run_forecast_backtests.R"))
run_script(file.path(root, "scripts/22_build_forecast_outputs.R"))
cat("PHASE12_PIPELINE_OK\n")
