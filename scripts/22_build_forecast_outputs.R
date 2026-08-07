options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)

cfg <- load_config()
snap <- load_processed_snapshot(cfg)
coverage <- snap$detailed_coverage %||% trade_flow_coverage_status(snap)

monthly_long <- safe_read_parquet_dt(file.path(cfg$paths$processed, "monthly_trade_long.parquet"))
candidates <- safe_read_parquet_dt(file.path(cfg$paths$processed, "monthly_trade_candidates.parquet"))
quality <- safe_read_parquet_dt(file.path(cfg$paths$processed, "monthly_series_quality.parquet"))
selected <- safe_read_parquet_dt(file.path(cfg$paths$processed, "forecast_selected_series.parquet"))
backtests <- safe_read_parquet_dt(file.path(cfg$paths$processed, "forecast_backtest_predictions.parquet"))
metrics <- safe_read_parquet_dt(file.path(cfg$paths$processed, "forecast_model_metrics.parquet"))
selected_models <- safe_read_parquet_dt(file.path(cfg$paths$processed, "forecast_selected_models.parquet"))
state <- safe_read_parquet_dt(monthly_pipeline_state_file(cfg))
prov_file <- read_forecast_data_provenance(cfg)
force_fixture <- identical(tolower(Sys.getenv("GTSC_FORECAST_USE_FIXTURES", "")), "true") ||
  identical(prov_file$data_mode %||% "", FORECAST_DATA_MODE_FIXTURE) ||
  detect_forecast_fixture_markers(monthly_long, candidates)

if (is.null(monthly_long) || is.null(selected_models)) {
  cat("Missing inputs for final forecast build.\n")
  quit(status = 1)
}

forecasts <- generate_final_forecasts(monthly_long, selected_models, horizon = 12L)
residuals <- compute_forecast_residual_diagnostics(monthly_long, selected_models)
validation <- validate_phase12_forecasts(
  monthly_long, quality %||% data.table::data.table(), selected %||% data.table::data.table(),
  backtests %||% data.table::data.table(), metrics %||% data.table::data.table(),
  selected_models, forecasts
)
profile <- build_forecast_profile(
  candidates %||% data.table::data.table(),
  quality %||% data.table::data.table(),
  selected %||% data.table::data.table(),
  metrics %||% data.table::data.table(),
  selected_models,
  forecasts,
  coverage = coverage,
  monthly_state = state,
  monthly_long = monthly_long,
  data_mode = prov_file$data_mode,
  data_source = prov_file$data_source,
  force_fixture = force_fixture
)
persist_phase12_outputs(
  candidates %||% data.table::data.table(),
  monthly_long,
  quality %||% data.table::data.table(),
  selected %||% data.table::data.table(),
  backtests %||% data.table::data.table(),
  metrics %||% data.table::data.table(),
  selected_models,
  forecasts,
  residuals,
  validation,
  profile,
  cfg = cfg
)
cat(
  "FORECAST_OUTPUTS_OK rows=", nrow(forecasts),
  " residuals=", nrow(residuals),
  " prophet=", profile$prophet_available,
  " data_mode=", profile$data_mode,
  " production_forecast_available=", profile$production_forecast_available,
  " live_successes=", profile$live_monthly_successful_requests,
  "\n",
  sep = ""
)
