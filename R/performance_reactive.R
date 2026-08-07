profile_reactive_invalidations <- function(cfg = normalise_performance_config()) {

  data.table::data.table(
    counter = names(get_perf_counters()),
    count = unlist(get_perf_counters(), use.names = FALSE),
    enabled = perf_counters_enabled(),
    note = "Development-only; does not alter calculations",
    measured_at = utc_now()
  )
}

load_forecast_ui_snapshot <- function(cfg = load_config()) {
  inc_perf_counter("forecast_snapshot_load_count")
  paths <- list(
    monthly_long = file.path(cfg$paths$processed, "monthly_trade_long.parquet"),
    quality = file.path(cfg$paths$processed, "monthly_series_quality.parquet"),
    selected = file.path(cfg$paths$processed, "forecast_selected_series.parquet"),
    metrics = file.path(cfg$paths$processed, "forecast_model_metrics.parquet"),
    selected_models = file.path(cfg$paths$processed, "forecast_selected_models.parquet"),
    forecasts = file.path(cfg$paths$processed, "forecast_predictions.parquet"),
    backtests = file.path(cfg$paths$processed, "forecast_backtest_predictions.parquet"),
    residuals = file.path(cfg$paths$processed, "forecast_residual_diagnostics.parquet"),
    profile = file.path(cfg$paths$processed, "forecast_profile.json"),
    candidates = file.path(cfg$paths$processed, "monthly_trade_candidates.parquet"),
    state = monthly_pipeline_state_file(cfg)
  )
  monthly_long <- safe_read_parquet_dt(paths$monthly_long)
  candidates <- safe_read_parquet_dt(paths$candidates)
  raw_profile <- safe_read_json(paths$profile)
  state <- safe_read_parquet_dt(paths$state)
  profile <- normalize_forecast_profile(
    raw_profile,
    monthly_long = monthly_long,
    candidates = candidates,
    monthly_state = state
  )
  list(
    monthly_long = monthly_long,
    quality = safe_read_parquet_dt(paths$quality),
    selected = safe_read_parquet_dt(paths$selected),
    metrics = safe_read_parquet_dt(paths$metrics),
    selected_models = safe_read_parquet_dt(paths$selected_models),
    forecasts = safe_read_parquet_dt(paths$forecasts),
    backtests = safe_read_parquet_dt(paths$backtests),
    residuals = safe_read_parquet_dt(paths$residuals),
    profile = profile,
    candidates = candidates
  )
}
