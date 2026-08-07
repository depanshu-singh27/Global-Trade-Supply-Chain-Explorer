build_forecast_profile <- function(candidates,
                                     quality,
                                     selected,
                                     metrics,
                                     selected_models,
                                     forecasts,
                                     coverage = NULL,
                                     monthly_state = NULL,
                                     monthly_long = NULL,
                                     data_mode = NULL,
                                     data_source = NULL,
                                     force_fixture = FALSE) {
  prop <- prophet_availability()
  prov <- resolve_forecast_provenance(
    data_mode = data_mode,
    data_source = data_source,
    monthly_state = monthly_state,
    monthly_long = monthly_long,
    candidates = candidates,
    force_fixture = force_fixture
  )
  list(
    engine_version = FORECAST_ENGINE_VERSION,
    candidate_version = FORECAST_CANDIDATE_VERSION,
    generated_at = utc_now(),
    methodology_notice = forecast_methodology_notice(),
    data_mode = prov$data_mode,
    data_source = prov$data_source,
    is_fixture = prov$is_fixture,
    production_forecast_available = prov$production_forecast_available,
    live_monthly_successful_requests = prov$live_monthly_successful_requests,
    fixture_accuracy_disclaimer = prov$fixture_accuracy_disclaimer,
    universe_version = coverage$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM,
    production_status = coverage$production_status %||% "unknown",
    represented_reporter_count = coverage$represented_reporter_count %||% NA_integer_,
    selected_reporter_count = coverage$selected_reporter_count %||% NA_integer_,
    candidate_count = nrow(candidates),
    stable_selected_count = nrow(selected),
    rejected_count = if (nrow(quality)) sum(quality$stable == FALSE) else NA_integer_,
    prophet_available = isTRUE(prop$available),
    prophet_reason = prop$reason,
    package_versions = forecast_package_versions(),
    monthly_request_counts = if (!is.null(monthly_state) && nrow(monthly_state)) {
      as.list(table(monthly_state$status))
    } else {
      list()
    },
    median_mape = {
      m <- data.table::as.data.table(metrics)
      if (!nrow(m) || !any(is.finite(m$mape))) NA_real_ else stats::median(m$mape, na.rm = TRUE)
    },
    mape_claim_below_15 = FALSE,
    mape_claim_note = if (isTRUE(prov$is_fixture)) {
      paste(
        "No MAPE threshold claim is made.",
        "Fixture median MAPE is an offline synthetic diagnostic only and is not production accuracy."
      )
    } else {
      "No MAPE threshold claim is made unless validated live-data backtests support it."
    }
  )
}

persist_phase12_outputs <- function(candidates,
                                      monthly_long,
                                      quality,
                                      selected,
                                      backtests,
                                      metrics,
                                      selected_models,
                                      forecasts,
                                      residuals,
                                      validation,
                                      profile,
                                      cfg = load_config()) {
  ensure_dir(cfg$paths$processed)
  profile <- normalize_forecast_profile(
    profile,
    monthly_long = monthly_long,
    candidates = candidates,
    monthly_state = NULL
  )

  write_forecast_data_provenance(
    resolve_forecast_provenance(
      data_mode = profile$data_mode,
      data_source = profile$data_source,
      monthly_long = monthly_long,
      candidates = candidates,
      force_fixture = isTRUE(profile$is_fixture)
    ),
    cfg = cfg
  )
  write_one <- function(dt, name) {
    path <- file.path(cfg$paths$processed, paste0(name, ".parquet"))
    atomic_write_parquet_dt(data.table::as.data.table(dt), path)
    path
  }
  paths <- list(
    candidates = write_one(candidates, "monthly_trade_candidates"),
    monthly_long = write_one(monthly_long, "monthly_trade_long"),
    quality = write_one(quality, "monthly_series_quality"),
    selected = write_one(selected, "forecast_selected_series"),
    backtests = write_one(backtests, "forecast_backtest_predictions"),
    metrics = write_one(metrics, "forecast_model_metrics"),
    selected_models = write_one(selected_models, "forecast_selected_models"),
    forecasts = write_one(forecasts, "forecast_predictions"),
    residuals = write_one(residuals, "forecast_residual_diagnostics"),
    validation = write_one(validation, "phase12_validation_results")
  )
  write_json_atomic(profile, file.path(cfg$paths$processed, "forecast_profile.json"))
  manifest <- list(
    engine_version = FORECAST_ENGINE_VERSION,
    generated_at = utc_now(),
    data_mode = profile$data_mode,
    data_source = profile$data_source,
    is_fixture = isTRUE(profile$is_fixture),
    production_forecast_available = isTRUE(profile$production_forecast_available),
    live_monthly_successful_requests = as.integer(profile$live_monthly_successful_requests %||% 0L),
    fixture_accuracy_disclaimer = profile$fixture_accuracy_disclaimer,
    mape_claim_below_15 = FALSE,
    outputs = paths,
    prophet_available = profile$prophet_available,
    stable_selected_count = profile$stable_selected_count,
    production_status = profile$production_status,
    contains_credentials = FALSE,
    replaces_prior_forecast_state = TRUE
  )
  write_json_atomic(manifest, file.path(cfg$paths$processed, "forecast_pipeline_manifest.json"))
  cov_rep <- data.table::data.table(
    metric = c(
      "candidates", "stable_selected", "monthly_rows", "backtest_rows", "forecast_rows",
      "live_monthly_successful_requests", "production_forecast_available"
    ),
    value = c(
      nrow(candidates), nrow(selected), nrow(monthly_long),
      nrow(backtests), nrow(forecasts),
      as.numeric(profile$live_monthly_successful_requests %||% 0),
      as.numeric(isTRUE(profile$production_forecast_available))
    ),
    data_mode = profile$data_mode,
    data_source = profile$data_source
  )
  write_one(cov_rep, "forecast_coverage_report")
  invisible(list(paths = paths, profile = profile, manifest = manifest))
}

run_phase12_model_pipeline <- function(monthly_long,
                                         candidates,
                                         coverage = NULL,
                                         cfg = load_config(),
                                         n_stable = 10L,
                                         data_mode = NULL,
                                         data_source = NULL,
                                         monthly_state = NULL,
                                         force_fixture = FALSE) {
  quality <- compute_monthly_series_quality(monthly_long)
  selected <- select_stable_forecast_series(quality, candidates = candidates, n = n_stable)
  models <- c("seasonal_naive", "naive", "drift", "ets", "arima", "prophet")
  backtests <- run_forecast_backtests(
    monthly_long, selected, models = models,
    min_train = 24L, horizons = c(1L, 3L, 6L), step = 3L
  )
  train_map <- lapply(unique(selected$series_id), function(sid) {
    s <- data.table::as.data.table(monthly_long)[series_id == sid]
    data.table::setorderv(s, "date")
    s$model_value_usd %||% s$trade_value_usd
  })
  names(train_map) <- unique(selected$series_id)
  metrics <- summarise_forecast_metrics(backtests, training_by_series = train_map)
  selected_models <- select_forecast_models(metrics)
  forecasts <- generate_final_forecasts(monthly_long, selected_models, horizon = 12L)
  residuals <- compute_forecast_residual_diagnostics(monthly_long, selected_models)
  validation <- validate_phase12_forecasts(
    monthly_long, quality, selected, backtests, metrics, selected_models, forecasts
  )
  profile <- build_forecast_profile(
    candidates, quality, selected, metrics, selected_models, forecasts,
    coverage = coverage,
    monthly_state = monthly_state,
    monthly_long = monthly_long,
    data_mode = data_mode,
    data_source = data_source,
    force_fixture = force_fixture
  )
  persist_phase12_outputs(
    candidates, monthly_long, quality, selected, backtests, metrics,
    selected_models, forecasts, residuals, validation, profile, cfg = cfg
  )
}
