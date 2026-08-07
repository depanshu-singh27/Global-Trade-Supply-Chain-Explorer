test_that("forecasting module loads series filters offline", {
  bundle <- make_forecast_fixture_bundle(4L)
  q <- compute_monthly_series_quality(bundle$monthly_long)
  sel <- select_stable_forecast_series(q, bundle$candidates, n = 3L)
  tmp <- tempfile("fcfg")
  dir.create(file.path(tmp, "data", "processed"), recursive = TRUE)
  cfg <- list(paths = list(processed = file.path(tmp, "data", "processed"), interim = file.path(tmp, "data", "interim"), raw = file.path(tmp, "data", "raw")))
  arrow::write_parquet(bundle$monthly_long, file.path(cfg$paths$processed, "monthly_trade_long.parquet"))
  arrow::write_parquet(q, file.path(cfg$paths$processed, "monthly_series_quality.parquet"))
  arrow::write_parquet(sel, file.path(cfg$paths$processed, "forecast_selected_series.parquet"))
  arrow::write_parquet(data.table::data.table(), file.path(cfg$paths$processed, "forecast_model_metrics.parquet"))
  arrow::write_parquet(data.table::data.table(series_id = sel$series_id, selected_model_id = "seasonal_naive"),
                       file.path(cfg$paths$processed, "forecast_selected_models.parquet"))
  arrow::write_parquet(data.table::data.table(), file.path(cfg$paths$processed, "forecast_predictions.parquet"))
  arrow::write_parquet(data.table::data.table(), file.path(cfg$paths$processed, "forecast_backtest_predictions.parquet"))
  arrow::write_parquet(data.table::data.table(), file.path(cfg$paths$processed, "forecast_residual_diagnostics.parquet"))
  jsonlite::write_json(list(
    engine_version = FORECAST_ENGINE_VERSION,
    prophet_available = FALSE,
    mape_claim_below_15 = FALSE,
    data_mode = FORECAST_DATA_MODE_FIXTURE,
    data_source = FORECAST_DATA_SOURCE_FIXTURE,
    is_fixture = TRUE,
    production_forecast_available = FALSE,
    live_monthly_successful_requests = 0L,
    fixture_accuracy_disclaimer = "Fixture metrics are offline synthetic diagnostics only and must not be described as production accuracy."
  ),
                       file.path(cfg$paths$processed, "forecast_profile.json"), auto_unbox = TRUE)

  snap <- shiny::reactiveVal(list(detailed_coverage = make_shock_coverage()))
  cfg_r <- shiny::reactive(cfg)
  shiny::testServer(mod_forecasting_server, args = list(snap = snap, cfg = cfg_r), {
    expect_true(length(series_choices()) >= 1)
    expect_true(isTRUE(forecast_snap()$profile$is_fixture))
    expect_false(isTRUE(forecast_snap()$profile$production_forecast_available))
    expect_equal(forecast_snap()$profile$live_monthly_successful_requests, 0L)
    session$setInputs(series_id = names(series_choices())[1], model_mode = "selected",
                      model_id = "seasonal_naive", horizon_view = 1, interval_level = "80", show_imputed = TRUE)
    expect_true(nrow(active_series()) > 0)
    expect_true(grepl("Synthetic fixture results", forecast_fixture_notice(), fixed = TRUE))
  })

  txt <- paste(readLines(file.path(TEST_ROOT, "R/mod_forecasting.R"), warn = FALSE), collapse = "\n")
  expect_false(grepl("guaranteed forecast|expected realised trade|250 ms", txt, ignore.case = TRUE))
  expect_false(grepl("MAPE is (at or )?below|median MAPE.*(<=|≤)\\s*15", txt, ignore.case = TRUE))
  expect_true(grepl("Synthetic fixture results", txt, fixed = TRUE))
  expect_false(grepl(paste0("COMTRADE", "_", "PRIMARY"), txt))
})
