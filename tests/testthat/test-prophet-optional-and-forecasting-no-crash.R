test_that("app sources and forecasting works when Prophet is absent", {
  root <- release_test_root()
  expect_true(file.exists(file.path(root, "app.R")))

  prophet_available <- requireNamespace("prophet", quietly = TRUE)

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(root)

  with_temp_env(
    list(
      GTSC_RUNTIME_PROFILE = "demo",
      GTSC_PUBLIC_MODE = "true",
      GTSC_ALLOW_SCENARIO_WRITES = "false",
      GTSC_READ_ONLY_MODE = "true",
      GTSC_DATA_ROOT = file.path(root, "data", "release", "demo")
    ),
    {
      expect_no_error(suppressMessages(source("app.R", local = FALSE)))
    }
  )

  av <- prophet_availability()
  if (!prophet_available) {
    expect_false(isTRUE(av$available), info = paste("Unexpected Prophet availability:", av$reason))
  }

  y <- abs(sin(seq(0, 7, length.out = 48))) * 1000 + 500
  fc_prop <- fit_forecast_model(y, model_id = "prophet", h = 3L)
  expect_identical(fc_prop$model_id, "prophet")
  expect_true(fc_prop$status %in% c("ok", "unavailable"))
  if (!prophet_available) {
    expect_identical(fc_prop$status, "unavailable")
    expect_identical(length(fc_prop$point), 3L)
    expect_true(all(is.na(fc_prop$point)))
    expect_true(nzchar(fc_prop$warning_message %||% ""))
  }

  for (m in c("seasonal_naive", "naive", "drift")) {
    fc <- fit_forecast_model(y, model_id = m, h = 3L)
    expect_identical(fc$status, "ok", info = paste("model:", m, "status:", fc$status))
  }

  if (requireNamespace("forecast", quietly = TRUE)) {
    ets <- fit_forecast_model(y, model_id = "ets", h = 3L)
    ar <- fit_forecast_model(y, model_id = "arima", h = 3L)
    expect_true(ets$status %in% c("ok", "unavailable"))
    expect_true(ar$status %in% c("ok", "unavailable"))
  }

  bundle <- make_forecast_fixture_bundle(4L)
  q <- compute_monthly_series_quality(bundle$monthly_long)
  tmp <- tempfile("fcfg")
  dir.create(file.path(tmp, "data", "processed"), recursive = TRUE)
  cfg <- list(
    paths = list(
      processed = file.path(tmp, "data", "processed"),
      interim = file.path(tmp, "data", "interim"),
      raw = file.path(tmp, "data", "raw")
    )
  )

  arrow::write_parquet(bundle$monthly_long, file.path(cfg$paths$processed, "monthly_trade_long.parquet"))
  arrow::write_parquet(q, file.path(cfg$paths$processed, "monthly_series_quality.parquet"))
  sel <- select_stable_forecast_series(q, bundle$candidates, n = 3L)
  arrow::write_parquet(sel, file.path(cfg$paths$processed, "forecast_selected_series.parquet"))
  arrow::write_parquet(data.table::data.table(), file.path(cfg$paths$processed, "forecast_model_metrics.parquet"))
  arrow::write_parquet(
    data.table::data.table(series_id = sel$series_id, selected_model_id = "seasonal_naive"),
    file.path(cfg$paths$processed, "forecast_selected_models.parquet")
  )
  arrow::write_parquet(data.table::data.table(), file.path(cfg$paths$processed, "forecast_predictions.parquet"))
  arrow::write_parquet(data.table::data.table(), file.path(cfg$paths$processed, "forecast_backtest_predictions.parquet"))
  arrow::write_parquet(data.table::data.table(), file.path(cfg$paths$processed, "forecast_residual_diagnostics.parquet"))

  jsonlite::write_json(
    list(
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
    file.path(cfg$paths$processed, "forecast_profile.json"),
    auto_unbox = TRUE
  )

  snap <- shiny::reactiveVal(list(detailed_coverage = make_shock_coverage()))
  cfg_r <- shiny::reactive(cfg)

  shiny::testServer(mod_forecasting_server, args = list(snap = snap, cfg = cfg_r), {
    expect_true(length(series_choices()) >= 1)
    sid <- names(series_choices())[1]

    session$setInputs(
      series_id = sid,
      model_mode = "manual",
      model_id = "prophet",
      horizon_view = 1,
      interval_level = "80",
      show_imputed = TRUE
    )

    fc <- active_forecast()
    expect_true(data.table::is.data.table(fc))
    if (!prophet_available) {
      expect_equal(nrow(fc), 0L)
    }
  })
})
