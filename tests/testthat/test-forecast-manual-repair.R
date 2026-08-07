test_that("forecast model choice polarity submits canonical IDs", {
  ch <- forecast_model_choices()
  expect_true(all(unname(ch) %in% forecast_model_ids()))
  expect_equal(unname(ch[["ETS"]]), "ets")
  expect_equal(normalise_forecast_model_id("ETS"), "ets")
  expect_equal(normalise_forecast_model_id("Prophet"), "prophet")
  expect_equal(normalise_forecast_model_id("Seasonal naïve"), "seasonal_naive")
  expect_equal(normalise_forecast_model_id("seasonal_naive"), "seasonal_naive")
})

test_that("safe_forecast_total never turns empty into zero", {
  expect_true(is.na(safe_forecast_total(numeric())))
  expect_true(is.na(safe_forecast_total(c(NA_real_, NA_real_))))
  expect_equal(safe_forecast_total(c(1, 2, NA)), 3)
  expect_identical(format_forecast_usd(safe_forecast_total(numeric())), "Unavailable")
  expect_false(identical(format_forecast_usd(safe_forecast_total(numeric())), "0.0 US$ millions"))
})

test_that("generate_series_model_forecast supports manual models including prophet", {
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 36)
  sid <- "DEU__CHN__8542__M"
  long <- data.table::data.table(
    series_id = sid,
    date = dates,
    trade_value_usd = 1000 + seq_along(dates) * 10,
    model_value_usd = 1000 + seq_along(dates) * 10,
    value_observed = TRUE,
    imputation_status = "none"
  )
  for (mid in c("seasonal_naive", "naive", "drift", "ets", "arima")) {
    fc <- generate_series_model_forecast(long, sid, mid, horizon = 6L)
    expect_equal(nrow(fc), 6L, info = mid)
    expect_true(any(is.finite(fc$predicted_value_usd)), info = mid)
    expect_true(is.finite(safe_forecast_total(fc$predicted_value_usd)), info = mid)
  }
  if (isTRUE(prophet_availability()$available)) {
    pfc <- generate_series_model_forecast(long, sid, "prophet", horizon = 6L)
    expect_equal(nrow(pfc), 6L)
    expect_true(all(is.finite(pfc$predicted_value_usd)))
  }
})

test_that("forecast module manual ETS replaces selected-model forecast", {
  skip_if_not_installed("shiny")
  dates <- seq.Date(as.Date("2020-01-01"), by = "month", length.out = 36)
  sid <- "DEU__CHN__8542__M"
  long <- data.table::data.table(
    series_id = sid,
    date = dates,
    trade_value_usd = 1000 + seq_along(dates) * 10,
    model_value_usd = 1000 + seq_along(dates) * 10,
    value_observed = TRUE,
    imputation_status = "none"
  )
  sn <- generate_series_model_forecast(long, sid, "seasonal_naive", horizon = 12L)
  fs <- list(
    monthly_long = long,
    quality = data.table::data.table(
      series_id = sid, expected_months = 36L, observed_months = 36L,
      missing_months = 0L, zero_months = 0L, completeness_pct = 100,
      longest_missing_run = 0L, recent_12_month_availability = 12L,
      stability_class = "stable", selection_or_rejection_reason = "ok",
      imputation_count = 0L
    ),
    selected = data.table::data.table(series_id = sid),
    metrics = data.table::data.table(
      series_id = rep(sid, 2), model_id = c("seasonal_naive", "ets"),
      horizon = 1L, mape = c(10, 9), mase = c(0.9, 0.8), smape = c(11, 10)
    ),
    selected_models = data.table::data.table(
      series_id = sid, selected_model_id = "seasonal_naive", selection_reason = "fixture"
    ),
    forecasts = sn,
    backtests = data.table::data.table(),
    residuals = data.table::data.table(series_id = sid, model_id = "seasonal_naive", residual = 1),
    profile = list(
      is_fixture = TRUE, data_mode = "fixture_synthetic",
      production_forecast_available = FALSE, fixture_accuracy_disclaimer = "fixture"
    ),
    candidates = data.table::data.table(series_id = sid),
    detailed_coverage = list(
      universe_checksum = "uv_test", production_status = "partial",
      represented_reporter_count = 6L, selected_reporter_count = 20L
    )
  )
  old <- load_forecast_ui_snapshot
  assign("load_forecast_ui_snapshot", function(cfg = NULL) fs, envir = .GlobalEnv)
  on.exit(assign("load_forecast_ui_snapshot", old, envir = .GlobalEnv), add = TRUE)

  shiny::testServer(mod_forecasting_server, args = list(
    snap = shiny::reactiveVal(list(detailed_coverage = fs$detailed_coverage)),
    cfg = shiny::reactive(list(paths = list(processed = tempdir())))
  ), {
    session$setInputs(
      series_id = sid,
      model_mode = "selected",
      model_id = "seasonal_naive",
      horizon_view = "1",
      interval_level = "80",
      show_imputed = TRUE
    )
    expect_equal(active_model(), "seasonal_naive")
    expect_true(nrow(active_forecast()) >= 1L)
    session$setInputs(model_mode = "manual", model_id = "ets")
    expect_equal(active_model(), "ets")
    fc <- active_forecast()
    expect_true(nrow(fc) >= 1L)
    expect_true(all(fc$model_id == "ets"))
    expect_true(is.finite(safe_forecast_total(fc$predicted_value_usd)))

    expect_identical(format_forecast_usd(safe_forecast_total(numeric())), "Unavailable")
  })
})
