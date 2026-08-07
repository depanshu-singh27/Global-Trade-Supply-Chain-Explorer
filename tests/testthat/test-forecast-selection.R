test_that("final forecasts start after history and intervals order", {
  bundle <- make_forecast_fixture_bundle(2L)
  q <- compute_monthly_series_quality(bundle$monthly_long)
  sel <- select_stable_forecast_series(q, bundle$candidates, n = 2L)
  bt <- run_forecast_backtests(
    bundle$monthly_long, sel,
    models = c("seasonal_naive", "naive", "drift"),
    min_train = 24L, horizons = 1L, step = 12L
  )
  train_map <- lapply(unique(sel$series_id), function(sid) {
    s <- bundle$monthly_long[series_id == sid][order(date)]
    s$trade_value_usd
  })
  names(train_map) <- unique(sel$series_id)
  met <- summarise_forecast_metrics(bt, training_by_series = train_map)
  sm <- select_forecast_models(met, min_folds = 1L, min_valid_predictions = 1L)
  expect_true(any(!is.na(sm$selected_model_id)))
  fc <- generate_final_forecasts(bundle$monthly_long, sm, horizon = 6L)
  expect_true(nrow(fc) > 0)
  expect_true(all(as.Date(fc$forecast_date) > as.Date(fc$last_observation_date)))
  expect_true(all(fc$lower_95 <= fc$upper_95 + 1e-8, na.rm = TRUE))
  expect_true(all(fc$lower_95 >= -1e-8, na.rm = TRUE))
})
