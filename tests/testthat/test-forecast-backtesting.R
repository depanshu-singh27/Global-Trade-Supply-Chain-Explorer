test_that("metrics and MAPE zero handling", {
  actual <- c(100, 0, 50, NA_real_)
  pred <- c(110, 5, 40, 10)
  expect_true(is.finite(forecast_mae(actual, pred)))
  expect_true(is.finite(forecast_rmse(actual, pred)))
  expect_true(is.finite(forecast_smape(actual, pred)))
  mp <- forecast_mape(actual, pred)
  expect_equal(mp$valid_count, 2L)
  expect_true(is.finite(mp$mape))
  mase <- forecast_mase(c(10, 12), c(11, 13), training = 1:24, frequency = 12)
  expect_true(is.finite(mase) || is.na(mase))
  cov <- forecast_interval_coverage(c(10, 20), c(5, 15), c(15, 25))
  expect_equal(cov, 100)
})

test_that("rolling folds have no leakage and selection is deterministic", {
  bundle <- make_forecast_fixture_bundle(3L)
  sid <- bundle$candidates$series_id[1]
  s <- bundle$monthly_long[series_id == sid]
  bt <- run_series_backtest(
    s, models = c("seasonal_naive", "naive", "drift"),
    min_train = 24L, horizons = c(1L, 3L), step = 6L
  )
  expect_true(nrow(bt) > 0)
  expect_true(all(as.Date(bt$forecast_date) > as.Date(bt$training_end)))
  train_map <- list()
  train_map[[sid]] <- s$trade_value_usd
  met <- summarise_forecast_metrics(bt, training_by_series = train_map)
  s1 <- select_forecast_models(met)
  s2 <- select_forecast_models(met)
  expect_equal(s1$selected_model_id, s2$selected_model_id)
})
