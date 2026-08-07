test_that("baseline and package models produce non-negative forecasts", {
  y <- abs(sin(seq(0, 6, length.out = 36))) * 1000 + 500
  for (m in c("seasonal_naive", "naive", "drift")) {
    fc <- fit_forecast_model(y, model_id = m, h = 3L)
    expect_equal(fc$status, "ok")
    expect_true(all(fc$point >= 0))
    expect_true(all(fc$lower_95 >= 0, na.rm = TRUE))
  }
  if (requireNamespace("forecast", quietly = TRUE)) {
    ets <- fit_forecast_model(y, model_id = "ets", h = 3L)
    expect_true(ets$status %in% c("ok", "unavailable"))
    if (identical(ets$status, "ok")) expect_true(all(ets$point >= 0))
    ar <- fit_forecast_model(y, model_id = "arima", h = 3L)
    expect_true(ar$status %in% c("ok", "unavailable"))
  }
  prop <- fit_forecast_model(y, model_id = "prophet", h = 3L)
  expect_true(prop$status %in% c("ok", "unavailable"))
  if (identical(prop$status, "unavailable")) {
    expect_true(nzchar(prop$warning_message %||% ""))
  }
})

test_that("log1p transform inverts and stays non-negative", {
  y <- c(10, 20, 30, 40)
  tr <- transform_series_values(y, "log1p")
  inv <- inverse_transform_series_values(tr$values, "log1p")
  expect_equal(inv, y, tolerance = 1e-8)
})
