build_rolling_origin_folds <- function(dates,
                                         min_train = 24L,
                                         horizons = c(1L, 3L, 6L),
                                         step = 3L) {
  dates <- as.Date(dates)
  n <- length(dates)
  min_train <- as.integer(min_train)
  step <- as.integer(step)
  horizons <- as.integer(horizons)
  folds <- list()
  fold_id <- 0L
  origin <- min_train
  while (origin < n) {
    for (h in horizons) {
      test_idx <- origin + h
      if (test_idx > n) next
      fold_id <- fold_id + 1L
      folds[[length(folds) + 1L]] <- data.table::data.table(
        fold_id = fold_id,
        training_end_idx = origin,
        training_start = as.character(dates[1]),
        training_end = as.character(dates[origin]),
        forecast_date = as.character(dates[test_idx]),
        horizon = h,
        test_idx = test_idx
      )
    }
    origin <- origin + step
  }
  if (!length(folds)) return(data.table::data.table())
  data.table::rbindlist(folds)
}

run_series_backtest <- function(series_dt,
                                  models = c("seasonal_naive", "naive", "drift", "ets", "arima"),
                                  min_train = 24L,
                                  horizons = c(1L, 3L, 6L),
                                  step = 3L,
                                  transform = "none",
                                  prep_mode = "none") {
  dt <- data.table::as.data.table(series_dt)
  data.table::setorderv(dt, "date")
  prep <- prepare_forecast_model_input(dt, mode = prep_mode)
  if (isTRUE(prep$rejected)) {
    return(data.table::data.table(
      series_id = dt$series_id[1],
      model_id = NA_character_,
      model_status = "rejected_series",
      warning_message = prep$reason
    ))
  }
  sdt <- prep$series

  y_all <- sdt$model_value_usd
  dates <- sdt$date
  folds <- build_rolling_origin_folds(dates, min_train = min_train, horizons = horizons, step = step)
  if (!nrow(folds)) return(data.table::data.table())

  rows <- list()
  for (m in models) {
    for (i in seq_len(nrow(folds))) {
      f <- folds[i]
      train_y <- y_all[seq_len(f$training_end_idx)]

      if (!(dates[f$test_idx] > dates[f$training_end_idx])) next
      h <- f$horizon

      use_transform <- transform
      if (m %in% c("ets", "arima", "prophet") && any(!is.finite(train_y))) {
        fc <- empty_forecast_result(m, h, "unavailable", "missing_in_training_fold")
      } else {
        fc <- fit_forecast_model(train_y, model_id = m, h = h, transform = use_transform)
      }
      actual <- sdt$trade_value_usd[f$test_idx]
      pred <- fc$point[h]
      err <- if (is.finite(actual) && is.finite(pred)) pred - actual else NA_real_
      mape_ok <- is.finite(actual) && actual > 0 && is.finite(pred)
      rows[[length(rows) + 1L]] <- data.table::data.table(
        series_id = sdt$series_id[1],
        model_id = m,
        fold_id = f$fold_id,
        training_start = f$training_start,
        training_end = f$training_end,
        forecast_date = f$forecast_date,
        horizon = h,
        actual_value_usd = actual,
        predicted_value_usd = pred,
        lower_80 = fc$lower_80[h],
        upper_80 = fc$upper_80[h],
        lower_95 = fc$lower_95[h],
        upper_95 = fc$upper_95[h],
        error = err,
        absolute_error = if (is.finite(err)) abs(err) else NA_real_,
        percentage_error = if (isTRUE(mape_ok)) 100 * abs(err) / actual else NA_real_,
        valid_mape_observation = mape_ok,
        model_status = fc$status,
        warning_message = fc$warning_message,
        engine_version = FORECAST_ENGINE_VERSION,
        package_versions = jsonlite::toJSON(forecast_package_versions(), auto_unbox = TRUE)
      )
    }
  }
  if (!length(rows)) return(data.table::data.table())
  data.table::rbindlist(rows, fill = TRUE)
}

run_forecast_backtests <- function(monthly_long,
                                     selected_series,
                                     models = c("seasonal_naive", "naive", "drift", "ets", "arima", "prophet"),
                                     ...) {
  sel <- data.table::as.data.table(selected_series)
  long <- data.table::as.data.table(monthly_long)
  if (!nrow(sel) || !nrow(long)) return(data.table::data.table())
  ids <- unique(sel$series_id)
  out <- lapply(ids, function(sid) {
    run_series_backtest(long[series_id == sid], models = models, ...)
  })
  data.table::rbindlist(out, fill = TRUE)
}
