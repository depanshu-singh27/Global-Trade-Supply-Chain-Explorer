forecast_mae <- function(actual, predicted) {
  actual <- sanitize_chart_numeric(actual)
  predicted <- sanitize_chart_numeric(predicted)
  ok <- is.finite(actual) & is.finite(predicted)
  if (!any(ok)) return(NA_real_)
  mean(abs(actual[ok] - predicted[ok]))
}

forecast_rmse <- function(actual, predicted) {
  actual <- sanitize_chart_numeric(actual)
  predicted <- sanitize_chart_numeric(predicted)
  ok <- is.finite(actual) & is.finite(predicted)
  if (!any(ok)) return(NA_real_)
  sqrt(mean((actual[ok] - predicted[ok])^2))
}

forecast_bias <- function(actual, predicted) {
  actual <- sanitize_chart_numeric(actual)
  predicted <- sanitize_chart_numeric(predicted)
  ok <- is.finite(actual) & is.finite(predicted)
  if (!any(ok)) return(NA_real_)
  mean(predicted[ok] - actual[ok])
}

forecast_smape <- function(actual, predicted) {
  actual <- sanitize_chart_numeric(actual)
  predicted <- sanitize_chart_numeric(predicted)
  ok <- is.finite(actual) & is.finite(predicted)
  if (!any(ok)) return(NA_real_)
  a <- actual[ok]; p <- predicted[ok]
  denom <- abs(a) + abs(p)
  keep <- denom > 0
  if (!any(keep)) return(NA_real_)
  mean(2 * abs(p[keep] - a[keep]) / denom[keep]) * 100
}

forecast_mape <- function(actual, predicted) {
  actual <- sanitize_chart_numeric(actual)
  predicted <- sanitize_chart_numeric(predicted)
  ok <- is.finite(actual) & is.finite(predicted) & actual > 0
  list(
    mape = if (any(ok)) mean(abs((predicted[ok] - actual[ok]) / actual[ok])) * 100 else NA_real_,
    valid_count = sum(ok),
    total_count = sum(is.finite(actual) & is.finite(predicted)),
    valid_pct = if (sum(is.finite(actual) & is.finite(predicted)) > 0) {
      100 * sum(ok) / sum(is.finite(actual) & is.finite(predicted))
    } else {
      NA_real_
    }
  )
}

forecast_mase <- function(actual, predicted, training, frequency = 12L) {
  actual <- sanitize_chart_numeric(actual)
  predicted <- sanitize_chart_numeric(predicted)
  training <- sanitize_chart_numeric(training)
  ok <- is.finite(actual) & is.finite(predicted)
  if (!any(ok)) return(NA_real_)
  mae <- mean(abs(actual[ok] - predicted[ok]))
  tr <- training[is.finite(training)]
  freq <- as.integer(frequency)
  if (length(tr) <= freq) {
    scale <- mean(abs(diff(tr)), na.rm = TRUE)
  } else {
    scale <- mean(abs(tr[(freq + 1):length(tr)] - tr[1:(length(tr) - freq)]), na.rm = TRUE)
  }
  if (!is.finite(scale) || scale == 0) return(NA_real_)
  mae / scale
}

forecast_interval_coverage <- function(actual, lower, upper) {
  actual <- sanitize_chart_numeric(actual)
  lower <- sanitize_chart_numeric(lower)
  upper <- sanitize_chart_numeric(upper)
  ok <- is.finite(actual) & is.finite(lower) & is.finite(upper)
  if (!any(ok)) return(NA_real_)
  mean(actual[ok] >= lower[ok] & actual[ok] <= upper[ok]) * 100
}

summarise_forecast_metrics <- function(pred_dt, training_by_series = NULL, frequency = 12L) {
  dt <- data.table::as.data.table(pred_dt)
  if (!nrow(dt)) return(data.table::data.table())
  dt[, .(
    fold_count = data.table::uniqueN(fold_id),
    prediction_count = .N,
    valid_prediction_count = sum(is.finite(actual_value_usd) & is.finite(predicted_value_usd)),
    mae = forecast_mae(actual_value_usd, predicted_value_usd),
    rmse = forecast_rmse(actual_value_usd, predicted_value_usd),
    bias = forecast_bias(actual_value_usd, predicted_value_usd),
    smape = forecast_smape(actual_value_usd, predicted_value_usd),
    mape = forecast_mape(actual_value_usd, predicted_value_usd)$mape,
    mape_valid_count = forecast_mape(actual_value_usd, predicted_value_usd)$valid_count,
    mape_valid_pct = forecast_mape(actual_value_usd, predicted_value_usd)$valid_pct,
    mase = {
      tr <- if (!is.null(training_by_series) && series_id[1] %in% names(training_by_series)) {
        training_by_series[[series_id[1]]]
      } else {
        numeric()
      }
      forecast_mase(actual_value_usd, predicted_value_usd, tr, frequency = frequency)
    },
    coverage_80 = forecast_interval_coverage(actual_value_usd, lower_80, upper_80),
    coverage_95 = forecast_interval_coverage(actual_value_usd, lower_95, upper_95),
    model_status = {
      st <- unique(model_status)
      if ("ok" %in% st) "ok" else st[1]
    }
  ), by = .(series_id, model_id, horizon)]
}
