empty_forecast_result <- function(model_id, h, status = "unavailable", warning = NA_character_) {
  list(
    model_id = model_id,
    status = status,
    warning_message = warning,
    point = rep(NA_real_, h),
    lower_80 = rep(NA_real_, h),
    upper_80 = rep(NA_real_, h),
    lower_95 = rep(NA_real_, h),
    upper_95 = rep(NA_real_, h),
    fitted = NULL,
    transform = "none"
  )
}

fit_forecast_model <- function(y,
                                 model_id = "seasonal_naive",
                                 h = 1L,
                                 frequency = 12L,
                                 transform = "none",
                                 level = c(80, 95)) {
  model_id <- as.character(model_id)[1]
  h <- as.integer(h)[1]
  y <- sanitize_chart_numeric(as.numeric(y))
  if (!length(y) || sum(is.finite(y)) < 3L) {
    return(empty_forecast_result(model_id, h, "unavailable", "insufficient_history"))
  }

  y_obs <- y
  tr <- transform_series_values(y_obs, transform = transform)
  z <- tr$values
  z[!is.finite(z)] <- NA_real_

  out <- switch(
    model_id,
    seasonal_naive = forecast_seasonal_naive(z, h = h, frequency = frequency),
    naive = forecast_naive(z, h = h),
    drift = forecast_drift(z, h = h),
    ets = forecast_ets(z, h = h, frequency = frequency),
    arima = forecast_arima(z, h = h, frequency = frequency),
    prophet = forecast_prophet(z, h = h, frequency = frequency),
    empty_forecast_result(model_id, h, "unavailable", "unknown_model")
  )
  out$transform <- tr$transform

  for (nm in c("point", "lower_80", "upper_80", "lower_95", "upper_95")) {
    if (!is.null(out[[nm]])) {
      out[[nm]] <- inverse_transform_series_values(out[[nm]], transform = tr$transform)
      out[[nm]] <- pmax(0, out[[nm]])
    }
  }

  out$lower_80 <- pmin(out$lower_80, out$point, na.rm = FALSE)
  out$upper_80 <- pmax(out$upper_80, out$point, na.rm = FALSE)
  out$lower_95 <- pmin(out$lower_95, out$lower_80, na.rm = FALSE)
  out$upper_95 <- pmax(out$upper_95, out$upper_80, na.rm = FALSE)
  out$model_id <- model_id
  out
}

forecast_seasonal_naive <- function(y, h = 1L, frequency = 12L) {
  y <- as.numeric(y)
  n <- length(y)
  freq <- as.integer(frequency)
  point <- rep(NA_real_, h)
  for (i in seq_len(h)) {
    idx <- n - freq + i
    while (idx > 0 && !is.finite(y[idx])) idx <- idx - freq
    point[i] <- if (idx > 0) y[idx] else utils::tail(y[is.finite(y)], 1)
  }
  resid <- y - c(rep(NA_real_, freq), utils::head(y, n - freq))
  s <- stats::sd(resid, na.rm = TRUE)
  if (!is.finite(s)) s <- 0
  list(
    model_id = "seasonal_naive",
    status = "ok",
    warning_message = NA_character_,
    point = point,
    lower_80 = pmax(0, point - 1.28 * s),
    upper_80 = point + 1.28 * s,
    lower_95 = pmax(0, point - 1.96 * s),
    upper_95 = point + 1.96 * s,
    fitted = NULL
  )
}

forecast_naive <- function(y, h = 1L) {
  last <- utils::tail(y[is.finite(y)], 1)
  if (!length(last)) return(empty_forecast_result("naive", h, "unavailable", "no_finite_values"))
  diffs <- diff(y[is.finite(y)])
  s <- stats::sd(diffs, na.rm = TRUE)
  if (!is.finite(s)) s <- 0
  point <- rep(last, h)
  list(
    model_id = "naive", status = "ok", warning_message = NA_character_,
    point = point,
    lower_80 = pmax(0, point - 1.28 * s * sqrt(seq_len(h))),
    upper_80 = point + 1.28 * s * sqrt(seq_len(h)),
    lower_95 = pmax(0, point - 1.96 * s * sqrt(seq_len(h))),
    upper_95 = point + 1.96 * s * sqrt(seq_len(h)),
    fitted = NULL
  )
}

forecast_drift <- function(y, h = 1L) {
  yf <- y[is.finite(y)]
  if (length(yf) < 2L) return(empty_forecast_result("drift", h, "unavailable", "need_two_points"))
  n <- length(yf)
  drift <- (yf[n] - yf[1]) / (n - 1)
  point <- yf[n] + drift * seq_len(h)
  diffs <- diff(yf)
  s <- stats::sd(diffs, na.rm = TRUE)
  if (!is.finite(s)) s <- 0
  list(
    model_id = "drift", status = "ok", warning_message = NA_character_,
    point = point,
    lower_80 = pmax(0, point - 1.28 * s * sqrt(seq_len(h))),
    upper_80 = point + 1.28 * s * sqrt(seq_len(h)),
    lower_95 = pmax(0, point - 1.96 * s * sqrt(seq_len(h))),
    upper_95 = point + 1.96 * s * sqrt(seq_len(h)),
    fitted = NULL
  )
}

forecast_ets <- function(y, h = 1L, frequency = 12L) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    return(empty_forecast_result("ets", h, "unavailable", "forecast_package_missing"))
  }
  yf <- y

  if (any(!is.finite(yf))) {
    return(empty_forecast_result("ets", h, "unavailable", "missing_values_in_training"))
  }
  ts_y <- stats::ts(yf, frequency = frequency)
  fit <- tryCatch(forecast::ets(ts_y), error = function(e) e)
  if (inherits(fit, "error")) {
    return(empty_forecast_result("ets", h, "unavailable", conditionMessage(fit)))
  }
  fc <- tryCatch(forecast::forecast(fit, h = h, level = c(80, 95)), error = function(e) e)
  if (inherits(fc, "error")) {
    return(empty_forecast_result("ets", h, "unavailable", conditionMessage(fc)))
  }
  list(
    model_id = "ets", status = "ok", warning_message = NA_character_,
    point = as.numeric(fc$mean),
    lower_80 = as.numeric(fc$lower[, 1]),
    upper_80 = as.numeric(fc$upper[, 1]),
    lower_95 = as.numeric(fc$lower[, 2]),
    upper_95 = as.numeric(fc$upper[, 2]),
    fitted = as.numeric(fitted(fit))
  )
}

forecast_arima <- function(y, h = 1L, frequency = 12L) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    return(empty_forecast_result("arima", h, "unavailable", "forecast_package_missing"))
  }
  if (any(!is.finite(y))) {
    return(empty_forecast_result("arima", h, "unavailable", "missing_values_in_training"))
  }
  ts_y <- stats::ts(y, frequency = frequency)
  fit <- tryCatch(forecast::auto.arima(ts_y, seasonal = TRUE, stepwise = TRUE, approximation = TRUE),
                  error = function(e) e)
  if (inherits(fit, "error")) {
    return(empty_forecast_result("arima", h, "unavailable", conditionMessage(fit)))
  }
  fc <- tryCatch(forecast::forecast(fit, h = h, level = c(80, 95)), error = function(e) e)
  if (inherits(fc, "error")) {
    return(empty_forecast_result("arima", h, "unavailable", conditionMessage(fc)))
  }
  list(
    model_id = "arima", status = "ok", warning_message = NA_character_,
    point = as.numeric(fc$mean),
    lower_80 = as.numeric(fc$lower[, 1]),
    upper_80 = as.numeric(fc$upper[, 1]),
    lower_95 = as.numeric(fc$lower[, 2]),
    upper_95 = as.numeric(fc$upper[, 2]),
    fitted = as.numeric(fitted(fit))
  )
}

forecast_prophet <- function(y, h = 1L, frequency = 12L) {
  av <- prophet_availability()
  if (!isTRUE(av$available)) {
    return(empty_forecast_result("prophet", h, "unavailable", av$reason))
  }
  if (sum(is.finite(y)) < 24L) {
    return(empty_forecast_result("prophet", h, "unavailable", "insufficient_history_for_prophet"))
  }

  n <- length(y)
  ds <- seq.Date(as.Date("2019-01-01"), by = "month", length.out = n)
  df <- data.frame(ds = ds, y = as.numeric(y))
  df <- df[is.finite(df$y), , drop = FALSE]
  fit <- tryCatch({
    m <- prophet::prophet(df, weekly.seasonality = FALSE, daily.seasonality = FALSE, yearly.seasonality = TRUE)
    m
  }, error = function(e) e)
  if (inherits(fit, "error")) {
    return(empty_forecast_result("prophet", h, "unavailable", conditionMessage(fit)))
  }
  future <- tryCatch(prophet::make_future_dataframe(fit, periods = h, freq = "month"), error = function(e) e)
  if (inherits(future, "error")) {
    return(empty_forecast_result("prophet", h, "unavailable", conditionMessage(future)))
  }
  pred <- tryCatch(stats::predict(fit, future), error = function(e) e)
  if (inherits(pred, "error")) {
    return(empty_forecast_result("prophet", h, "unavailable", conditionMessage(pred)))
  }
  tail_pred <- utils::tail(pred, h)
  list(
    model_id = "prophet", status = "ok", warning_message = NA_character_,
    point = as.numeric(tail_pred$yhat),
    lower_80 = pmax(0, as.numeric(tail_pred$yhat_lower)),
    upper_80 = as.numeric(tail_pred$yhat_upper),
    lower_95 = pmax(0, as.numeric(tail_pred$yhat_lower)),
    upper_95 = as.numeric(tail_pred$yhat_upper),
    fitted = NULL
  )
}
