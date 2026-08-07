select_forecast_models <- function(metrics_dt,
                                     min_folds = 3L,
                                     min_valid_predictions = 3L,
                                     primary_metric = c("mase", "smape")) {
  primary_metric <- match.arg(primary_metric)
  dt <- data.table::as.data.table(metrics_dt)
  if (!nrow(dt)) return(data.table::data.table())

  use <- if (any(dt$horizon == 1L)) dt[horizon == 1L] else dt

  complexity <- c(
    seasonal_naive = 1L, naive = 2L, drift = 3L, ets = 4L, arima = 5L, prophet = 6L
  )

  rows <- lapply(unique(use$series_id), function(sid) {
    s <- use[series_id == sid]
    s[, eligible := model_status == "ok" &
        fold_count >= min_folds &
        valid_prediction_count >= min_valid_predictions &
        is.finite(get(primary_metric))]
    elig <- s[eligible == TRUE]
    if (!nrow(elig)) {
      return(data.table::data.table(
        series_id = sid,
        selected_model_id = NA_character_,
        selection_reason = "no_eligible_models",
        primary_metric = primary_metric,
        primary_metric_value = NA_real_
      ))
    }
    elig[, complexity_rank := complexity[model_id]]
    data.table::setorderv(
      elig,
      c(primary_metric, "rmse", "complexity_rank", "model_id"),
      c(1L, 1L, 1L, 1L)
    )
    win <- elig[1]
    data.table::data.table(
      series_id = sid,
      selected_model_id = win$model_id,
      selection_reason = sprintf(
        "lowest_%s_then_rmse_then_simpler_model", primary_metric
      ),
      primary_metric = primary_metric,
      primary_metric_value = win[[primary_metric]],
      rmse = win$rmse,
      smape = win$smape,
      mape = win$mape,
      mase = win$mase,
      fold_count = win$fold_count,
      mape_valid_count = win$mape_valid_count,
      engine_version = FORECAST_ENGINE_VERSION
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

generate_final_forecasts <- function(monthly_long,
                                       selected_models,
                                       horizon = 12L,
                                       prep_mode = "none",
                                       transform = "none") {
  long <- data.table::as.data.table(monthly_long)
  sel <- data.table::as.data.table(selected_models)
  horizon <- as.integer(horizon)
  if (!nrow(sel) || !nrow(long)) return(data.table::data.table())

  rows <- lapply(seq_len(nrow(sel)), function(i) {
    sid <- sel$series_id[i]
    mid <- sel$selected_model_id[i]
    if (!nzchar(mid %||% "")) return(NULL)
    sdt <- long[series_id == sid]
    data.table::setorderv(sdt, "date")
    prep <- prepare_forecast_model_input(sdt, mode = prep_mode)
    if (isTRUE(prep$rejected)) return(NULL)
    sdt <- prep$series
    y <- sdt$model_value_usd
    if (mid %in% c("ets", "arima", "prophet") && any(!is.finite(y))) {

      return(NULL)
    }
    last_date <- max(sdt$date[sdt$value_observed], na.rm = TRUE)
    fc <- fit_forecast_model(y, model_id = mid, h = horizon, transform = transform)
    if (!identical(fc$status, "ok")) return(NULL)
    fut_dates <- seq.Date(as.Date(last_date), by = "month", length.out = horizon + 1L)[-1]
    data.table::data.table(
      series_id = sid,
      model_id = mid,
      horizon = seq_len(horizon),
      forecast_date = as.character(fut_dates),
      date = fut_dates,
      predicted_value_usd = fc$point,
      lower_80 = fc$lower_80,
      upper_80 = fc$upper_80,
      lower_95 = fc$lower_95,
      upper_95 = fc$upper_95,
      last_observation_date = as.character(last_date),
      training_start = as.character(min(sdt$date)),
      training_end = as.character(last_date),
      engine_version = FORECAST_ENGINE_VERSION,
      generated_at = utc_now(),
      selection_reason = sel$selection_reason[i]
    )
  })
  data.table::rbindlist(Filter(Negate(is.null), rows), fill = TRUE)
}

generate_series_model_forecast <- function(monthly_long,
                                             series_id,
                                             model_id,
                                             horizon = 12L,
                                             prep_mode = "none",
                                             transform = "none") {
  sid <- as.character(series_id %||% "")[1]
  mid <- normalise_forecast_model_id(model_id)
  horizon <- as.integer(horizon %||% 12L)[1]
  if (!nzchar(sid) || is.na(mid) || !mid %in% forecast_model_ids()) {
    return(data.table::data.table())
  }
  long <- data.table::as.data.table(monthly_long)
  if (!nrow(long) || !"series_id" %in% names(long)) return(data.table::data.table())
  sdt <- long[series_id == sid]
  if (!nrow(sdt)) return(data.table::data.table())
  data.table::setorderv(sdt, "date")
  prep <- prepare_forecast_model_input(sdt, mode = prep_mode)
  if (isTRUE(prep$rejected)) return(data.table::data.table())
  sdt <- prep$series
  y <- sdt$model_value_usd
  if ("value_observed" %in% names(sdt) && any(sdt$value_observed %in% TRUE)) {
    last_date <- max(sdt$date[sdt$value_observed %in% TRUE], na.rm = TRUE)
  } else {
    last_date <- max(sdt$date, na.rm = TRUE)
  }
  if (!length(last_date) || is.na(last_date)) return(data.table::data.table())
  fc <- fit_forecast_model(y, model_id = mid, h = horizon, transform = transform)
  if (!identical(fc$status, "ok") || !any(is.finite(fc$point))) {
    return(data.table::data.table())
  }
  fut_dates <- seq.Date(as.Date(last_date), by = "month", length.out = horizon + 1L)[-1]
  data.table::data.table(
    series_id = sid,
    model_id = mid,
    horizon = seq_len(horizon),
    forecast_date = as.character(fut_dates),
    date = fut_dates,
    predicted_value_usd = fc$point,
    lower_80 = fc$lower_80,
    upper_80 = fc$upper_80,
    lower_95 = fc$lower_95,
    upper_95 = fc$upper_95,
    last_observation_date = as.character(last_date),
    training_start = as.character(min(sdt$date)),
    training_end = as.character(last_date),
    engine_version = FORECAST_ENGINE_VERSION,
    generated_at = utc_now(),
    model_status = fc$status,
    warning = fc$warning_message %||% NA_character_,
    selection_reason = "manual_session_fit"
  )
}
