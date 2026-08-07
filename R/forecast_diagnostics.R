compute_forecast_residual_diagnostics <- function(monthly_long,
                                                    selected_models,
                                                    prep_mode = "none") {
  long <- data.table::as.data.table(monthly_long)
  sel <- data.table::as.data.table(selected_models)
  if (!nrow(sel) || !nrow(long)) return(data.table::data.table())

  rows <- lapply(seq_len(nrow(sel)), function(i) {
    sid <- sel$series_id[i]
    mid <- sel$selected_model_id[i]
    if (!nzchar(mid %||% "")) return(NULL)
    sdt <- long[series_id == sid]
    data.table::setorderv(sdt, "date")
    prep <- prepare_forecast_model_input(sdt, mode = prep_mode)
    y <- prep$series$model_value_usd
    y <- y[is.finite(y)]
    if (length(y) < 12L) return(NULL)
    fc <- fit_forecast_model(y, model_id = mid, h = 1L)
    fitted <- fc$fitted
    if (is.null(fitted) || length(fitted) != length(y)) {

      resid <- c(NA_real_, diff(y))
    } else {
      resid <- y - as.numeric(fitted)
    }
    resid <- resid[is.finite(resid)]
    if (!length(resid)) return(NULL)
    lb <- tryCatch({
      bt <- stats::Box.test(resid, lag = min(10L, length(resid) - 1L), type = "Ljung-Box")
      list(statistic = unname(bt$statistic), p_value = bt$p.value)
    }, error = function(e) list(statistic = NA_real_, p_value = NA_real_))
    data.table::data.table(
      series_id = sid,
      model_id = mid,
      residual_mean = mean(resid),
      residual_sd = stats::sd(resid),
      residual_ac1 = if (length(resid) > 1) stats::cor(resid[-1], resid[-length(resid)]) else NA_real_,
      ljung_box_statistic = lb$statistic,
      ljung_box_p_value = lb$p_value,
      outlier_residual_count = sum(abs(resid) > 3 * stats::sd(resid), na.rm = TRUE),
      engine_version = FORECAST_ENGINE_VERSION
    )
  })
  data.table::rbindlist(Filter(Negate(is.null), rows), fill = TRUE)
}

validate_phase12_forecasts <- function(monthly_long,
                                         quality,
                                         selected,
                                         backtests,
                                         metrics,
                                         selected_models,
                                         forecasts) {
  checks <- list()
  ml <- data.table::as.data.table(monthly_long)
  checks$one_row_per_series_month <- {
    if (!nrow(ml)) TRUE else {
      u <- ml[, .N, by = .(series_id, period)]
      all(u$N == 1L)
    }
  }
  checks$hs_character <- !nrow(ml) || is.character(ml$hs_code)
  checks$no_world <- !nrow(ml) || !any(ml$partner_iso3 %in% c("WLD", "W00"))
  checks$no_self <- !nrow(ml) || !any(ml$partner_iso3 == ml$reporter_iso3)
  checks$no_negative <- !nrow(ml) || !any(ml$trade_value_usd < 0, na.rm = TRUE)
  checks$missing_zero_distinct <- !nrow(ml) || {
    all(ml$value_missing == is.na(ml$trade_value_usd)) &&
      all(ml$value_zero == (!is.na(ml$trade_value_usd) & ml$trade_value_usd == 0))
  }
  bt <- data.table::as.data.table(backtests)
  checks$no_leakage <- !nrow(bt) || {
    all(as.Date(bt$forecast_date) > as.Date(bt$training_end), na.rm = TRUE)
  }
  fc <- data.table::as.data.table(forecasts)
  checks$forecast_after_history <- !nrow(fc) || {
    all(as.Date(fc$forecast_date) > as.Date(fc$last_observation_date), na.rm = TRUE)
  }
  checks$interval_ordered <- !nrow(fc) || {
    all(fc$lower_95 <= fc$lower_80 + 1e-8, na.rm = TRUE) &&
      all(fc$lower_80 <= fc$predicted_value_usd + 1e-8, na.rm = TRUE) &&
      all(fc$predicted_value_usd <= fc$upper_80 + 1e-8, na.rm = TRUE) &&
      all(fc$upper_80 <= fc$upper_95 + 1e-8, na.rm = TRUE)
  }
  checks$nonneg_lower <- !nrow(fc) || all(fc$lower_95 >= -1e-8, na.rm = TRUE)

  data.table::data.table(
    check_id = names(checks),
    status = ifelse(unlist(checks), "pass", "error"),
    message = names(checks),
    checked_at = utc_now()
  )
}
