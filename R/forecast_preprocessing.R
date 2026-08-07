prepare_forecast_model_input <- function(series_dt,
                                           mode = c("none", "short_gap_linear"),
                                           max_gap = 2L) {
  mode <- match.arg(mode)
  dt <- data.table::as.data.table(series_dt)
  data.table::setorderv(dt, "date")
  dt[, model_value_usd := trade_value_usd]
  dt[, imputation_status := "none"]
  if (identical(mode, "none")) {
    return(list(series = dt, imputed_months = 0L, rejected = FALSE, reason = NA_character_))
  }

  miss_run <- longest_run(dt$value_missing)
  if (miss_run > as.integer(max_gap)) {
    return(list(
      series = dt,
      imputed_months = 0L,
      rejected = TRUE,
      reason = sprintf("excessive_gap_%d_gt_%d", miss_run, max_gap)
    ))
  }

  vals <- dt$model_value_usd
  miss <- is.na(vals)
  if (any(miss) && any(!miss)) {
    idx <- seq_along(vals)

    vals_imp <- approx(idx[!miss], vals[!miss], xout = idx, method = "linear", rule = 1)$y

    first_obs <- which(!miss)[1]
    last_obs <- utils::tail(which(!miss), 1)
    vals_imp[idx < first_obs | idx > last_obs] <- NA_real_
    changed <- miss & !is.na(vals_imp)
    dt[changed, `:=`(
      model_value_usd = vals_imp[changed],
      imputation_status = "short_gap_linear"
    )]
  }
  list(
    series = dt,
    imputed_months = sum(dt$imputation_status != "none"),
    rejected = FALSE,
    reason = NA_character_
  )
}

transform_series_values <- function(x, transform = c("none", "log1p")) {
  transform <- match.arg(transform)
  x <- sanitize_chart_numeric(x)
  if (identical(transform, "log1p")) {
    return(list(values = log1p(pmax(0, x)), transform = "log1p"))
  }
  list(values = x, transform = "none")
}

inverse_transform_series_values <- function(x, transform = c("none", "log1p")) {
  transform <- match.arg(transform)
  x <- sanitize_chart_numeric(x)
  if (identical(transform, "log1p")) {
    return(pmax(0, expm1(x)))
  }
  pmax(0, x)
}
