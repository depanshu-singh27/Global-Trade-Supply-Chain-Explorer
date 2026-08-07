forecast_history_chart <- function(history_dt,
                                     forecast_dt = NULL,
                                     show_imputed = TRUE,
                                     title = "Historical reported trade and forecast") {
  hist <- data.table::as.data.table(history_dt)
  empty <- plotly::plot_ly(type = "scatter", mode = "lines") |>
    plotly::layout(title = list(text = "No series selected", font = list(size = 13)))
  if (!nrow(hist)) return(empty)

  p <- plotly::plot_ly()
  obs <- hist[value_observed == TRUE]
  if (nrow(obs)) {
    p <- plotly::add_trace(
      p, data = obs, x = ~date, y = ~trade_value_usd,
      type = "scatter", mode = "lines+markers", name = "Observed",
      line = list(color = "#0B3D5C"), marker = list(size = 5)
    )
  }
  if (isTRUE(show_imputed) && "imputation_status" %in% names(hist)) {
    imp <- hist[imputation_status != "none"]
    if (nrow(imp)) {
      p <- plotly::add_trace(
        p, data = imp, x = ~date, y = ~model_value_usd,
        type = "scatter", mode = "markers", name = "Imputed model input",
        marker = list(color = "#B45309", symbol = "x", size = 8)
      )
    }
  }
  fc <- data.table::as.data.table(forecast_dt)
  if (nrow(fc) && any(is.finite(fc$predicted_value_usd))) {
    p <- plotly::add_ribbons(
      p, data = fc, x = ~date, ymin = ~lower_95, ymax = ~upper_95,
      name = "95% interval", line = list(color = "transparent"),
      fillcolor = "rgba(74,144,164,0.15)"
    )
    p <- plotly::add_ribbons(
      p, data = fc, x = ~date, ymin = ~lower_80, ymax = ~upper_80,
      name = "80% interval", line = list(color = "transparent"),
      fillcolor = "rgba(74,144,164,0.30)"
    )
    p <- plotly::add_trace(
      p, data = fc, x = ~date, y = ~predicted_value_usd,
      type = "scatter", mode = "lines+markers", name = "Point forecast",
      line = list(color = "#2F6F7E", dash = "dash")
    )
  } else {
    p <- plotly::layout(
      p,
      annotations = list(list(
        text = "No forecast available for the active model",
        xref = "paper", yref = "paper", x = 0.5, y = 0.95,
        showarrow = FALSE, font = list(size = 12, color = "#5B6B7C")
      ))
    )
  }
  p |>
    plotly::layout(
      title = list(text = title, font = list(size = 13)),
      xaxis = list(title = "Month"),
      yaxis = list(title = "Trade value (current US$)"),
      legend = list(orientation = "h"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}

forecast_chart_text_summary <- function(history_dt, forecast_dt = NULL, model_id = NULL) {
  hist <- data.table::as.data.table(history_dt)
  if (!nrow(hist)) return("No historical series available.")
  last <- hist[value_observed == TRUE][.N]
  fc <- data.table::as.data.table(forecast_dt)
  nxt <- if (nrow(fc)) fc[1]$predicted_value_usd else NA_real_
  sprintf(
    paste(
      "Latest observed monthly value %s on %s. Model %s next-month point forecast %s.",
      "Values are statistical extrapolations of historical reported trade, not realised outcomes."
    ),
    format_forecast_usd(last$trade_value_usd),
    as.character(last$date),
    model_id %||% "unspecified",
    format_forecast_usd(nxt)
  )
}

forecast_leaderboard_order <- function(metrics_dt, selected_model_id = NULL) {
  dt <- data.table::as.data.table(metrics_dt)
  if (!nrow(dt)) return(dt)
  dt[, selected := !is.null(selected_model_id) & model_id == selected_model_id]
  data.table::setorderv(dt, c("selected", "mase", "smape", "model_id"), c(-1L, 1L, 1L, 1L))
  dt
}
