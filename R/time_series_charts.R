ts_empty_plotly <- function(msg = "No observations for the current filters.") {
  overview_empty_plotly(msg)
}

ts_main_plotly <- function(series_dt, title, metric, transform = "absolute",
                             show_zero = FALSE) {
  p <- gte_palette()
  dt <- data.table::as.data.table(series_dt)
  if (!nrow(dt) || !("display_value" %in% names(dt) || "value" %in% names(dt))) {
    return(ts_empty_plotly())
  }
  if (!"display_value" %in% names(dt)) dt[, display_value := value]
  if (!"series" %in% names(dt)) dt[, series := "Series"]
  df <- as.data.frame(dt)
  ytitle <- switch(
    as.character(transform),
    "yoy" = "Year-over-year %",
    "balance_change" = "Balance change (current US$)",
    "index" = "Index (baseline = 100)",
    "share" = "Share of selected total (%)",
    ts_metric_label(metric)
  )
  fig <- plotly::plot_ly(
    df, x = ~year, y = ~display_value, color = ~series,
    type = "scatter", mode = "lines+markers",
    colors = c(p$imports, p$exports, p$accent, p$accent_soft, p$surplus, p$deficit, p$total, p$warn),
    hovertemplate = paste0("%{fullData.name}<br>Year %{x}<br>", ytitle, ": %{y:.2f}<extra></extra>")
  )
  lay <- list(
    title = list(text = title, font = list(size = 15, color = p$ink)),
    xaxis = list(title = "Year", dtick = 1, gridcolor = p$grid),
    yaxis = list(title = ytitle, gridcolor = p$grid,
                 zeroline = isTRUE(show_zero) || ts_metric_is_signed(metric) ||
                   transform %in% c("yoy", "balance_change"),
                 zerolinecolor = p$zero),
    legend = list(orientation = "h", y = -0.22),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 60, r = 20, t = 48, b = 60)
  )
  do.call(plotly::layout, c(list(p = fig), lay)) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

ts_decomposition_plotly <- function(decomp, title) {
  p <- gte_palette()
  if (is.null(decomp) || !nrow(decomp)) return(ts_empty_plotly())
  df <- as.data.frame(decomp)
  plotly::plot_ly(df, x = ~year) |>
    plotly::add_bars(y = ~imports, name = "Imports", marker = list(color = p$imports)) |>
    plotly::add_bars(y = ~exports, name = "Exports", marker = list(color = p$exports)) |>
    plotly::add_trace(
      y = ~balance, name = "Balance", type = "scatter", mode = "lines+markers",
      line = list(color = p$balance_neg, width = 2), yaxis = "y2"
    ) |>
    plotly::layout(
      barmode = "group",
      title = list(text = title, font = list(size = 15, color = p$ink)),
      yaxis = list(title = "Current US$", gridcolor = p$grid),
      yaxis2 = list(overlaying = "y", side = "right", title = "Balance",
                    zeroline = TRUE, zerolinecolor = p$zero, showgrid = FALSE),
      legend = list(orientation = "h", y = -0.2),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

ts_treemap_plotly <- function(prep, title) {
  p <- gte_palette()
  dt <- prep$data
  if (is.null(dt) || !nrow(dt) || !(prep$total > 0)) {
    return(ts_empty_plotly("Treemap unavailable (zero or missing filtered total)."))
  }
  df <- as.data.frame(dt)
  plotly::plot_ly(
    type = "treemap",
    labels = df$label,
    parents = rep("", nrow(df)),
    values = df$value,
    textinfo = "label+percent root",
    hovertemplate = paste0(
      "%{label}<br>", format_usd_compact(df$value),
      " (%{percentRoot:.1%})<extra></extra>"
    ),
    marker = list(colors = ifelse(df$hs_code == "OTHER", p$muted, p$accent_soft),
                  line = list(color = "#FFFFFF", width = 1))
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 15, color = p$ink)),
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = 48, l = 10, r = 10, b = 10)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

ts_movers_plotly <- function(movers_dt, value_col, title, color) {
  p <- gte_palette()
  if (is.null(movers_dt) || !nrow(movers_dt)) return(ts_empty_plotly())
  df <- as.data.frame(movers_dt)
  if (!value_col %in% names(df) || !"hs_code" %in% names(df)) {
    return(ts_empty_plotly("Mover chart unavailable for the current filters."))
  }
  df$hs_code <- as.character(df$hs_code)
  if ("commodity_key" %in% names(df)) {
    df$lab <- as.character(df$commodity_key)
  } else {
    desc <- if ("commodity_description" %in% names(df)) df$commodity_description else NA_character_
    df$lab <- commodity_mover_key(df$hs_code, desc)
  }

  if (anyDuplicated(df$hs_code) || anyDuplicated(df$lab)) {
    return(ts_empty_plotly("Mover ranking has duplicate commodity identities; refresh filters."))
  }
  df <- df[order(df[[value_col]], df$hs_code), , drop = FALSE]
  lev <- unique(as.character(df$lab))
  plotly::plot_ly(
    df,
    x = df[[value_col]],
    y = ~factor(lab, levels = lev),
    type = "bar", orientation = "h",
    marker = list(color = color),
    hovertemplate = "%{y}<br>%{x:.2f}<extra></extra>"
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 13, color = p$ink)),
      xaxis = list(title = "", zeroline = TRUE, zerolinecolor = p$zero),
      yaxis = list(title = "", automargin = TRUE),
      margin = list(l = 120, t = 40, b = 30),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}
