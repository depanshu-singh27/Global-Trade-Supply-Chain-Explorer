overview_empty_plotly <- function(message = "No data available for the current filters.") {
  p <- gte_palette()
  plotly::plotly_empty(type = "scatter") |>
    plotly::layout(
      annotations = list(
        list(
          text = message, xref = "paper", yref = "paper",
          x = 0.5, y = 0.5, showarrow = FALSE,
          font = list(size = 13, color = p$muted)
        )
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

overview_trend_plotly <- function(series, title, show_balance = FALSE) {
  p <- gte_palette()
  if (is.null(series) || !nrow(series)) return(overview_empty_plotly())
  fig <- plotly::plot_ly(data = as.data.frame(series), x = ~year)
  fig <- fig |>
    plotly::add_trace(
      y = ~imports, name = "Imports", type = "scatter", mode = "lines+markers",
      line = list(color = p$imports, width = 2),
      marker = list(size = 7),
      hovertemplate = "Year %{x}<br>Imports: %{customdata}<extra></extra>",
      customdata = format_usd_compact(series$imports)
    ) |>
    plotly::add_trace(
      y = ~exports, name = "Exports", type = "scatter", mode = "lines+markers",
      line = list(color = p$exports, width = 2),
      marker = list(size = 7),
      hovertemplate = "Year %{x}<br>Exports: %{customdata}<extra></extra>",
      customdata = format_usd_compact(series$exports)
    ) |>
    plotly::add_trace(
      y = ~total, name = "Total trade", type = "scatter", mode = "lines+markers",
      line = list(color = p$total, width = 2.5, dash = "dot"),
      marker = list(size = 7),
      hovertemplate = "Year %{x}<br>Total: %{customdata}<extra></extra>",
      customdata = format_usd_compact(series$total)
    )
  if (isTRUE(show_balance)) {
    fig <- fig |>
      plotly::add_trace(
        y = ~balance, name = "Trade balance", type = "scatter", mode = "lines+markers",
        yaxis = "y2",
        line = list(color = p$balance_neg, width = 2),
        marker = list(size = 6),
        hovertemplate = "Year %{x}<br>Balance: %{customdata}<extra></extra>",
        customdata = format_usd_compact(series$balance)
      )
    lay <- plotly_layout_base(title, p)
    lay$yaxis2 <- list(
      overlaying = "y", side = "right", title = "Balance (current US$)",
      gridcolor = "rgba(0,0,0,0)", zeroline = TRUE, zerolinecolor = p$zero
    )
    lay$yaxis$title <- "Trade value (current US$)"
    fig <- do.call(plotly::layout, c(list(p = fig), lay))
  } else {
    lay <- plotly_layout_base(title, p)
    lay$yaxis$title <- "Trade value (current US$)"
    fig <- do.call(plotly::layout, c(list(p = fig), lay))
  }
  fig |> plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

overview_ranking_plotly <- function(ranked, title, measure = "total") {
  p <- gte_palette()
  if (is.null(ranked) || !nrow(ranked)) return(overview_empty_plotly())
  df <- as.data.frame(ranked)
  df$label <- paste0(df$reporter_name, " (", df$reporter_iso3, ")")
  df <- df[order(df$value, df$reporter_iso3), , drop = FALSE]
  colors <- if (identical(measure, "balance")) {
    ifelse(df$value >= 0, p$surplus, p$deficit)
  } else {
    rep(p$accent, nrow(df))
  }
  plotly::plot_ly(
    df,
    x = ~value,
    y = ~factor(label, levels = label),
    type = "bar",
    orientation = "h",
    marker = list(color = colors),
    hovertemplate = "%{y}<br>%{customdata}<extra></extra>",
    customdata = format_usd_compact(df$value)
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 15, color = p$ink)),
      xaxis = list(
        title = "Current US$",
        zeroline = TRUE,
        zerolinecolor = p$zero,
        gridcolor = p$grid
      ),
      yaxis = list(title = "", automargin = TRUE),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 140, r = 24, t = 48, b = 40),
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

overview_composition_plotly <- function(comp, title) {
  p <- gte_palette()
  if (is.null(comp) || (is.na(comp$total) || identical(comp$total, 0))) {
    return(overview_empty_plotly("Composition unavailable (zero or missing total trade)."))
  }
  df <- data.frame(
    flow = c("Imports", "Exports"),
    value = c(comp$imports, comp$exports),
    share = c(comp$imports_share_pct, comp$exports_share_pct),
    stringsAsFactors = FALSE
  )
  hover <- paste0(
    df$flow, ": ", format_usd_compact(df$value),
    " (", format_pct(df$share), " of total)"
  )
  plotly::plot_ly(
    df,
    x = ~value,
    y = ~rep("HS-85 trade", 2),
    color = ~flow,
    colors = c("Imports" = p$imports, "Exports" = p$exports),
    type = "bar",
    orientation = "h",
    hovertemplate = "%{customdata}<extra></extra>",
    customdata = hover
  ) |>
    plotly::layout(
      barmode = "stack",
      title = list(text = title, font = list(size = 15, color = p$ink)),
      xaxis = list(title = "Current US$", gridcolor = p$grid),
      yaxis = list(title = ""),
      legend = list(orientation = "h", y = -0.3),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 80, r = 24, t = 48, b = 56)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

overview_balance_bars_plotly <- function(dist, title) {
  p <- gte_palette()
  top_s <- dist$top_surplus
  top_d <- dist$top_deficit
  if ((is.null(top_s) || !nrow(top_s)) && (is.null(top_d) || !nrow(top_d))) {
    return(overview_empty_plotly())
  }
  df <- data.table::rbindlist(list(top_s, top_d), fill = TRUE)
  df <- unique(df, by = c("reporter_iso3"))
  df <- df[order(value, reporter_iso3)]
  df$label <- paste0(df$reporter_name, " (", df$reporter_iso3, ")")
  colors <- ifelse(df$value >= 0, p$surplus, p$deficit)
  plotly::plot_ly(
    as.data.frame(df),
    x = ~value,
    y = ~factor(label, levels = label),
    type = "bar",
    orientation = "h",
    marker = list(color = colors),
    hovertemplate = "%{y}<br>%{customdata}<extra></extra>",
    customdata = format_usd_compact(df$value)
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 15, color = p$ink)),
      xaxis = list(
        title = "Trade balance (current US$)",
        zeroline = TRUE, zerolinecolor = p$zero, gridcolor = p$grid
      ),
      yaxis = list(title = "", automargin = TRUE),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 140, r = 24, t = 48, b = 40),
      showlegend = FALSE,
      annotations = list(list(
        text = "Surplus and deficit shown for context; neither is inherently good or bad.",
        xref = "paper", yref = "paper", x = 0, y = -0.18,
        showarrow = FALSE, font = list(size = 10, color = p$muted),
        xanchor = "left"
      ))
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

overview_macro_scatter_plotly <- function(prep, title) {
  p <- gte_palette()
  dt <- prep$data
  if (is.null(dt) || !nrow(dt)) {
    return(overview_empty_plotly("Insufficient macro coverage for scatter."))
  }
  df <- as.data.frame(dt)
  pop <- df$population
  pop[!is.finite(pop) | pop <= 0] <- NA_real_
  size <- if (all(is.na(pop))) {
    rep(10, nrow(df))
  } else {

    s <- sqrt(pmax(pop, 1, na.rm = FALSE) / max(pop, na.rm = TRUE)) * 40
    s[is.na(s)] <- 8
    s
  }
  hover <- paste0(
    df$reporter_name, " (", df$reporter_iso3, ")<br>",
    "Year: ", df$year, "<br>",
    "GDP: ", format_usd_compact(df$gdp), "<br>",
    "HS-85 total trade: ", format_usd_compact(df$total_trade), "<br>",
    "Population: ", ifelse(is.na(df$population), "Unavailable",
                           format(round(df$population), big.mark = ",")), "<br>",
    "Trade/GDP: ", format_pct(df$trade_pct_gdp)
  )
  fig <- plotly::plot_ly(
    df,
    x = ~gdp,
    y = ~total_trade,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = size,
      color = p$accent_soft,
      opacity = 0.7,
      line = list(color = p$accent, width = 0.5)
    ),
    hovertemplate = "%{customdata}<extra></extra>",
    customdata = hover
  )
  xax <- list(title = if (prep$use_log) "GDP (current US$, log scale)" else "GDP (current US$)",
              type = if (prep$use_log) "log" else "linear", gridcolor = p$grid)
  yax <- list(title = if (prep$use_log) "HS-85 total trade (current US$, log scale)" else "HS-85 total trade (current US$)",
              type = if (prep$use_log) "log" else "linear", gridcolor = p$grid)
  fig |>
    plotly::layout(
      title = list(text = title, font = list(size = 15, color = p$ink)),
      xaxis = xax,
      yaxis = yax,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 70, r = 24, t = 48, b = 56)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

trend_text_summary <- function(series, scope_label) {
  if (is.null(series) || !nrow(series)) {
    return(paste("No trend data available for", scope_label, "."))
  }
  last <- series[nrow(series)]
  paste0(
    "For ", scope_label, " across ", min(series$year), "–", max(series$year),
    ", latest total HS-85 trade is ", format_usd_compact(last$total),
    " (imports ", format_usd_compact(last$imports),
    "; exports ", format_usd_compact(last$exports),
    "). Values are current US dollars."
  )
}
