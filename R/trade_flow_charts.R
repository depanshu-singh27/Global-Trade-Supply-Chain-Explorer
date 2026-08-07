trade_flow_empty_plotly <- function(message = "No observations match these filters.") {
  overview_empty_plotly(message)
}

trade_flow_timeseries_plotly <- function(prep, title, scale = "auto") {
  p <- gte_palette()
  dt <- prep$data
  if (is.null(dt) || !nrow(dt)) return(trade_flow_empty_plotly())
  df <- as.data.frame(dt)
  sc <- tf_scale_divisor(scale, df$value)
  df$plot_value <- df$value / sc$div
  plotly::plot_ly(
    df,
    x = ~year,
    y = ~plot_value,
    color = ~series,
    colors = c(p$imports, p$exports, p$accent, p$accent_soft, p$surplus, p$deficit, p$total, p$warn),
    type = "scatter",
    mode = "lines+markers",
    hovertemplate = paste0("%{fullData.name}<br>Year %{x}<br>", sc$unit, ": %{y:.2f}<extra></extra>")
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 15, color = p$ink)),
      xaxis = list(title = "Year", dtick = 1, gridcolor = p$grid),
      yaxis = list(title = sc$unit, gridcolor = p$grid),
      legend = list(orientation = "h", y = -0.22),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 60, r = 20, t = 48, b = 60)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

trade_flow_composition_plotly <- function(prep, title, scale = "auto") {
  p <- gte_palette()
  dt <- prep$data
  if (is.null(dt) || !nrow(dt) || !(prep$total > 0)) {
    return(trade_flow_empty_plotly("Commodity composition unavailable (zero or missing total)."))
  }
  df <- as.data.frame(dt)
  df <- df[order(df$value, df$hs_code), , drop = FALSE]
  sc <- tf_scale_divisor(scale, df$value)
  hover <- paste0(
    df$hs_code, "<br>",
    ifelse(is.na(df$commodity_description), "", substr(df$commodity_description, 1, 80)),
    "<br>", format_usd_compact(df$value),
    " (", format_pct(df$share_pct), ")"
  )
  plotly::plot_ly(
    df,
    x = ~value / sc$div,
    y = ~factor(hs_code, levels = hs_code),
    type = "bar",
    orientation = "h",
    marker = list(color = ifelse(df$hs_code == "OTHER", p$muted, p$accent)),
    hovertemplate = "%{customdata}<extra></extra>",
    customdata = hover
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 15, color = p$ink)),
      xaxis = list(title = sc$unit, gridcolor = p$grid),
      yaxis = list(title = "", automargin = TRUE),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 80, r = 20, t = 48, b = 40),
      showlegend = FALSE
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

trade_flow_matrix_plotly <- function(prep, title, scale = "auto") {
  p <- gte_palette()
  long <- prep$long
  if (is.null(long) || !nrow(long)) return(trade_flow_empty_plotly())
  reps <- prep$reporters
  pars <- prep$partners
  if (!length(reps) || !length(pars)) return(trade_flow_empty_plotly())

  mat <- matrix(NA_real_, nrow = length(reps), ncol = length(pars),
                dimnames = list(reps, pars))
  obs <- matrix(FALSE, nrow = length(reps), ncol = length(pars),
                dimnames = list(reps, pars))
  for (i in seq_len(nrow(long))) {
    r <- long$reporter_iso3[i]
    c <- long$partner_iso3[i]
    if (r %in% reps && c %in% pars) {
      mat[r, c] <- long$value[i]
      obs[r, c] <- isTRUE(long$observed[i])
    }
  }
  sc <- tf_scale_divisor(scale, mat[is.finite(mat)])
  z <- mat / sc$div

  txt <- matrix("", nrow = nrow(z), ncol = ncol(z))
  for (i in seq_len(nrow(z))) {
    for (j in seq_len(ncol(z))) {
      if (!obs[i, j]) {
        txt[i, j] <- "Unobserved"
      } else if (is.na(mat[i, j])) {
        txt[i, j] <- "Unavailable"
      } else {
        txt[i, j] <- format_usd_compact(mat[i, j])
      }
    }
  }
  plotly::plot_ly(
    x = pars,
    y = reps,
    z = z,
    type = "heatmap",
    colorscale = list(c(0, "#F4F7FB"), c(1, p$accent)),
    hovertemplate = "Reporter %{y} → Partner %{x}<br>%{text}<extra></extra>",
    text = txt,
    colorbar = list(title = sc$unit)
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 15, color = p$ink)),
      xaxis = list(title = "Partner", side = "bottom"),
      yaxis = list(title = "Reporter", autorange = "reversed"),
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 60, r = 20, t = 48, b = 60)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

render_trade_flow_sankey <- function(sankey, height = 480) {
  if (is.null(sankey) || !nrow(sankey$nodes) || !nrow(sankey$links)) {
    return(NULL)
  }
  networkD3::sankeyNetwork(
    Links = sankey$links,
    Nodes = sankey$nodes,
    Source = "source",
    Target = "target",
    Value = "value",
    NodeID = "name",
    sinksRight = FALSE,
    fontSize = 11,
    nodeWidth = 18,
    iterations = 0,
    width = NULL,
    height = height
  )
}

sankey_text_summary <- function(sankey, coverage) {
  if (is.null(sankey) || !nrow(sankey$links)) {
    return("No Sankey links for the current filters.")
  }
  links <- sankey$links
  nodes <- sankey$nodes
  ord <- order(-links$value)
  top <- utils::head(ord, 5L)
  parts <- vapply(top, function(i) {
    sprintf(
      "%s → %s (%s)",
      nodes$name[links$source[i] + 1L],
      nodes$name[links$target[i] + 1L],
      format_usd_compact(links$value[i])
    )
  }, character(1))
  paste0(
    "Visible Sankey coverage: ", format_pct(coverage$coverage_pct),
    " of filtered detailed trade (", format_usd_compact(coverage$total_value), "). ",
    "Largest visible links: ", paste(parts, collapse = "; "),
    ". Flows preserve the reporting-economy perspective (imports and exports are not mirrored)."
  )
}
