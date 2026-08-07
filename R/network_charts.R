nw_empty_plotly <- function(msg = "No network edges for the current filters.") {
  plotly::plotly_empty(type = "scatter") |>
    plotly::layout(
      title = list(text = msg, font = list(size = 13)),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

network_status_colour <- function(status) {
  status <- as.character(status)
  dplyr_map <- c(
    represented_reporter = "#1f4e79",
    both = "#2a9d8f",
    partner_only = "#9a6b00",
    selected_reporter_missing = "#6b7280"
  )
  out <- unname(dplyr_map[status])
  out[is.na(out)] <- "#5b6b7c"
  out
}

prepare_network_plotly <- function(net,
                                     size_metric = "total_strength",
                                     colour_metric = "reporting_status",
                                     scale = "auto",
                                     selected_iso3 = NULL) {
  nodes <- data.table::as.data.table(net$nodes)
  edges <- data.table::as.data.table(net$edges)
  if (!nrow(nodes) || !nrow(edges)) return(NULL)

  if (!size_metric %in% names(nodes)) size_metric <- "total_strength"
  size_vals <- sanitize_chart_numeric(nodes[[size_metric]])
  size_vals[!is.finite(size_vals) | size_vals < 0] <- 0
  mx <- max(size_vals, na.rm = TRUE)
  nodes[, plot_size := if (is.finite(mx) && mx > 0) 12 + 28 * (size_vals / mx) else 14]

  if (identical(colour_metric, "community") && "community" %in% names(nodes)) {
    nodes[, plot_colour := paste0("Community ", as.character(community %||% "?"))]
  } else if (identical(colour_metric, "pagerank_quantile") && "pagerank_quantile" %in% names(nodes)) {
    nodes[, plot_colour := paste0("PR Q", as.character(pagerank_quantile %||% "?"))]
  } else if (identical(colour_metric, "universe_membership")) {
    nodes[, plot_colour := data.table::fcase(
      is_selected_reporter & represented_as_reporter, "Selected represented reporter",
      is_selected_reporter, "Selected reporter (missing detailed)",
      is_selected_partner, "Selected partner",
      rep(TRUE, .N), "Other observed partner"
    )]
  } else {
    nodes[, plot_colour := reporting_status_label(reporting_status)]
  }

  sc <- tf_scale_divisor(scale, edges$trade_value_usd)
  pos <- nodes[, .(iso3, x, y)]
  el <- merge(edges, pos, by.x = "from_iso3", by.y = "iso3", all.x = TRUE)
  data.table::setnames(el, c("x", "y"), c("x0", "y0"))
  el <- merge(el, pos, by.x = "to_iso3", by.y = "iso3", all.x = TRUE)
  data.table::setnames(el, c("x", "y"), c("x1", "y1"))

  list(nodes = nodes, edges = el, scale = sc, selected = selected_iso3)
}

network_plotly_axis_ranges <- function(nodes, pad_frac = 0.18) {
  nd <- data.table::as.data.table(nodes)
  x <- sanitize_chart_numeric(nd$x)
  y <- sanitize_chart_numeric(nd$y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (!length(x)) {
    return(list(x = c(-1, 1), y = c(-1, 1)))
  }
  xmin <- min(x)
  xmax <- max(x)
  ymin <- min(y)
  ymax <- max(y)
  cx <- (xmin + xmax) / 2
  cy <- (ymin + ymax) / 2
  pad_frac <- max(0, as.numeric(pad_frac)[1])
  hx <- max((xmax - xmin) / 2, 1e-6)
  hy <- max((ymax - ymin) / 2, 1e-6)

  hx <- hx * (1 + pad_frac) + hx * 0.08
  hy <- hy * (1 + pad_frac) + hy * 0.14
  list(
    x = c(cx - hx, cx + hx),
    y = c(cy - hy, cy + hy)
  )
}

network_plotly <- function(net,
                             size_metric = "total_strength",
                             colour_metric = "reporting_status",
                             scale = "auto",
                             selected_iso3 = NULL,
                             title = "Available-observation trade network") {
  prep <- prepare_network_plotly(net, size_metric, colour_metric, scale, selected_iso3)
  if (is.null(prep)) return(nw_empty_plotly())

  nodes <- prep$nodes
  edges <- prep$edges
  axis_rng <- network_plotly_axis_ranges(nodes)

  edge_x <- as.vector(rbind(edges$x0, edges$x1, NA_real_))
  edge_y <- as.vector(rbind(edges$y0, edges$y1, NA_real_))
  p <- plotly::plot_ly()
  p <- plotly::add_trace(
    p,
    x = edge_x, y = edge_y,
    type = "scatter", mode = "lines",
    line = list(color = "rgba(91,107,124,0.35)", width = 1),
    hoverinfo = "skip",
    showlegend = FALSE
  )

  hover <- paste0(
    nodes$display_name, " (", nodes$iso3, ")<br>",
    reporting_status_label(nodes$reporting_status), "<br>",
    "Total strength: ", format_trade_value_scaled(nodes$total_strength, scale), "<br>",
    "In / Out: ", format_trade_value_scaled(nodes$in_strength, scale), " / ",
    format_trade_value_scaled(nodes$out_strength, scale), "<br>",
    "Degree: ", nodes$degree, "<br>",
    "PageRank: ", format_network_metric(nodes$pagerank), "<br>",
    "Betweenness: ", format_network_metric(nodes$betweenness)
  )
  sel <- as.character(selected_iso3 %||% "")
  nodes[, is_selected := iso3 == sel & nzchar(sel)]
  p <- plotly::add_trace(
    p,
    data = nodes,
    x = ~x, y = ~y,
    type = "scatter", mode = "markers+text",
    text = ~iso3,
    textposition = "top center",
    textfont = list(size = 10),
    marker = list(
      size = ~plot_size,
      color = ~plot_colour,
      line = list(
        color = ifelse(nodes$is_selected, "#1b2a3b", "rgba(255,255,255,0.8)"),
        width = ifelse(nodes$is_selected, 3, 1)
      )
    ),
    customdata = ~iso3,
    hovertext = hover,
    hoverinfo = "text",
    showlegend = TRUE
  )
  plotly::layout(
    p,
    title = list(text = title, font = list(size = 14)),

    xaxis = list(
      visible = FALSE,
      scaleanchor = "y",
      scaleratio = 1,
      constrain = "domain",
      constraintoward = "center",
      autorange = FALSE,
      range = axis_rng$x,
      fixedrange = FALSE
    ),
    yaxis = list(
      visible = FALSE,
      constrain = "domain",
      constraintoward = "middle",
      autorange = FALSE,
      range = axis_rng$y,
      fixedrange = FALSE
    ),
    legend = list(orientation = "h", y = -0.12),
    margin = list(l = 20, r = 20, t = 40, b = 60),
    autosize = TRUE
  ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE, doubleClick = "reset")
}

centrality_ranking_plotly <- function(ranked, metric = "total_strength",
                                        scale = "auto", title = "Centrality ranking") {
  dt <- data.table::as.data.table(ranked)
  if (!nrow(dt) || !metric %in% names(dt)) return(nw_empty_plotly("No ranking available."))
  dt <- data.table::copy(dt)
  dt[, label := paste0(display_name, " (", iso3, ")")]
  dt[, label := factor(label, levels = rev(label))]
  vals <- dt[[metric]]
  if (metric %in% c("total_strength", "in_strength", "out_strength")) {
    sc <- tf_scale_divisor(scale, vals)
    ytitle <- sc$unit
    plot_vals <- vals / sc$div
  } else {
    ytitle <- metric
    plot_vals <- vals
  }
  plotly::plot_ly(
    dt,
    x = plot_vals,
    y = ~label,
    type = "bar",
    orientation = "h",
    text = ~reporting_status_label(reporting_status),
    hoverinfo = "text",
    hovertext = paste0(dt$label, "<br>", metric, ": ", format_network_metric(vals)),
    marker = list(color = "#1f4e79")
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 13)),
      xaxis = list(title = ytitle),
      yaxis = list(title = ""),
      margin = list(l = 140, r = 20, t = 40, b = 40)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

network_diagnostics_plotly <- function(nodes, edges, which = "degree") {
  nd <- data.table::as.data.table(nodes)
  ed <- data.table::as.data.table(edges)
  if (identical(which, "strength") && nrow(nd)) {
    vals <- sort(sanitize_chart_numeric(nd$total_strength), decreasing = TRUE)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) return(nw_empty_plotly())
    plotly::plot_ly(x = seq_along(vals), y = vals, type = "scatter", mode = "lines+markers",
                    name = "Strength") |>
      plotly::layout(
        title = list(text = "Node-strength distribution (ranked)", font = list(size = 13)),
        xaxis = list(title = "Rank"),
        yaxis = list(title = "Total strength (US$)")
      ) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  } else if (identical(which, "edges") && nrow(ed)) {
    vals <- sort(sanitize_chart_numeric(ed$trade_value_usd), decreasing = TRUE)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) return(nw_empty_plotly())
    plotly::plot_ly(x = seq_along(vals), y = vals, type = "scatter", mode = "lines+markers",
                    name = "Edge weight") |>
      plotly::layout(
        title = list(text = "Edge-weight concentration (ranked)", font = list(size = 13)),
        xaxis = list(title = "Rank"),
        yaxis = list(title = "Trade value (US$)")
      ) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  } else if (nrow(nd)) {
    plotly::plot_ly(x = nd$degree, type = "histogram", marker = list(color = "#1f4e79")) |>
      plotly::layout(
        title = list(text = "Degree distribution", font = list(size = 13)),
        xaxis = list(title = "Degree"),
        yaxis = list(title = "Nodes")
      ) |>
      plotly::config(displayModeBar = FALSE, responsive = TRUE)
  } else {
    nw_empty_plotly()
  }
}
