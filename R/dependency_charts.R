dep_empty_plotly <- function(msg = "No dependency observations for the current filters.") {
  plotly::plotly_empty(type = "heatmap") |>
    plotly::layout(
      title = list(text = msg, font = list(size = 13)),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

dependency_heatmap_plotly <- function(mat,
                                        title = "Dependency matrix",
                                        zlab = "Value",
                                        colorscale = "YlOrRd") {
  if (is.null(mat) || !length(dim(mat)) || !nrow(mat) || !ncol(mat)) {
    return(dep_empty_plotly())
  }

  plotly::plot_ly(
    z = mat,
    x = colnames(mat),
    y = rownames(mat),
    type = "heatmap",
    colorscale = colorscale,
    hovertemplate = paste0(
      "Row: %{y}<br>Column: %{x}<br>", zlab, ": %{z}<extra></extra>"
    ),
    colorbar = list(title = zlab)
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 14)),
      xaxis = list(title = "", tickangle = -45, automargin = TRUE),
      yaxis = list(title = "", automargin = TRUE),
      margin = list(l = 80, r = 20, t = 50, b = 100)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

reporter_supplier_heatmap <- function(matrix_obj, title = "Reporter × supplier import share") {
  if (is.null(matrix_obj) || !nrow(matrix_obj$long)) return(dep_empty_plotly())
  mat <- matrix(
    NA_real_,
    nrow = length(matrix_obj$row_ids),
    ncol = length(matrix_obj$col_ids),
    dimnames = list(matrix_obj$row_ids, matrix_obj$col_ids)
  )
  long <- matrix_obj$long
  for (i in seq_len(nrow(long))) {
    mat[long$row_id[i], long$col_id[i]] <- long$value[i]
  }
  dependency_heatmap_plotly(mat, title = title, zlab = matrix_obj$metric %||% "share")
}

country_commodity_heatmap <- function(sparse, max_display = 40L,
                                        title = "Country-commodity dependency (sparse)") {
  if (is.null(sparse) || !nrow(sparse$edges) || !nrow(sparse$nodes)) {
    return(dep_empty_plotly())
  }
  ids <- sparse$nodes$node_id
  max_display <- min(as.integer(max_display %||% 40L), length(ids))
  ids <- ids[seq_len(max_display)]
  mat <- sparse_edges_to_display_matrix(
    sparse$edges, ids, ids, value_col = "weight"
  )
  dependency_heatmap_plotly(
    mat,
    title = paste0(title, " — ", nrow(mat), "×", ncol(mat), " displayed"),
    zlab = "weight"
  )
}

concentration_scatter_plotly <- function(group_conc, scale = "auto") {
  dt <- add_commodity_importance(group_conc)
  if (!nrow(dt) || !"commodity_import_share" %in% names(dt)) {
    return(dep_empty_plotly("No concentration points available."))
  }
  sc <- tf_scale_divisor(scale, dt$reporter_commodity_total)
  plotly::plot_ly(
    dt,
    x = ~commodity_import_share,
    y = ~supplier_hhi,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = pmax(8, 40 * (dt$reporter_commodity_total / max(dt$reporter_commodity_total, na.rm = TRUE))),
      color = "#1f4e79",
      opacity = 0.7
    ),
    text = ~paste0(reporter_iso3, " · ", hs_code),
    hovertemplate = paste0(
      "%{text}<br>Commodity share: %{x:.1%}<br>HHI: %{y:.3f}",
      "<br>Import value: ", format_trade_value_scaled(dt$reporter_commodity_total, scale),
      "<extra></extra>"
    )
  ) |>
    plotly::layout(
      title = list(text = "Commodity importance vs supplier HHI", font = list(size = 13)),
      xaxis = list(title = "Commodity import share of reporter total", tickformat = ".0%"),
      yaxis = list(title = "Supplier HHI (0–1)")
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

dependency_ranking_plotly <- function(ranked, metric, title, scale = "auto") {
  dt <- data.table::as.data.table(ranked)
  if (!nrow(dt) || !metric %in% names(dt)) return(dep_empty_plotly())
  dt[, label := paste0(reporter_iso3, " · ", hs_code)]
  dt[, label := factor(label, levels = rev(label))]
  vals <- dt[[metric]]
  plotly::plot_ly(
    dt, x = vals, y = ~label, type = "bar", orientation = "h",
    marker = list(color = "#1f4e79"),
    hovertext = paste0(dt$label, ": ", format_dependency_hhi(vals)),
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = list(text = title, font = list(size = 13)),
      xaxis = list(title = metric),
      yaxis = list(title = ""),
      margin = list(l = 120)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}

dependency_trend_plotly <- function(trend_dt) {
  dt <- data.table::as.data.table(trend_dt)
  if (!nrow(dt)) return(dep_empty_plotly("No trend observations."))
  plotly::plot_ly(dt, x = ~year, y = ~weighted_hhi, type = "scatter", mode = "lines+markers",
                  name = "Weighted HHI") |>
    plotly::add_trace(y = ~weighted_top_1_share, name = "Weighted top-1 share",
                      mode = "lines+markers", yaxis = "y2") |>
    plotly::layout(
      title = list(text = "Reporter concentration trends (absolute levels)", font = list(size = 13)),
      xaxis = list(title = "Year", dtick = 1),
      yaxis = list(title = "Weighted HHI"),
      yaxis2 = list(overlaying = "y", side = "right", title = "Top-1 share", range = c(0, 1)),
      legend = list(orientation = "h", y = -0.2)
    ) |>
    plotly::config(displayModeBar = FALSE, responsive = TRUE)
}
