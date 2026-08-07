shock_reporter_rank_plot <- function(ranked, metric = "residual_unmet_value_usd", selected = NULL) {
  dt <- data.table::as.data.table(ranked)
  if (!nrow(dt) || !metric %in% names(dt)) {
    return(
      plotly::plot_ly(type = "bar") |>
        plotly::layout(title = list(text = "No reporter impacts", font = list(size = 13)))
    )
  }

  data.table::setorderv(dt, c(metric, "reporter_iso3"), c(1L, 1L))
  colors <- ifelse(
    !is.null(selected) & dt$reporter_iso3 %in% selected,
    "#0B3D5C",
    "#4A90A4"
  )
  y_lab <- names(shock_reporter_rank_metric_choices())[
    match(metric, unname(shock_reporter_rank_metric_choices()))
  ]
  if (is.na(y_lab)) y_lab <- metric

  plotly::plot_ly(
    dt,
    x = ~get(metric),
    y = ~factor(reporter_iso3, levels = reporter_iso3),
    type = "bar",
    orientation = "h",
    marker = list(color = colors),
    hovertemplate = paste0("%{y}<br>", y_lab, ": %{x}<extra></extra>")
  ) |>
    plotly::layout(
      xaxis = list(title = y_lab),
      yaxis = list(title = ""),
      margin = list(l = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}

shock_reporter_rank_text <- function(ranked, metric = "residual_unmet_value_usd") {
  dt <- data.table::as.data.table(ranked)
  if (!nrow(dt) || !metric %in% names(dt)) {
    return("No reporter ranking available for the active scenario.")
  }
  top <- dt[1]
  sprintf(
    "Highest %s: %s at %s. Rankings use deterministic tie-breaking. Residual unmet imports are not GDP loss.",
    metric,
    top$reporter_iso3,
    if (grepl("pct|share|rate", metric, ignore.case = TRUE)) {
      format_shock_pct(top[[metric]])
    } else {
      format_shock_usd(top[[metric]])
    }
  )
}

shock_commodity_rank_plot <- function(ranked, metric = "residual_unmet_value_usd") {
  dt <- data.table::as.data.table(ranked)
  if (!nrow(dt) || !metric %in% names(dt)) {
    return(plotly::plot_ly(type = "bar") |> plotly::layout(title = list(text = "No commodity impacts", font = list(size = 13))))
  }
  data.table::setorderv(dt, c(metric, "hs_code"), c(1L, 1L))
  plotly::plot_ly(
    dt,
    x = ~get(metric),
    y = ~factor(hs_code, levels = hs_code),
    type = "bar",
    orientation = "h",
    marker = list(color = "#2F6F7E"),
    hovertemplate = "%{y}: %{x}<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = metric),
      yaxis = list(title = ""),
      margin = list(l = 70),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}

shock_supplier_allocation_plot <- function(summary) {
  subs <- data.table::as.data.table(summary$substitutes %||% data.table::data.table())
  val_col <- intersect(
    c("substitution_received_usd", "substitution_allocated_usd", "additional_substitution_received_usd"),
    names(subs)
  )
  id_col <- intersect(c("supplier_iso3", "partner_iso3"), names(subs))
  if (!nrow(subs) || !length(val_col) || !length(id_col)) {
    return(
      plotly::plot_ly(type = "bar") |>
        plotly::layout(
          title = list(text = "No substitute allocation in this scenario", font = list(size = 13))
        )
    )
  }
  subs <- data.table::copy(subs)
  subs[, alloc := get(val_col[1])]
  subs[, sid := get(id_col[1])]
  data.table::setorderv(subs, c("alloc", "sid"), c(-1L, 1L))
  subs <- utils::head(subs, 20L)
  plotly::plot_ly(
    subs,
    x = ~alloc,
    y = ~factor(sid, levels = rev(sid)),
    type = "bar",
    orientation = "h",
    marker = list(color = "#6B8F71"),
    hovertemplate = "%{y}: %{x}<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = "Substitution allocated (USD)"),
      yaxis = list(title = ""),
      annotations = list(list(
        text = "Analytical allocation across observed suppliers — not verified spare capacity",
        xref = "paper", yref = "paper", x = 0, y = 1.12, showarrow = FALSE,
        font = list(size = 11, color = "#555")
      ))
    )
}

shock_concentration_change_plot <- function(changes) {
  dt <- data.table::as.data.table(changes)
  if (!nrow(dt) || !all(c("supplier_hhi", "post_shock_hhi") %in% names(dt))) {
    return(
      plotly::plot_ly(type = "scatter") |>
        plotly::layout(title = list(text = "No concentration changes", font = list(size = 13)))
    )
  }
  dt <- dt[is.finite(supplier_hhi) & is.finite(post_shock_hhi)]
  if (!nrow(dt)) {
    return(
      plotly::plot_ly(type = "scatter") |>
        plotly::layout(title = list(text = "No concentration changes", font = list(size = 13)))
    )
  }
  dt[, label := paste(reporter_iso3, hs_code, sep = " · ")]
  plotly::plot_ly(
    dt,
    x = ~supplier_hhi,
    y = ~post_shock_hhi,
    type = "scatter",
    mode = "markers",
    text = ~label,
    marker = list(size = 9, color = "#0B3D5C"),
    hovertemplate = "%{text}<br>Before %{x:.3f}<br>After %{y:.3f}<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = "HHI before", range = c(0, 1)),
      yaxis = list(title = "HHI after", range = c(0, 1)),
      shapes = list(list(
        type = "line", x0 = 0, x1 = 1, y0 = 0, y1 = 1,
        line = list(dash = "dot", color = "#999")
      ))
    )
}

shock_propagation_depth_plot <- function(paths) {
  dt <- data.table::as.data.table(paths)
  if (!nrow(dt) || !"propagated_value_usd" %in% names(dt)) {
    return(
      plotly::plot_ly(type = "bar") |>
        plotly::layout(
          title = list(
            text = "No propagation paths are generated in Direct only mode.",
            font = list(size = 13)
          )
        )
    )
  }
  agg <- dt[, .(propagated_value_usd = sum(propagated_value_usd, na.rm = TRUE)), by = depth]
  data.table::setorder(agg, depth)
  plotly::plot_ly(
    agg,
    x = ~factor(depth),
    y = ~propagated_value_usd,
    type = "bar",
    marker = list(color = "#8C5E3B"),
    hovertemplate = "Depth %{x}: %{y}<extra></extra>"
  ) |>
    plotly::layout(
      xaxis = list(title = "Propagation depth"),
      yaxis = list(title = "Propagated value (USD)")
    )
}

shock_filter_propagation_paths <- function(paths,
                                             depth_max = NULL,
                                             reporters = NULL,
                                             hs_codes = NULL,
                                             min_value = 0,
                                             cap = 500L) {
  dt <- data.table::as.data.table(paths)
  if (!nrow(dt)) return(dt)
  if (!is.null(depth_max) && "depth" %in% names(dt)) {
    dt <- dt[depth <= as.integer(depth_max)]
  }
  if (length(reporters) && "affected_reporter" %in% names(dt)) {
    dt <- dt[affected_reporter %in% reporters]
  }
  if (length(hs_codes) && "hs_code" %in% names(dt)) {
    dt <- dt[hs_code %in% hs_codes]
  }
  if ("propagated_value_usd" %in% names(dt)) {
    dt <- dt[propagated_value_usd >= as.numeric(min_value %||% 0)]
  }
  utils::head(dt, as.integer(cap))
}
