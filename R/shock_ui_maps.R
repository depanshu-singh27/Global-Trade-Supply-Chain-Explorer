shock_prepare_map_impacts <- function(reporter_impacts,
                                        geometry = NULL,
                                        metric = "residual_unmet_value_usd",
                                        represented = NULL,
                                        selected_universe = NULL) {
  rep <- data.table::as.data.table(reporter_impacts)
  if (!nrow(rep) || !metric %in% names(rep)) {
    return(list(
      table = data.table::data.table(
        reporter_iso3 = character(),
        reporter_name = character(),
        map_value = numeric()
      ),
      metric = metric,
      n_mapped = 0L,
      n_unavailable = 0L,
      sf = NULL
    ))
  }

  out <- rep[, .(
    reporter_iso3 = as.character(reporter_iso3),
    reporter_name = as.character(reporter_name %||% reporter_iso3),
    map_value = shock_safe_display_numeric(get(metric))
  )]

  if (length(selected_universe)) {
    missing_u <- setdiff(as.character(selected_universe), out$reporter_iso3)
    if (length(missing_u)) {
      out <- rbind(
        out,
        data.table::data.table(
          reporter_iso3 = missing_u,
          reporter_name = missing_u,
          map_value = NA_real_
        ),
        fill = TRUE
      )
    }
  }
  if (length(represented)) {
    out[!reporter_iso3 %in% as.character(represented), map_value := NA_real_]
  }

  sf_out <- NULL
  if (!is.null(geometry) && inherits(geometry, "sf")) {
    g <- geometry

    iso_col <- intersect(c("iso_a3", "ISO_A3", "adm0_a3", "map_iso3"), names(g))
    if (length(iso_col)) {
      g$map_iso3 <- as.character(g[[iso_col[1]]])
      merged <- merge(
        g,
        as.data.frame(out),
        by.x = "map_iso3",
        by.y = "reporter_iso3",
        all.x = TRUE,
        sort = FALSE
      )
      sf_out <- merged
    }
  }

  list(
    table = out,
    metric = metric,
    n_mapped = sum(is.finite(out$map_value)),
    n_unavailable = sum(!is.finite(out$map_value)),
    sf = sf_out
  )
}

shock_map_text_summary <- function(prepared) {
  sprintf(
    paste(
      "Geographic view of reporter scenario impacts for metric %s.",
      "%d reporters with mapped values; %d unavailable or outside current detailed coverage.",
      "Missing is distinct from zero. Residual unmet imports are not forecasts of economic loss."
    ),
    prepared$metric %||% "metric",
    as.integer(prepared$n_mapped %||% 0L),
    as.integer(prepared$n_unavailable %||% 0L)
  )
}

shock_build_impact_leaflet <- function(prepared, metric_label = NULL) {
  title <- metric_label %||% prepared$metric %||% "Impact"
  base <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::setView(20, 20, zoom = 2)

  sf_obj <- prepared$sf
  if (is.null(sf_obj) || !inherits(sf_obj, "sf") || !nrow(sf_obj)) {
    return(base)
  }
  vals <- sf_obj$map_value
  finite_vals <- vals[is.finite(vals)]
  pal <- leaflet::colorNumeric(
    "YlOrRd",
    domain = if (length(finite_vals)) range(finite_vals) else c(0, 1),
    na.color = "#D9D9D9"
  )
  labels <- sprintf(
    "%s<br/>%s: %s",
    sf_obj$map_iso3 %||% sf_obj$reporter_name %||% "",
    title,
    ifelse(is.finite(sf_obj$map_value), format_shock_usd(sf_obj$map_value), "Unavailable")
  ) |> lapply(htmltools::HTML)

  base |>
    leaflet::addPolygons(
      data = sf_obj,
      fillColor = ~pal(map_value),
      weight = 0.6,
      color = "#666",
      fillOpacity = 0.75,
      label = labels,
      highlight = leaflet::highlightOptions(weight = 2, color = "#000", bringToFront = TRUE)
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = if (length(finite_vals)) finite_vals else 0,
      title = title,
      opacity = 0.85
    )
}
