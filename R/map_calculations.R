prepare_map_analytics <- function(cy) {
  dt <- data.table::as.data.table(cy)
  if (!nrow(dt)) return(dt)
  dt <- dt[
    !is.na(reporter_iso3) &
      nchar(as.character(reporter_iso3)) == 3L &
      !(reporter_iso3 %in% AGGREGATE_ISO3_MAP)
  ]
  dt[, `:=`(
    year = as.integer(year),
    reporter_iso3 = as.character(reporter_iso3),
    imports_value_usd = sanitize_chart_numeric(imports_value_usd),
    exports_value_usd = sanitize_chart_numeric(exports_value_usd),
    total_trade_value_usd = sanitize_chart_numeric(total_trade_value_usd),
    trade_balance_usd = sanitize_chart_numeric(trade_balance_usd)
  )]

  need_tot <- is.na(dt$total_trade_value_usd) &
    !(is.na(dt$imports_value_usd) & is.na(dt$exports_value_usd))
  if (any(need_tot)) {
    dt[need_tot, total_trade_value_usd :=
         ifelse(is.na(imports_value_usd), 0, imports_value_usd) +
         ifelse(is.na(exports_value_usd), 0, exports_value_usd)]
  }
  need_bal <- is.na(dt$trade_balance_usd) &
    !(is.na(dt$imports_value_usd) & is.na(dt$exports_value_usd))
  if (any(need_bal)) {
    dt[need_bal, trade_balance_usd :=
         ifelse(is.na(exports_value_usd), 0, exports_value_usd) -
         ifelse(is.na(imports_value_usd), 0, imports_value_usd)]
  }

  bad_gdp <- is.na(dt$gdp_current_usd) | !is.finite(dt$gdp_current_usd) | dt$gdp_current_usd <= 0
  bad_pop <- is.na(dt$population_total) | !is.finite(dt$population_total) | dt$population_total <= 0
  if ("total_trade_pct_gdp" %in% names(dt)) {
    dt[bad_gdp, total_trade_pct_gdp := NA_real_]
  } else {
    dt[, total_trade_pct_gdp := ifelse(bad_gdp, NA_real_,
                                      100 * total_trade_value_usd / gdp_current_usd)]
  }
  if ("trade_balance_pct_gdp" %in% names(dt)) {
    dt[bad_gdp, trade_balance_pct_gdp := NA_real_]
  } else {
    dt[, trade_balance_pct_gdp := ifelse(bad_gdp, NA_real_,
                                        100 * trade_balance_usd / gdp_current_usd)]
  }
  if ("total_trade_per_capita_usd" %in% names(dt)) {
    dt[bad_pop, total_trade_per_capita_usd := NA_real_]
  } else {
    dt[, total_trade_per_capita_usd := ifelse(bad_pop, NA_real_,
                                             total_trade_value_usd / population_total)]
  }
  data.table::setkeyv(dt, c("year", "reporter_iso3"))
  dt
}

map_year_choices <- function(cy) {
  dt <- prepare_map_analytics(cy)
  if (!nrow(dt)) return(integer())
  sort(unique(as.integer(dt$year)))
}

choose_map_default_year <- function(cy) {
  years <- map_year_choices(cy)
  if (!length(years)) return(NA_integer_)

  cov <- prepare_map_analytics(cy)[!is.na(total_trade_value_usd),
                                  .(n = data.table::uniqueN(reporter_iso3)), by = year]
  if (!nrow(cov)) return(max(years))
  max_n <- max(cov$n)
  adequate <- cov[n >= 0.75 * max_n]
  as.integer(adequate[order(-year)][1]$year)
}

filter_map_year <- function(cy, year, region = NULL, geometry = NULL) {
  dt <- prepare_map_analytics(cy)
  year_sel <- as.integer(year)
  dt <- dt[year == year_sel]
  if (!is.null(region) && nzchar(region) && !identical(region, "__ALL__") &&
      !is.null(geometry) && inherits(geometry, "sf")) {
    iso_reg <- geometry$map_iso3[geometry$region_wb == region]
    dt <- dt[reporter_iso3 %in% iso_reg]
  }

  if (nrow(dt)) {
    dt <- unique(dt, by = "reporter_iso3")
  }
  dt
}

map_metric_values <- function(dt, metric) {
  defs <- map_metric_defs()
  col <- defs[[metric]]$column %||% metric
  if (!col %in% names(dt)) return(rep(NA_real_, nrow(dt)))
  sanitize_chart_numeric(dt[[col]])
}

join_map_data <- function(year_dt, geometry, metric) {
  if (is.null(geometry) || !inherits(geometry, "sf")) {
    return(list(
      sf = NULL,
      table = data.table::as.data.table(year_dt),
      crosswalk = build_geographic_crosswalk(year_dt$reporter_iso3, NULL)
    ))
  }
  dt <- data.table::as.data.table(year_dt)
  dt[, map_value := map_metric_values(dt, metric)]
  dt[, metric := metric]
  xw <- build_geographic_crosswalk(dt$reporter_iso3, geometry)
  if (nrow(xw) && "reporter_name" %in% names(dt)) {
    nm <- unique(dt[, .(source_iso3 = reporter_iso3, trade_name = reporter_name)])
    xw[nm, country_name := i.trade_name, on = "source_iso3"]
  }

  g <- if (isTRUE(attr(geometry, "leaflet_geometry_prepared"))) {
    geometry
  } else {
    prepare_leaflet_geometry(geometry)
  }
  trade_on_map <- dt[, .(
    reporter_iso3, reporter_name, year,
    imports_value_usd, exports_value_usd, total_trade_value_usd, trade_balance_usd,
    gdp_current_usd, population_total, gdp_per_capita_usd, inflation_annual_pct,
    total_trade_pct_gdp, trade_balance_pct_gdp, total_trade_per_capita_usd,
    total_trade_yoy_pct, map_value, metric
  )]
  data.table::setnames(trade_on_map, "reporter_iso3", "map_iso3")

  trade_on_map <- unique(trade_on_map, by = "map_iso3")

  idx <- match(as.character(g$map_iso3), as.character(trade_on_map$map_iso3))
  trade_cols <- setdiff(names(trade_on_map), "map_iso3")
  for (col in trade_cols) {
    g[[col]] <- trade_on_map[[col]][idx]
  }
  joined <- g
  joined$geometry_match_status <- ifelse(
    is.na(joined$reporter_name) & is.na(joined$map_value),
    "geometry_only",
    "matched"
  )
  joined$geometry_match_status[
    is.na(joined$map_value) & !is.na(joined$reporter_name)
  ] <- "matched_missing_metric"
  list(sf = joined, table = dt, crosswalk = xw)
}

mapped_value_coverage <- function(year_dt, joined_sf, metric) {
  dt <- data.table::as.data.table(year_dt)
  vals <- map_metric_values(dt, metric)
  if (map_metric_is_signed(metric)) {
    denom <- sum(abs(vals), na.rm = TRUE)
  } else {
    denom <- sum(vals, na.rm = TRUE)
  }
  if (is.null(joined_sf) || !inherits(joined_sf, "sf")) {
    return(list(
      coverage_pct = NA_real_, mapped_abs = 0, source_abs = denom,
      method = if (map_metric_is_signed(metric)) "absolute_value_ratio" else "value_ratio",
      n_matched = 0L, n_source = data.table::uniqueN(dt$reporter_iso3)
    ))
  }
  mapped_iso <- joined_sf$map_iso3[!is.na(joined_sf$map_value)]
  mapped_vals <- vals[dt$reporter_iso3 %in% mapped_iso]
  if (map_metric_is_signed(metric)) {
    numer <- sum(abs(mapped_vals), na.rm = TRUE)
  } else {
    numer <- sum(mapped_vals, na.rm = TRUE)
  }
  list(
    coverage_pct = if (denom > 0) 100 * numer / denom else NA_real_,
    mapped_abs = numer,
    source_abs = denom,
    method = if (map_metric_is_signed(metric)) "absolute_value_ratio" else "value_ratio",
    n_matched = length(unique(mapped_iso)),
    n_source = data.table::uniqueN(dt$reporter_iso3)
  )
}

map_kpi_summary <- function(year_dt, coverage = NULL) {
  dt <- data.table::as.data.table(year_dt)
  bal <- dt$trade_balance_usd
  list(
    total_trade = sum(dt$total_trade_value_usd, na.rm = TRUE),
    imports = sum(dt$imports_value_usd, na.rm = TRUE),
    exports = sum(dt$exports_value_usd, na.rm = TRUE),
    trade_balance = sum(dt$exports_value_usd, na.rm = TRUE) - sum(dt$imports_value_usd, na.rm = TRUE),
    n_economies = data.table::uniqueN(dt$reporter_iso3),
    n_surplus = sum(bal > 0, na.rm = TRUE),
    n_deficit = sum(bal < 0, na.rm = TRUE),
    n_zero = sum(bal == 0, na.rm = TRUE),
    mapped_coverage_pct = coverage$coverage_pct %||% NA_real_,
    scope_note = "World-partner HS-85 country totals (global dataset). Not bilateral dependency."
  )
}

selected_country_profile <- function(cy, iso3, year) {
  dt <- prepare_map_analytics(cy)
  year_sel <- as.integer(year)
  iso <- as.character(iso3)
  row <- dt[reporter_iso3 == iso & year == year_sel]
  if (!nrow(row)) {
    return(list(available = FALSE, reporter_iso3 = iso, year = year_sel))
  }
  r <- row[1]
  list(
    available = TRUE,
    reporter_iso3 = iso,
    reporter_name = as.character(r$reporter_name),
    year = year_sel,
    imports = as.numeric(r$imports_value_usd),
    exports = as.numeric(r$exports_value_usd),
    total_trade = as.numeric(r$total_trade_value_usd),
    trade_balance = as.numeric(r$trade_balance_usd),
    gdp = as.numeric(r$gdp_current_usd),
    population = as.numeric(r$population_total),
    gdp_per_capita = as.numeric(r$gdp_per_capita_usd),
    inflation = as.numeric(r$inflation_annual_pct),
    trade_pct_gdp = as.numeric(r$total_trade_pct_gdp),
    balance_pct_gdp = as.numeric(r$trade_balance_pct_gdp),
    trade_per_capita = as.numeric(r$total_trade_per_capita_usd),
    yoy_total_pct = as.numeric(r$total_trade_yoy_pct)
  )
}

prepare_country_trend <- function(cy, iso3) {
  dt <- prepare_map_analytics(cy)
  iso <- as.character(iso3)
  out <- dt[reporter_iso3 == iso, .(
    year, imports = imports_value_usd, exports = exports_value_usd,
    total = total_trade_value_usd, balance = trade_balance_usd
  )]
  data.table::setorderv(out, "year")
  out
}

prepare_global_trend <- function(cy, region = NULL, geometry = NULL) {
  dt <- prepare_map_analytics(cy)
  if (!is.null(region) && nzchar(region) && !identical(region, "__ALL__") &&
      !is.null(geometry)) {
    iso_reg <- geometry$map_iso3[geometry$region_wb == region]
    dt <- dt[reporter_iso3 %in% iso_reg]
  }
  out <- dt[, .(
    imports = sum(imports_value_usd, na.rm = TRUE),
    exports = sum(exports_value_usd, na.rm = TRUE),
    total = sum(total_trade_value_usd, na.rm = TRUE),
    balance = sum(exports_value_usd, na.rm = TRUE) - sum(imports_value_usd, na.rm = TRUE)
  ), by = year]
  data.table::setorderv(out, "year")
  out
}

map_surplus_ranking <- function(year_dt, top_n = 10L) {
  dt <- data.table::as.data.table(year_dt)
  dt <- dt[!is.na(trade_balance_usd) & trade_balance_usd >= 0]
  data.table::setorderv(dt, c("trade_balance_usd", "reporter_iso3"), c(-1L, 1L))
  dt <- dt[seq_len(min(as.integer(top_n), nrow(dt)))]
  dt[, .(rank = .I, reporter_iso3, reporter_name, trade_balance_usd)]
}

map_deficit_ranking <- function(year_dt, top_n = 10L) {
  dt <- data.table::as.data.table(year_dt)
  dt <- dt[!is.na(trade_balance_usd) & trade_balance_usd < 0]
  data.table::setorderv(dt, c("trade_balance_usd", "reporter_iso3"), c(1L, 1L))
  dt <- dt[seq_len(min(as.integer(top_n), nrow(dt)))]
  dt[, .(rank = .I, reporter_iso3, reporter_name, trade_balance_usd)]
}

map_coverage_diagnostics <- function(year_dt, joined, coverage, metric, snap = NULL) {
  xw <- joined$crosswalk
  dt <- data.table::as.data.table(year_dt)
  list(
    n_source = nrow(dt),
    n_matched = sum(xw$geometry_match_status == "matched", na.rm = TRUE),
    n_unmatched = sum(xw$geometry_match_status == "unmatched", na.rm = TRUE),
    unmatched_iso3 = xw[geometry_match_status == "unmatched"]$source_iso3,
    coverage_pct = coverage$coverage_pct,
    coverage_method = coverage$method,
    missing_metric = sum(is.na(map_metric_values(dt, metric))),
    missing_gdp = if ("gdp_current_usd" %in% names(dt)) {
      sum(is.na(dt$gdp_current_usd) | dt$gdp_current_usd <= 0)
    } else {
      NA_integer_
    },
    missing_population = if ("population_total" %in% names(dt)) {
      sum(is.na(dt$population_total) | dt$population_total <= 0)
    } else {
      NA_integer_
    },
    global_trade_status = snap$pipeline_status$global_trade %||% "complete",
    detailed_status = snap$pipeline_status$detailed_trade %||% "partial",
    ingested_at = if ("latest_ingested_at" %in% names(dt) && nrow(dt)) {
      max(as.character(dt$latest_ingested_at), na.rm = TRUE)
    } else {
      NA_character_
    }
  )
}

map_accessibility_summary <- function(year_dt, metric, selected_iso3 = NULL) {
  vals <- map_metric_values(year_dt, metric)
  ok <- is.finite(vals)
  parts <- c(
    paste0("Economies with values: ", sum(ok)),
    paste0("Missing values: ", sum(!ok))
  )
  if (any(ok)) {
    parts <- c(
      parts,
      paste0("Highest: ", format_map_metric_value(max(vals, na.rm = TRUE), metric)),
      paste0("Lowest: ", format_map_metric_value(min(vals, na.rm = TRUE), metric))
    )
  }
  if (!is.null(selected_iso3) && nzchar(selected_iso3)) {
    parts <- c(parts, paste0("Selected country: ", selected_iso3))
  }
  paste(parts, collapse = ". ")
}

map_table_data <- function(year_dt, crosswalk) {
  dt <- data.table::as.data.table(year_dt)
  xw <- data.table::as.data.table(crosswalk)
  xw_match <- xw[!is.na(source_iso3), .(reporter_iso3 = source_iso3, geometry_match_status)]
  out <- xw_match[dt, on = "reporter_iso3"]
  out[is.na(geometry_match_status), geometry_match_status := "unmatched"]
  cols <- c(
    "reporter_name", "reporter_iso3", "year",
    "imports_value_usd", "exports_value_usd", "total_trade_value_usd", "trade_balance_usd",
    "gdp_current_usd", "total_trade_pct_gdp", "total_trade_per_capita_usd",
    "geometry_match_status"
  )
  out[, intersect(cols, names(out)), with = FALSE]
}
