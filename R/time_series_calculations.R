prepare_ts_global <- function(cy) {
  dt <- data.table::as.data.table(cy)
  if (!nrow(dt)) return(dt)
  dt <- dt[
    !is.na(reporter_iso3) &
      nchar(as.character(reporter_iso3)) == 3L &
      !(reporter_iso3 %in% TS_AGGREGATE_ISO3)
  ]
  dt[, `:=`(
    year = as.integer(year),
    reporter_iso3 = as.character(reporter_iso3),
    imports_value_usd = sanitize_chart_numeric(imports_value_usd),
    exports_value_usd = sanitize_chart_numeric(exports_value_usd),
    total_trade_value_usd = sanitize_chart_numeric(total_trade_value_usd),
    trade_balance_usd = sanitize_chart_numeric(trade_balance_usd)
  )]

  if ("gdp_current_usd" %in% names(dt) && "total_trade_pct_gdp" %in% names(dt)) {
    bad <- is.na(dt$gdp_current_usd) | !is.finite(dt$gdp_current_usd) | dt$gdp_current_usd <= 0
    dt[bad, `:=`(total_trade_pct_gdp = NA_real_, trade_balance_pct_gdp = NA_real_)]
  }
  if ("population_total" %in% names(dt) && "total_trade_per_capita_usd" %in% names(dt)) {
    badp <- is.na(dt$population_total) | !is.finite(dt$population_total) | dt$population_total <= 0
    dt[badp, total_trade_per_capita_usd := NA_real_]
  }
  data.table::setkeyv(dt, c("year", "reporter_iso3"))
  dt
}

ts_year_choices <- function(cy) {
  dt <- prepare_ts_global(cy)
  if (!nrow(dt)) return(integer())
  sort(unique(as.integer(dt$year)))
}

ts_default_year_range <- function(cy) {
  ys <- ts_year_choices(cy)
  if (!length(ys)) return(c(NA_integer_, NA_integer_))
  c(min(ys), max(ys))
}

filter_ts_years <- function(dt, year_min, year_max) {
  dt <- data.table::as.data.table(dt)
  ymin <- if (is.null(year_min) || length(year_min) == 0L) NA_integer_ else as.integer(year_min)[1]
  ymax <- if (is.null(year_max) || length(year_max) == 0L) NA_integer_ else as.integer(year_max)[1]
  if (!is.na(ymin) && !is.na(ymax) && ymin > ymax) {
    tmp <- ymin; ymin <- ymax; ymax <- tmp
  }
  if (!is.na(ymin)) dt <- dt[year >= ymin]
  if (!is.na(ymax)) dt <- dt[year <= ymax]
  dt
}

validate_comparison_economies <- function(isos, max_n = 5L, primary = NULL) {
  isos <- unique(as.character(isos))
  isos <- isos[nzchar(isos) & !(isos %in% TS_AGGREGATE_ISO3) & nchar(isos) == 3L]
  prim <- normalize_primary_economy(primary)
  if (!is.na(prim)) {
    isos <- unique(c(prim, isos))
  }
  if (length(isos) > as.integer(max_n)) isos <- isos[seq_len(as.integer(max_n))]
  isos
}

default_comparison_economies <- function(cy, primary = NULL, n = 4L) {
  dt <- prepare_ts_global(cy)
  if (!nrow(dt)) return(character())
  latest <- max(dt$year, na.rm = TRUE)
  ranks <- dt[year == latest & !is.na(total_trade_value_usd),
              .(total = total_trade_value_usd[1]), by = .(reporter_iso3)]
  data.table::setorderv(ranks, c("total", "reporter_iso3"), c(-1L, 1L))
  picks <- ranks$reporter_iso3
  if (!is.null(primary) && nzchar(primary)) {
    picks <- c(as.character(primary), setdiff(picks, primary))
  }
  validate_comparison_economies(picks, max_n = n, primary = NULL)
}

aggregate_global_series <- function(cy, year_min = NULL, year_max = NULL) {
  dt <- filter_ts_years(prepare_ts_global(cy), year_min, year_max)
  if (!nrow(dt)) {
    return(data.table::data.table(
      year = integer(), imports = numeric(), exports = numeric(),
      total = numeric(), balance = numeric()
    ))
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

empty_economy_metric_series <- function() {
  data.table::data.table(
    year = integer(), value = numeric(),
    reporter_iso3 = character(), reporter_name = character(), series = character()
  )
}

normalize_primary_economy <- function(iso3) {
  if (is.null(iso3) || length(iso3) == 0L) return(NA_character_)
  iso <- as.character(iso3[[1]])
  if (length(iso) != 1L || is.na(iso) || !nzchar(iso)) return(NA_character_)
  if (!(nchar(iso) == 3L) || iso %in% TS_AGGREGATE_ISO3) return(NA_character_)
  iso
}

economy_metric_series <- function(cy, iso3, metric = "total_trade",
                                    year_min = NULL, year_max = NULL) {
  iso <- normalize_primary_economy(iso3)
  if (is.na(iso)) return(empty_economy_metric_series())
  dt <- filter_ts_years(prepare_ts_global(cy), year_min, year_max)
  dt <- dt[reporter_iso3 == iso]
  col <- ts_metric_column(metric)
  if (!col %in% names(dt)) {
    return(empty_economy_metric_series())
  }
  out <- dt[, .(
    year,
    value = sanitize_chart_numeric(get(col)),
    reporter_iso3 = iso,
    reporter_name = reporter_name[1],
    series = paste0(reporter_name[1], " (", iso, ")")
  )]
  data.table::setorderv(out, "year")
  unique(out, by = "year")
}

comparison_metric_series <- function(cy, isos, metric = "total_trade",
                                       year_min = NULL, year_max = NULL,
                                       max_n = 5L) {
  isos <- validate_comparison_economies(isos, max_n = max_n)
  parts <- lapply(isos, function(iso) {
    economy_metric_series(cy, iso, metric, year_min, year_max)
  })
  if (!length(parts)) {
    return(data.table::data.table(
      year = integer(), value = numeric(), reporter_iso3 = character(), series = character()
    ))
  }
  data.table::rbindlist(parts, fill = TRUE)
}

calc_yoy_pct <- function(years, values, allow = TRUE) {
  years <- as.integer(years)
  values <- sanitize_chart_numeric(values)
  n <- length(values)
  out <- rep(NA_real_, n)
  if (!isTRUE(allow) || n < 2L) return(out)
  ord <- order(years)
  y <- years[ord]
  v <- values[ord]
  res <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i == 1L) next
    if (is.na(y[i]) || is.na(y[i - 1L]) || (y[i] - y[i - 1L]) != 1L) next
    if (is.na(v[i]) || is.na(v[i - 1L]) || v[i - 1L] == 0) next
    res[i] <- 100 * (v[i] / v[i - 1L] - 1)
  }
  out[ord] <- res
  sanitize_chart_numeric(out)
}

calc_balance_change <- function(years, values) {
  years <- as.integer(years)
  values <- sanitize_chart_numeric(values)
  n <- length(values)
  out <- rep(NA_real_, n)
  if (n < 2L) return(out)
  ord <- order(years)
  y <- years[ord]
  v <- values[ord]
  res <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    if (i == 1L) next
    if (is.na(y[i]) || is.na(y[i - 1L]) || (y[i] - y[i - 1L]) != 1L) next
    if (is.na(v[i]) || is.na(v[i - 1L])) next
    res[i] <- v[i] - v[i - 1L]
  }
  out[ord] <- res
  sanitize_chart_numeric(out)
}

select_index_baseline <- function(years, values) {
  years <- as.integer(years)
  values <- sanitize_chart_numeric(values)
  ord <- order(years)
  for (i in ord) {
    if (!is.na(values[i]) && is.finite(values[i]) && values[i] > 0) {
      return(list(year = years[i], value = values[i], index = i))
    }
  }
  list(year = NA_integer_, value = NA_real_, index = NA_integer_)
}

calc_index <- function(years, values, baseline_year = NULL, baseline_value = NULL) {
  years <- as.integer(years)
  values <- sanitize_chart_numeric(values)
  if (is.null(baseline_year) || is.na(baseline_year) ||
      is.null(baseline_value) || is.na(baseline_value) || baseline_value <= 0) {
    bl <- select_index_baseline(years, values)
    baseline_year <- bl$year
    baseline_value <- bl$value
  }
  idx <- rep(NA_real_, length(values))
  if (is.na(baseline_value) || baseline_value <= 0) {
    return(list(index = idx, baseline_year = baseline_year, baseline_value = baseline_value))
  }
  ok <- !is.na(values)
  idx[ok] <- 100 * values[ok] / baseline_value
  list(
    index = sanitize_chart_numeric(idx),
    baseline_year = as.integer(baseline_year),
    baseline_value = as.numeric(baseline_value)
  )
}

calc_cagr <- function(start_value, end_value, start_year, end_year) {
  start_value <- sanitize_chart_numeric(start_value)
  end_value <- sanitize_chart_numeric(end_value)
  start_year <- as.integer(start_year)
  end_year <- as.integer(end_year)
  if (is.na(start_value) || is.na(end_value) || is.na(start_year) || is.na(end_year)) {
    return(NA_real_)
  }
  if (start_value <= 0 || end_value <= 0) return(NA_real_)
  n <- end_year - start_year
  if (n <= 0) return(NA_real_)
  sanitize_chart_numeric(((end_value / start_value)^(1 / n) - 1) * 100)
}

apply_series_transform <- function(series_dt, metric, transform = "absolute") {
  dt <- data.table::as.data.table(series_dt)
  if (!nrow(dt)) {
    dt[, `:=`(display_value = numeric(), transform = character())]
    return(dt)
  }
  transform <- as.character(transform %||% "absolute")
  if (identical(transform, "yoy")) {
    if (ts_metric_allows_pct_growth(metric)) {
      dt[, display_value := calc_yoy_pct(year, value, allow = TRUE), by = series]
    } else {
      dt[, display_value := calc_balance_change(year, value), by = series]
      transform <- "balance_change"
    }
  } else if (identical(transform, "index")) {
    series_ids <- unique(as.character(dt$series))
    parts <- lapply(series_ids, function(sid) {
      chunk <- dt[series == sid]
      ix <- calc_index(chunk$year, chunk$value)
      chunk[, `:=`(
        display_value = ix$index,
        baseline_year = ix$baseline_year,
        baseline_value = ix$baseline_value
      )]
      chunk
    })
    dt <- data.table::rbindlist(parts, fill = TRUE)
  } else if (identical(transform, "share")) {

    dt[, display_value := {
      tot <- sum(value, na.rm = TRUE)
      if (!is.finite(tot) || tot == 0) rep(NA_real_, .N) else 100 * value / tot
    }, by = year]
  } else {
    dt[, display_value := value]
  }
  dt[, transform := transform]
  dt
}

import_export_decomposition <- function(cy, scope = "global", iso3 = NULL,
                                          year_min = NULL, year_max = NULL) {
  iso <- normalize_primary_economy(iso3)
  if (identical(scope, "global") || is.na(iso)) {
    ser <- aggregate_global_series(cy, year_min, year_max)
    ser[, `:=`(
      share_imports = ifelse(total > 0, 100 * imports / total, NA_real_),
      share_exports = ifelse(total > 0, 100 * exports / total, NA_real_)
    )]
    return(ser)
  }
  dt <- filter_ts_years(prepare_ts_global(cy), year_min, year_max)
  dt <- dt[reporter_iso3 == iso]
  out <- unique(dt[, .(
    year,
    imports = imports_value_usd,
    exports = exports_value_usd,
    total = total_trade_value_usd,
    balance = trade_balance_usd
  )], by = "year")
  data.table::setorderv(out, "year")
  out[, `:=`(
    share_imports = ifelse(!is.na(total) & total > 0, 100 * imports / total, NA_real_),
    share_exports = ifelse(!is.na(total) & total > 0, 100 * exports / total, NA_real_)
  )]
  out
}

latest_year_ranking <- function(cy, metric = "total_trade", year = NULL, top_n = 10L) {
  dt <- prepare_ts_global(cy)
  if (!nrow(dt)) return(data.table::data.table())
  if (is.null(year) || is.na(year)) year <- max(dt$year, na.rm = TRUE)
  col <- ts_metric_column(metric)
  ydt <- dt[year == as.integer(year) & !is.na(get(col))]
  data.table::setorderv(ydt, c(col, "reporter_iso3"),
                        c(if (ts_metric_is_signed(metric)) -1L else -1L, 1L))
  ydt <- ydt[seq_len(min(as.integer(top_n), nrow(ydt)))]
  ydt[, .(rank = .I, reporter_iso3, reporter_name, year,
          value = get(col), metric = metric)]
}

ts_kpi_single_or_global <- function(series_dt, metric, transform = "absolute") {
  dt <- data.table::as.data.table(series_dt)
  if (!nrow(dt)) {
    return(list(
      latest_value = NA_real_, yoy = NA_real_, cagr = NA_real_,
      period_high = NA_real_, period_low = NA_real_, latest_year = NA_integer_,
      baseline_year = NA_integer_
    ))
  }
  data.table::setorderv(dt, "year")
  latest <- dt[.N]
  prev <- if (nrow(dt) >= 2L) dt[nrow(dt) - 1L] else NULL
  yoy <- if (!is.null(prev) && ts_metric_allows_pct_growth(metric)) {
    calc_yoy_pct(c(prev$year, latest$year), c(prev$value, latest$value))[2]
  } else if (!is.null(prev)) {
    calc_balance_change(c(prev$year, latest$year), c(prev$value, latest$value))[2]
  } else NA_real_
  first_ok <- dt[!is.na(value) & value > 0]
  cagr <- if (nrow(first_ok) && !is.na(latest$value) && latest$value > 0) {
    calc_cagr(first_ok$value[1], latest$value, first_ok$year[1], latest$year)
  } else NA_real_
  list(
    latest_value = latest$value,
    latest_year = latest$year,
    yoy = yoy,
    cagr = cagr,
    period_high = suppressWarnings(max(dt$value, na.rm = TRUE)),
    period_low = suppressWarnings(min(dt$value, na.rm = TRUE)),
    n_years = data.table::uniqueN(dt$year)
  )
}

ts_kpi_comparison <- function(comp_dt, metric) {
  dt <- data.table::as.data.table(comp_dt)
  if (!nrow(dt)) {
    return(list(n_economies = 0L, highest_latest = NA_real_,
                fastest_cagr_iso = NA_character_, largest_decline_iso = NA_character_,
                median_latest = NA_real_))
  }
  latest_y <- max(dt$year, na.rm = TRUE)
  latest <- dt[year == latest_y]

  cagrs <- dt[, {
    ok <- !is.na(value) & value > 0
    if (!any(ok)) .(cagr = NA_real_) else {
      s <- value[ok][1]; sy <- year[ok][1]
      e <- value[.N]; ey <- year[.N]
      .(cagr = calc_cagr(s, e, sy, ey))
    }
  }, by = .(reporter_iso3, series)]
  data.table::setorderv(cagrs, c("cagr", "reporter_iso3"), c(-1L, 1L))
  best <- cagrs[!is.na(cagr)][1]
  worst <- cagrs[!is.na(cagr)][order(cagr, reporter_iso3)][1]
  list(
    n_economies = data.table::uniqueN(dt$reporter_iso3),
    highest_latest = if (nrow(latest)) max(latest$value, na.rm = TRUE) else NA_real_,
    highest_latest_iso = if (nrow(latest)) latest[which.max(value)]$reporter_iso3 else NA_character_,
    fastest_cagr_iso = best$reporter_iso3 %||% NA_character_,
    fastest_cagr = best$cagr %||% NA_real_,
    largest_decline_iso = worst$reporter_iso3 %||% NA_character_,
    largest_decline_cagr = worst$cagr %||% NA_real_,
    median_latest = if (nrow(latest)) stats::median(latest$value, na.rm = TRUE) else NA_real_,
    latest_year = latest_y
  )
}

ts_accessibility_summary <- function(series_dt, metric, scope_label = "") {
  dt <- data.table::as.data.table(series_dt)
  if (!nrow(dt)) return(paste("No series available for", scope_label))
  paste0(
    scope_label, " — years ", min(dt$year, na.rm = TRUE), "–", max(dt$year, na.rm = TRUE),
    "; metric ", ts_metric_label(metric),
    "; observations ", nrow(dt),
    "; missing values ", sum(is.na(dt$value)),
    ". Missing years are not interpolated. Values are current US dollars unless transformed."
  )
}
