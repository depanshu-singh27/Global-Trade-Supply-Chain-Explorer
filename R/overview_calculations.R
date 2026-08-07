AGGREGATE_ISO3_OVERVIEW <- c("EUR", "WLD", "W00", "ASE")

overview_exclude_aggregates <- function(dt, iso_col = "reporter_iso3") {
  dt <- data.table::as.data.table(dt)
  if (!nrow(dt) || !iso_col %in% names(dt)) return(dt)
  dt[!(get(iso_col) %in% AGGREGATE_ISO3_OVERVIEW) &
       grepl("^[A-Z]{3}$", get(iso_col))]
}

choose_overview_default_year <- function(cy, min_share_of_max = 0.75) {
  cy <- overview_exclude_aggregates(data.table::as.data.table(cy))
  if (!nrow(cy)) return(NA_integer_)
  cov <- cy[!is.na(gdp_current_usd), .(n = data.table::uniqueN(reporter_iso3)), by = year]
  if (!nrow(cov)) {
    return(as.integer(max(cy$year, na.rm = TRUE)))
  }
  max_n <- max(cov$n, na.rm = TRUE)
  adequate <- cov[n >= min_share_of_max * max_n]
  as.integer(adequate[order(-year)][1]$year)
}

overview_reporter_choices <- function(cy) {
  cy <- overview_exclude_aggregates(data.table::as.data.table(cy))
  if (!nrow(cy)) return(c("Global overview" = "__GLOBAL__"))
  labs <- unique(cy[, .(reporter_iso3, reporter_name)])
  labs <- labs[order(reporter_name, reporter_iso3)]
  choices <- c("Global overview" = "__GLOBAL__")
  vals <- setNames(as.character(labs$reporter_iso3),
                   paste0(labs$reporter_name, " (", labs$reporter_iso3, ")"))
  c(choices, vals)
}

filter_overview_data <- function(cy,
                                   year = NULL,
                                   reporter = "__GLOBAL__",
                                   years_all = FALSE) {
  dt <- overview_exclude_aggregates(data.table::as.data.table(cy))
  if (!nrow(dt)) return(dt)
  if (!isTRUE(years_all) && !is.null(year) && !is.na(year)) {
    year_sel <- as.integer(year)
    dt <- dt[year == year_sel]
  }
  if (!is.null(reporter) && nzchar(reporter) && reporter != "__GLOBAL__") {
    reporter_sel <- as.character(reporter)
    dt <- dt[reporter_iso3 == reporter_sel]
  }
  dt
}

measure_column <- function(measure = "total") {
  switch(
    as.character(measure),
    "imports" = "imports_value_usd",
    "exports" = "exports_value_usd",
    "balance" = "trade_balance_usd",
    "total_trade_value_usd"
  )
}

overview_kpi_global <- function(cy, year) {
  dt <- filter_overview_data(cy, year = year, reporter = "__GLOBAL__")
  prev <- filter_overview_data(cy, year = as.integer(year) - 1L, reporter = "__GLOBAL__")
  tot <- sum(dt$total_trade_value_usd, na.rm = TRUE)
  imp <- sum(dt$imports_value_usd, na.rm = TRUE)
  exp <- sum(dt$exports_value_usd, na.rm = TRUE)
  bal <- exp - imp
  tot_prev <- if (nrow(prev)) sum(prev$total_trade_value_usd, na.rm = TRUE) else NA_real_
  yoy <- if (!is.na(tot_prev) && tot_prev != 0) 100 * (tot - tot_prev) / tot_prev else NA_real_
  list(
    mode = "global",
    year = as.integer(year),
    total_trade = tot,
    imports = imp,
    exports = exp,
    trade_balance = bal,
    n_reporters = data.table::uniqueN(dt$reporter_iso3),
    yoy_total_pct = yoy,
    checks = list(
      total_equals_sum = isTRUE(abs(tot - (imp + exp)) < 1e-3 * max(1, abs(tot))),
      balance_equals_diff = isTRUE(abs(bal - (exp - imp)) < 1e-6)
    )
  )
}

overview_kpi_reporter <- function(cy, year, reporter_iso3) {
  dt <- filter_overview_data(cy, year = year, reporter = reporter_iso3)
  if (!nrow(dt)) {
    return(list(
      mode = "reporter", year = as.integer(year), reporter_iso3 = reporter_iso3,
      total_trade = NA_real_, imports = NA_real_, exports = NA_real_,
      trade_balance = NA_real_,       trade_pct_gdp = NA_real_, balance_pct_gdp = NA_real_,
      trade_per_capita = NA_real_
    ))
  }
  row <- dt[1]
  list(
    mode = "reporter",
    year = as.integer(year),
    reporter_iso3 = as.character(reporter_iso3),
    reporter_name = as.character(row$reporter_name),
    total_trade = as.numeric(row$total_trade_value_usd),
    imports = as.numeric(row$imports_value_usd),
    exports = as.numeric(row$exports_value_usd),
    trade_balance = as.numeric(row$trade_balance_usd),
    trade_pct_gdp = as.numeric(row$total_trade_pct_gdp),
    balance_pct_gdp = as.numeric(row$trade_balance_pct_gdp),
    trade_per_capita = as.numeric(row$total_trade_per_capita_usd),
    gdp = as.numeric(row$gdp_current_usd),
    population = as.numeric(row$population_total),
    gdp_per_capita = as.numeric(row$gdp_per_capita_usd),
    inflation = as.numeric(row$inflation_annual_pct),
    balance_change = as.numeric(row$trade_balance_change_usd),
    checks = list(
      total_equals_sum = isTRUE(abs(
        (row$total_trade_value_usd %||% 0) -
          ((row$imports_value_usd %||% 0) + (row$exports_value_usd %||% 0))
      ) < 1),
      balance_equals_diff = isTRUE(abs(
        (row$trade_balance_usd %||% 0) -
          ((row$exports_value_usd %||% 0) - (row$imports_value_usd %||% 0))
      ) < 1)
    )
  )
}

overview_trend_series <- function(cy, reporter = "__GLOBAL__") {
  dt <- overview_exclude_aggregates(data.table::as.data.table(cy))
  if (!identical(reporter, "__GLOBAL__")) {
    dt <- dt[reporter_iso3 == as.character(reporter)]
    out <- dt[, .(
      imports = imports_value_usd,
      exports = exports_value_usd,
      total = total_trade_value_usd,
      balance = trade_balance_usd
    ), by = year]
  } else {
    out <- dt[, .(
      imports = sum(imports_value_usd, na.rm = TRUE),
      exports = sum(exports_value_usd, na.rm = TRUE),
      total = sum(total_trade_value_usd, na.rm = TRUE),
      balance = sum(exports_value_usd, na.rm = TRUE) - sum(imports_value_usd, na.rm = TRUE)
    ), by = year]
  }
  data.table::setorderv(out, "year")
  out[, `:=`(
    imports = sanitize_chart_numeric(imports),
    exports = sanitize_chart_numeric(exports),
    total = sanitize_chart_numeric(total),
    balance = sanitize_chart_numeric(balance)
  )]
  out
}

overview_top_economies <- function(cy, year, measure = "total", top_n = 10L,
                                     lowest_balance = FALSE) {
  dt <- filter_overview_data(cy, year = year, reporter = "__GLOBAL__")
  col <- measure_column(measure)
  if (!col %in% names(dt)) stop("Unknown measure column", call. = FALSE)
  dt <- dt[!is.na(get(col))]
  if (!nrow(dt)) return(data.table::data.table())
  if (identical(measure, "balance") && isTRUE(lowest_balance)) {
    dt <- dt[order(get(col), reporter_iso3)]
  } else if (identical(measure, "balance")) {
    dt <- dt[order(-get(col), reporter_iso3)]
  } else {
    dt <- dt[order(-get(col), reporter_iso3)]
  }
  out <- dt[seq_len(min(as.integer(top_n), nrow(dt))),
            .(rank = .I, reporter_iso3, reporter_name,
              value = get(col), measure = measure)]
  out
}

overview_composition <- function(cy, year, reporter = "__GLOBAL__") {
  if (identical(reporter, "__GLOBAL__")) {
    kpi <- overview_kpi_global(cy, year)
    imp <- kpi$imports
    exp <- kpi$exports
  } else {
    kpi <- overview_kpi_reporter(cy, year, reporter)
    imp <- kpi$imports
    exp <- kpi$exports
  }
  tot <- ifelse(is.na(imp), 0, imp) + ifelse(is.na(exp), 0, exp)
  list(
    imports = imp,
    exports = exp,
    total = if (tot == 0 && (is.na(imp) || is.na(exp))) NA_real_ else tot,
    imports_share_pct = if (!is.na(tot) && tot > 0 && !is.na(imp)) 100 * imp / tot else NA_real_,
    exports_share_pct = if (!is.na(tot) && tot > 0 && !is.na(exp)) 100 * exp / tot else NA_real_
  )
}

overview_balance_distribution <- function(cy, year) {
  dt <- filter_overview_data(cy, year = year, reporter = "__GLOBAL__")
  bal <- sanitize_chart_numeric(dt$trade_balance_usd)
  ok <- !is.na(bal)
  list(
    n = sum(ok),
    n_surplus = sum(bal > 0, na.rm = TRUE),
    n_deficit = sum(bal < 0, na.rm = TRUE),
    n_zero = sum(bal == 0, na.rm = TRUE),
    median_balance = if (any(ok)) stats::median(bal[ok]) else NA_real_,
    top_surplus = overview_top_economies(cy, year, "balance", top_n = 5L, lowest_balance = FALSE),
    top_deficit = overview_top_economies(cy, year, "balance", top_n = 5L, lowest_balance = TRUE)
  )
}

overview_macro_scatter <- function(cy, year, use_log = TRUE) {
  dt <- filter_overview_data(cy, year = year, reporter = "__GLOBAL__")
  out <- dt[, .(
    reporter_iso3, reporter_name, year,
    gdp = sanitize_chart_numeric(gdp_current_usd),
    total_trade = sanitize_chart_numeric(total_trade_value_usd),
    population = sanitize_chart_numeric(population_total),
    trade_pct_gdp = sanitize_chart_numeric(total_trade_pct_gdp)
  )]
  excluded_log <- 0L
  if (isTRUE(use_log)) {
    bad <- is.na(out$gdp) | is.na(out$total_trade) | out$gdp <= 0 | out$total_trade <= 0
    excluded_log <- sum(bad)
    out <- out[!bad]
  } else {
    out <- out[!is.na(gdp) & !is.na(total_trade)]
  }
  list(data = out, excluded_log = excluded_log, use_log = isTRUE(use_log))
}

overview_coverage_status <- function(snap) {
  man <- snap$production_manifest %||% list()
  macro <- snap$macro_profile %||% list()
  trade_prof <- snap$trade_profile %||% list()
  v3 <- snap$phase3_validation
  v2 <- snap$production_validation
  n_warn <- 0L
  if (!is.null(v3) && nrow(v3)) n_warn <- n_warn + sum(v3$status == "warning", na.rm = TRUE)
  if (!is.null(v2) && nrow(v2)) n_warn <- n_warn + sum(v2$status == "warning", na.rm = TRUE)

  detailed_status <- man$production_status %||%
    snap$pipeline_status$detailed_trade %||% "partial"
  represented <- as.integer(
    man$represented_reporter_count %||%
      length(macro$detailed_represented_reporters %||% character()) %||% 0L
  )
  selected <- as.integer(man$selected_reporter_count %||% 20L)

  cy <- snap$country_year_analytics
  trade_years <- if (!is.null(cy) && nrow(cy)) range(cy$year, na.rm = TRUE) else c(NA, NA)
  wdi <- snap$wdi_production_wide
  macro_years <- if (!is.null(wdi) && nrow(wdi)) range(wdi$year, na.rm = TRUE) else c(NA, NA)

  list(
    global_trade_status = if (!is.null(snap$trade_global) && nrow(snap$trade_global)) "complete" else "unavailable",
    macro_status = if (!is.null(wdi) && nrow(wdi)) "complete" else "unavailable",
    detailed_status = detailed_status,
    detailed_coverage_label = sprintf("%d/%d", represented, selected),
    detailed_represented = represented,
    detailed_selected = selected,
    trade_ingested_at = if (!is.null(cy) && nrow(cy) && "latest_ingested_at" %in% names(cy)) {
      max(as.character(cy$latest_ingested_at), na.rm = TRUE)
    } else {
      NA_character_
    },
    wdi_ingested_at = if (!is.null(wdi) && nrow(wdi) && "ingested_at" %in% names(wdi)) {
      max(as.character(wdi$ingested_at), na.rm = TRUE)
    } else {
      NA_character_
    },
    trade_year_range = trade_years,
    macro_year_range = macro_years,
    validation_warnings = n_warn,
    universe_checksum = man$universe_version %||% macro$universe_checksum %||% NA_character_
  )
}

kpi_summary_table <- function(kpi) {
  if (identical(kpi$mode, "global")) {
    data.table::data.table(
      metric = c("total_trade_usd", "imports_usd", "exports_usd", "trade_balance_usd",
                 "n_reporters", "yoy_total_pct"),
      value = c(kpi$total_trade, kpi$imports, kpi$exports, kpi$trade_balance,
                kpi$n_reporters, kpi$yoy_total_pct),
      year = kpi$year,
      mode = "global"
    )
  } else {
    data.table::data.table(
      metric = c("total_trade_usd", "imports_usd", "exports_usd", "trade_balance_usd",
                 "trade_pct_gdp", "trade_per_capita_usd"),
      value = c(kpi$total_trade, kpi$imports, kpi$exports, kpi$trade_balance,
                kpi$trade_pct_gdp, kpi$trade_per_capita),
      year = kpi$year,
      mode = "reporter",
      reporter_iso3 = kpi$reporter_iso3
    )
  }
}
