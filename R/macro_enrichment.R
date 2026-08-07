MACRO_FIELDS <- c(
  "gdp_current_usd", "population_total", "cpi_index",
  "inflation_annual_pct", "gdp_per_capita_usd"
)

prefix_macro <- function(wide_dt, prefix) {
  dt <- data.table::as.data.table(data.table::copy(wide_dt))
  keep <- c("iso3", "year", MACRO_FIELDS)
  for (k in keep) if (!k %in% names(dt)) dt[, (k) := if (k %in% c("iso3")) NA_character_ else if (k == "year") NA_integer_ else NA_real_]
  out <- dt[, ..keep]
  data.table::setnames(
    out,
    old = MACRO_FIELDS,
    new = paste0(prefix, MACRO_FIELDS)
  )
  data.table::setnames(out, "iso3", paste0(prefix, "iso3_join"))
  out
}

safe_pct <- function(num, den) {
  data.table::fifelse(!is.na(num) & !is.na(den) & den != 0, 100 * num / den, NA_real_)
}

safe_per_capita <- function(num, pop) {
  data.table::fifelse(!is.na(num) & !is.na(pop) & pop > 0, num / pop, NA_real_)
}

safe_yoy_pct <- function(curr, prev) {
  data.table::fifelse(
    !is.na(curr) & !is.na(prev) & prev != 0,
    100 * (curr - prev) / prev,
    NA_real_
  )
}

enrich_trade_global <- function(trade_global, wdi_wide) {
  trade <- data.table::as.data.table(data.table::copy(trade_global))
  n_before <- nrow(trade)
  wide <- data.table::as.data.table(wdi_wide)
  rep_macro <- prefix_macro(wide, "reporter_")
  data.table::setnames(rep_macro, "reporter_iso3_join", "reporter_iso3")

  trade[, `:=`(reporter_iso3 = as.character(reporter_iso3), year = as.integer(year))]
  rep_macro[, `:=`(reporter_iso3 = as.character(reporter_iso3), year = as.integer(year))]
  out <- rep_macro[trade, on = .(reporter_iso3, year)]

  out[, trade_value_per_capita_usd := safe_per_capita(trade_value_usd, reporter_population_total)]
  out[, trade_value_pct_gdp := safe_pct(trade_value_usd, reporter_gdp_current_usd)]
  stopifnot(nrow(out) == n_before)
  list(data = out, n_before = n_before, n_after = nrow(out))
}

enrich_trade_detailed <- function(trade_detailed, wdi_wide,
                                    universe_checksum = NULL,
                                    production_status = "partial") {
  trade <- data.table::as.data.table(data.table::copy(trade_detailed))
  n_before <- nrow(trade)
  wide <- data.table::as.data.table(wdi_wide)

  rep_macro <- prefix_macro(wide, "reporter_")
  data.table::setnames(rep_macro, "reporter_iso3_join", "reporter_iso3")
  par_macro <- prefix_macro(wide, "partner_")
  data.table::setnames(par_macro, "partner_iso3_join", "partner_iso3")

  trade[, `:=`(
    reporter_iso3 = as.character(reporter_iso3),
    partner_iso3 = as.character(partner_iso3),
    year = as.integer(year)
  )]
  out <- rep_macro[trade, on = .(reporter_iso3, year)]
  out <- par_macro[out, on = .(partner_iso3, year)]

  if (is.null(universe_checksum) && "universe_checksum" %in% names(out)) {
    universe_checksum <- out$universe_checksum[1]
  }
  out[, universe_checksum := as.character(universe_checksum %||% NA_character_)]
  out[, production_status := as.character(production_status %||% "partial")]
  stopifnot(nrow(out) == n_before)
  list(data = out, n_before = n_before, n_after = nrow(out))
}

build_country_year_analytics <- function(trade_global, wdi_wide) {
  trade <- data.table::as.data.table(trade_global)
  world <- trade[partner_iso3 == "W00" | as.character(partner_code) == "0"]
  if (!nrow(world)) world <- trade

  flow_sum <- world[, .(
    value = sum(trade_value_usd, na.rm = TRUE),
    latest_ingested_at = max(as.character(ingested_at), na.rm = TRUE),
    latest_source_updated_at = if ("source_updated_at" %in% names(world)) {
      max(as.character(source_updated_at), na.rm = TRUE)
    } else {
      NA_character_
    }
  ), by = .(reporter_code, reporter_iso3, reporter_name, year, flow_code)]

  wide_flows <- data.table::dcast(
    flow_sum,
    reporter_code + reporter_iso3 + reporter_name + year +
      latest_ingested_at + latest_source_updated_at ~ flow_code,
    value.var = "value"
  )
  if (!"M" %in% names(wide_flows)) wide_flows[, M := NA_real_]
  if (!"X" %in% names(wide_flows)) wide_flows[, X := NA_real_]
  data.table::setnames(wide_flows, c("M", "X"), c("imports_value_usd", "exports_value_usd"))

  wide_flows[, total_trade_value_usd := ifelse(
    is.na(imports_value_usd) & is.na(exports_value_usd), NA_real_,
    ifelse(is.na(imports_value_usd), 0, imports_value_usd) +
      ifelse(is.na(exports_value_usd), 0, exports_value_usd)
  )]
  wide_flows[, trade_balance_usd := ifelse(
    is.na(exports_value_usd) & is.na(imports_value_usd), NA_real_,
    ifelse(is.na(exports_value_usd), 0, exports_value_usd) -
      ifelse(is.na(imports_value_usd), 0, imports_value_usd)
  )]

  wdi <- data.table::as.data.table(wdi_wide)
  for (f in MACRO_FIELDS) if (!f %in% names(wdi)) wdi[, (f) := NA_real_]
  wdi[, `:=`(iso3 = as.character(iso3), year = as.integer(year))]
  wide_flows[, `:=`(reporter_iso3 = as.character(reporter_iso3), year = as.integer(year))]

  out <- merge(
    wide_flows,
    wdi[, c("iso3", "year", MACRO_FIELDS), with = FALSE],
    by.x = c("reporter_iso3", "year"),
    by.y = c("iso3", "year"),
    all.x = TRUE
  )

  out[, imports_pct_gdp := safe_pct(imports_value_usd, gdp_current_usd)]
  out[, exports_pct_gdp := safe_pct(exports_value_usd, gdp_current_usd)]
  out[, total_trade_pct_gdp := safe_pct(total_trade_value_usd, gdp_current_usd)]
  out[, trade_balance_pct_gdp := safe_pct(trade_balance_usd, gdp_current_usd)]
  out[, imports_per_capita_usd := safe_per_capita(imports_value_usd, population_total)]
  out[, exports_per_capita_usd := safe_per_capita(exports_value_usd, population_total)]
  out[, total_trade_per_capita_usd := safe_per_capita(total_trade_value_usd, population_total)]
  out[, trade_balance_per_capita_usd := safe_per_capita(trade_balance_usd, population_total)]

  data.table::setorderv(out, c("reporter_iso3", "year"))
  out[, `:=`(
    imports_yoy_pct = {
      prev <- data.table::shift(imports_value_usd)
      py <- data.table::shift(year)
      data.table::fifelse(!is.na(py) & year == py + 1L, safe_yoy_pct(imports_value_usd, prev), NA_real_)
    },
    exports_yoy_pct = {
      prev <- data.table::shift(exports_value_usd)
      py <- data.table::shift(year)
      data.table::fifelse(!is.na(py) & year == py + 1L, safe_yoy_pct(exports_value_usd, prev), NA_real_)
    },
    total_trade_yoy_pct = {
      prev <- data.table::shift(total_trade_value_usd)
      py <- data.table::shift(year)
      data.table::fifelse(!is.na(py) & year == py + 1L, safe_yoy_pct(total_trade_value_usd, prev), NA_real_)
    },
    trade_balance_change_usd = {
      prev <- data.table::shift(trade_balance_usd)
      py <- data.table::shift(year)
      data.table::fifelse(
        !is.na(py) & year == py + 1L & !is.na(trade_balance_usd) & !is.na(prev),
        trade_balance_usd - prev, NA_real_
      )
    }
  ), by = reporter_iso3]

  cols <- c(
    "reporter_code", "reporter_iso3", "reporter_name", "year",
    "imports_value_usd", "exports_value_usd", "total_trade_value_usd", "trade_balance_usd",
    "gdp_current_usd", "population_total", "cpi_index", "inflation_annual_pct", "gdp_per_capita_usd",
    "imports_pct_gdp", "exports_pct_gdp", "total_trade_pct_gdp", "trade_balance_pct_gdp",
    "imports_per_capita_usd", "exports_per_capita_usd", "total_trade_per_capita_usd",
    "trade_balance_per_capita_usd",
    "imports_yoy_pct", "exports_yoy_pct", "total_trade_yoy_pct", "trade_balance_change_usd",
    "latest_source_updated_at", "latest_ingested_at"
  )
  for (c in cols) if (!c %in% names(out)) out[, (c) := NA]
  out[, ..cols]
}

build_macro_coverage_report <- function(long_dt, universe_dt) {
  long <- data.table::as.data.table(long_dt)
  uni <- data.table::as.data.table(universe_dt)[included == TRUE]
  expected <- nrow(uni)
  inds <- unique(as.character(long$indicator_code))
  years <- sort(unique(as.integer(long$year)))
  if (!length(inds)) inds <- character()
  if (!length(years)) years <- 2019:2024

  grid <- data.table::CJ(indicator_code = inds, year = years, sorted = TRUE)
  if (!nrow(grid) && length(inds) == 0L) {
    return(data.table::data.table(
      indicator_code = character(), year = integer(),
      expected_country_count = integer(), observed_value_count = integer(),
      missing_value_count = integer(), coverage_pct = numeric(),
      generated_at = character()
    ))
  }

  obs <- long[!is.na(value), .(observed_value_count = data.table::uniqueN(iso3)),
              by = .(indicator_code, year)]
  out <- obs[grid, on = .(indicator_code, year)]
  out[is.na(observed_value_count), observed_value_count := 0L]
  out[, expected_country_count := as.integer(expected)]
  out[, missing_value_count := pmax(0L, expected_country_count - observed_value_count)]
  out[, coverage_pct := data.table::fifelse(
    expected_country_count > 0,
    100 * observed_value_count / expected_country_count,
    NA_real_
  )]
  out[, generated_at := utc_now()]
  out[]
}

join_coverage <- function(trade_dt, wide_dt, iso_col = "reporter_iso3") {
  trade <- data.table::as.data.table(trade_dt)
  wide <- data.table::as.data.table(wide_dt)
  keys <- unique(trade[, .(iso3 = get(iso_col), year = as.integer(year))])
  keys <- keys[!is.na(iso3) & nzchar(iso3)]
  wkeys <- unique(wide[!is.na(gdp_current_usd) | !is.na(population_total),
                       .(iso3 = as.character(iso3), year = as.integer(year))])
  matched <- keys[wkeys, on = .(iso3, year), nomatch = 0]
  list(
    expected = nrow(keys),
    matched = nrow(matched),
    coverage_pct = if (nrow(keys)) 100 * nrow(matched) / nrow(keys) else NA_real_
  )
}
