empty_trade_table <- function() {
  data.table::data.table(
    period = character(), year = integer(), frequency = character(),
    reporter_code = character(), reporter_iso3 = character(),
    reporter_name = character(), partner_code = character(),
    partner_iso3 = character(), partner_name = character(),
    flow_code = character(), flow_name = character(),
    hs_revision = character(), hs_code = character(), hs_level = integer(),
    commodity_description = character(), trade_value_usd = numeric(),
    net_weight_kg = numeric(), quantity = numeric(),
    quantity_unit = character(), source_updated_at = character(),
    ingested_at = character()
  )
}

empty_wdi_long <- function() {
  data.table::data.table(
    iso3 = character(), country_name = character(), year = integer(),
    indicator_code = character(), indicator_name = character(),
    value = numeric(), source_updated_at = character(),
    ingested_at = character()
  )
}

clean_trade_data <- function(raw_dt, cfg = load_config(),
                             countries = build_country_reference(),
                             flows = build_flow_reference(),
                             hs = build_hs85_reference()) {
  if (is.null(raw_dt) || !nrow(raw_dt)) {
    return(list(
      trade = empty_trade_table(),
      unmatched_reporters = data.table::data.table(),
      unmatched_partners = data.table::data.table(),
      unmatched_hs = data.table::data.table()
    ))
  }

  dt <- data.table::as.data.table(data.table::copy(raw_dt))
  dt[, reporter_code := as_char_code(reporter_code)]
  dt[, partner_code := as_char_code(partner_code)]
  dt[, flow_code := as_char_code(flow_code)]
  dt[, hs_code := as_char_code(cmd_code)]
  dt[, year := {
    y <- suppressWarnings(as.integer(ref_year))
    miss <- is.na(y)
    if (any(miss)) {
      y[miss] <- suppressWarnings(as.integer(substr(as.character(period[miss]), 1, 4)))
    }
    y
  }]
  dt[, frequency := as.character(cfg$pilot$frequency)]
  dt[, trade_value_usd := suppressWarnings(as.numeric(primary_value))]
  dt[, net_weight_kg := suppressWarnings(as.numeric(net_wgt))]
  dt[, quantity := suppressWarnings(as.numeric(qty))]
  dt[, quantity_unit := as_char_code(qty_unit)]
  dt[, commodity_description := as_char_code(cmd_desc)]
  dt[, hs_level := {
    lvl <- suppressWarnings(as.integer(aggr_level))
    ifelse(is.na(lvl), nchar(as.character(hs_code)), lvl)
  }]
  dt[, hs_revision := "HS"]
  if (!"source_updated_at" %in% names(dt)) dt[, source_updated_at := NA_character_]
  if (!"ingested_at" %in% names(dt)) dt[, ingested_at := utc_now()]

  rep_map <- countries[, .(
    reporter_code = comtrade_code,
    reporter_iso3 = iso3,
    reporter_name = display_name
  )]
  data.table::setkey(rep_map, reporter_code)
  data.table::setkey(dt, reporter_code)
  dt <- rep_map[dt]

  part_map <- countries[, .(
    partner_code = comtrade_code,
    partner_iso3 = iso3,
    partner_name = display_name
  )]
  data.table::setkey(part_map, partner_code)
  data.table::setkey(dt, partner_code)
  dt <- part_map[dt]

  flow_map <- flows[, .(flow_code, flow_name)]
  data.table::setkey(flow_map, flow_code)
  data.table::setkey(dt, flow_code)
  dt <- flow_map[dt]
  if ("flow_desc" %in% names(dt)) {
    dt[is.na(flow_name) | !nzchar(as.character(flow_name)),
       flow_name := as.character(flow_desc)]
  }

  hs_map <- hs[, .(hs_code, hs_ref_level = hs_level, hs_ref_rev = hs_revision,
                   hs_ref_desc = commodity_description)]
  data.table::setkey(hs_map, hs_code)
  data.table::setkey(dt, hs_code)
  dt <- hs_map[dt]
  dt[is.na(commodity_description) | !nzchar(as.character(commodity_description)),
     commodity_description := as.character(hs_ref_desc)]
  dt[is.na(hs_revision), hs_revision := as.character(hs_ref_rev)]
  dt[is.na(hs_level), hs_level := as.integer(hs_ref_level)]

  unmatched_reporters <- unique(dt[
    is.na(reporter_iso3) | !nzchar(as.character(reporter_iso3)),
    .(reporter_code)
  ])
  unmatched_partners <- unique(dt[
    is.na(partner_iso3) | !nzchar(as.character(partner_iso3)),
    .(partner_code)
  ])
  unmatched_hs <- unique(dt[
    is.na(hs_code) | !nzchar(as.character(hs_code)),
    .(hs_code)
  ])

  before <- nrow(dt)
  dt <- dt[!is.na(trade_value_usd) & trade_value_usd >= 0]
  dropped_invalid_value_rows <- before - nrow(dt)

  keep <- c(
    "period", "year", "frequency",
    "reporter_code", "reporter_iso3", "reporter_name",
    "partner_code", "partner_iso3", "partner_name",
    "flow_code", "flow_name",
    "hs_revision", "hs_code", "hs_level", "commodity_description",
    "trade_value_usd", "net_weight_kg", "quantity", "quantity_unit",
    "source_updated_at", "ingested_at"
  )
  for (col in keep) if (!col %in% names(dt)) dt[, (col) := NA]
  trade <- dt[, ..keep]
  trade[, hs_code := as.character(hs_code)]
  trade[, year := as.integer(year)]
  trade[, hs_level := as.integer(hs_level)]
  trade[, trade_value_usd := as.numeric(trade_value_usd)]

  key_cols <- c(
    "year", "frequency", "reporter_code", "partner_code",
    "flow_code", "hs_code"
  )
  trade <- unique(trade, by = key_cols)
  attr(trade, "dropped_invalid_value_rows") <- dropped_invalid_value_rows

  list(
    trade = trade,
    unmatched_reporters = unmatched_reporters,
    unmatched_partners = unmatched_partners,
    unmatched_hs = unmatched_hs
  )
}

clean_wdi_data <- function(raw_long, cfg = load_config(),
                           countries = build_country_reference()) {
  if (is.null(raw_long) || !nrow(raw_long)) {
    return(list(long = empty_wdi_long(), wide = data.table::data.table()))
  }
  dt <- data.table::as.data.table(data.table::copy(raw_long))
  dt[, iso3 := as.character(iso3)]
  dt[, year := as.integer(year)]
  dt[, indicator_code := as.character(indicator_code)]
  dt[, indicator_name := as.character(indicator_name)]
  dt[, value := suppressWarnings(as.numeric(value))]
  if (!"ingested_at" %in% names(dt)) dt[, ingested_at := utc_now()]
  if (!"source_updated_at" %in% names(dt)) dt[, source_updated_at := utc_now()]
  if (!"country_name" %in% names(dt)) dt[, country_name := NA_character_]

  ref <- countries[, .(iso3, display_name)]
  data.table::setkey(ref, iso3)
  data.table::setkey(dt, iso3)
  dt <- ref[dt]
  dt[is.na(country_name) | !nzchar(as.character(country_name)),
     country_name := display_name]
  dt[, display_name := NULL]

  dt <- dt[!is.na(iso3) & nzchar(iso3) & !is.na(year) & !is.na(indicator_code)]
  data.table::setorderv(dt, c("iso3", "year", "indicator_code", "ingested_at"))
  dt <- unique(dt, by = c("iso3", "year", "indicator_code"), fromLast = TRUE)

  long <- dt[, .(
    iso3, country_name, year, indicator_code, indicator_name,
    value, source_updated_at, ingested_at
  )]
  wide <- data.table::dcast(
    long,
    iso3 + country_name + year ~ indicator_code,
    value.var = "value"
  )
  list(long = long, wide = wide)
}

build_pilot_country_summary <- function(trade, wdi_wide = NULL) {
  if (is.null(trade) || !nrow(trade)) {
    return(data.table::data.table(
      reporter_iso3 = character(), reporter_name = character(),
      n_partners = integer(), n_rows = integer(),
      total_trade_value_usd = numeric(), latest_ingested_at = character()
    ))
  }
  summary <- trade[, .(
    n_partners = data.table::uniqueN(partner_iso3),
    n_rows = .N,
    total_trade_value_usd = sum(trade_value_usd, na.rm = TRUE),
    latest_ingested_at = max(as.character(ingested_at), na.rm = TRUE)
  ), by = .(reporter_iso3, reporter_name)]

  if (!is.null(wdi_wide) && nrow(wdi_wide) && "NY.GDP.MKTP.CD" %in% names(wdi_wide)) {
    gdp <- wdi_wide[, .SD[which.max(year)], by = iso3][
      , .(reporter_iso3 = iso3, gdp_usd = `NY.GDP.MKTP.CD`, wdi_year = year)
    ]
    summary <- gdp[summary, on = "reporter_iso3"]
  }
  summary
}

trade_balance_by_pair <- function(trade) {
  if (is.null(trade) || !nrow(trade)) return(data.table::data.table())
  exp <- trade[flow_code == "X", .(
    exports = sum(trade_value_usd, na.rm = TRUE)
  ), by = .(year, reporter_iso3, partner_iso3)]
  imp <- trade[flow_code == "M", .(
    imports = sum(trade_value_usd, na.rm = TRUE)
  ), by = .(year, reporter_iso3, partner_iso3)]
  out <- merge(exp, imp, by = c("year", "reporter_iso3", "partner_iso3"), all = TRUE)
  out[is.na(exports), exports := 0]
  out[is.na(imports), imports := 0]
  out[, trade_balance := exports - imports]
  out
}
