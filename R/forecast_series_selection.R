AGGREGATE_ISO3_FORECAST <- c("WLD", "W00", "EUR", "ASE", "S19", "UNK", "XZZ")

build_annual_forecast_candidates <- function(detailed,
                                              min_annual_value_usd = 1e6,
                                              top_n = 40L,
                                              coverage = NULL) {
  dt <- data.table::as.data.table(detailed)
  empty <- data.table::data.table()
  if (!nrow(dt)) return(empty)

  need <- c("reporter_iso3", "partner_iso3", "hs_code", "flow_code", "trade_value_usd", "year")
  if (!all(need %in% names(dt))) return(empty)

  dt <- dt[
    !is.na(reporter_iso3) & !is.na(partner_iso3) & !is.na(hs_code) & !is.na(flow_code) &
      nchar(as.character(hs_code)) == 4L & startsWith(as.character(hs_code), "85") &
      flow_code %in% c("M", "X") &
      !(partner_iso3 %in% AGGREGATE_ISO3_FORECAST) &
      !(reporter_iso3 %in% AGGREGATE_ISO3_FORECAST) &
      partner_iso3 != reporter_iso3 &
      is.finite(trade_value_usd) & trade_value_usd >= 0
  ]
  if (!nrow(dt)) return(empty)

  ann <- dt[, .(
    annual_value_usd = sum(trade_value_usd, na.rm = TRUE),
    year_count = data.table::uniqueN(year),
    latest_year = max(as.integer(year), na.rm = TRUE),
    earliest_year = min(as.integer(year), na.rm = TRUE),
    reporter_name = if ("reporter_name" %in% names(dt)) reporter_name[1] else reporter_iso3[1],
    partner_name = if ("partner_name" %in% names(dt)) partner_name[1] else partner_iso3[1],
    commodity_description = if ("commodity_description" %in% names(dt)) {
      commodity_description[1]
    } else {
      hs_code[1]
    },
    reporter_code = if ("reporter_code" %in% names(dt)) as.character(reporter_code[1]) else NA_character_,
    partner_code = if ("partner_code" %in% names(dt)) as.character(partner_code[1]) else NA_character_
  ), by = .(reporter_iso3, partner_iso3, hs_code, flow_code)]

  ann <- ann[annual_value_usd >= as.numeric(min_annual_value_usd)]
  if (!nrow(ann)) return(empty)

  max_year <- max(ann$latest_year, na.rm = TRUE)
  ann[, `:=`(
    series_id = make_series_id(reporter_iso3, partner_iso3, hs_code, flow_code),
    recent_activity = as.integer(latest_year >= (max_year - 1L)),
    continuity_score = year_count / max(1, (max_year - min(earliest_year, na.rm = TRUE) + 1L)),
    candidate_version = FORECAST_CANDIDATE_VERSION,
    universe_version = coverage$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM,
    production_status = coverage$production_status %||% "unknown"
  )]

  ann[, pre_forecast_score := sanitize_chart_numeric(
    log1p(annual_value_usd) * (1 + 0.25 * recent_activity) * (0.5 + 0.5 * continuity_score)
  )]
  data.table::setorderv(
    ann,
    c("pre_forecast_score", "annual_value_usd", "series_id"),
    c(-1L, -1L, 1L)
  )
  ann[, candidate_rank := seq_len(.N)]
  utils::head(ann, as.integer(top_n))
}
