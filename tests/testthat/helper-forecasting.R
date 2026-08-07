make_monthly_fixture_series <- function(series_id = "DEU__CHN__8542__M",
                                          start = as.Date("2019-01-01"),
                                          n = 72L,
                                          base = 1e7,
                                          season_amp = 0.15,
                                          noise_sd = 0.05,
                                          missing_months = integer(),
                                          zero_months = integer(),
                                          seed = 42L) {
  set.seed(seed)
  parts <- parse_series_id(series_id)
  dates <- seq.Date(start, by = "month", length.out = n)
  t <- seq_len(n)
  seasonal <- 1 + season_amp * sin(2 * pi * t / 12)
  trend <- 1 + 0.002 * t
  vals <- base * seasonal * trend * exp(stats::rnorm(n, 0, noise_sd))
  vals[missing_months] <- NA_real_
  vals[zero_months] <- 0
  data.table::data.table(
    series_id = series_id,
    period = format(dates, "%Y%m"),
    year = as.integer(format(dates, "%Y")),
    month = as.integer(format(dates, "%m")),
    date = dates,
    frequency = "M",
    reporter_iso3 = parts$reporter_iso3,
    partner_iso3 = parts$partner_iso3,
    hs_code = parts$hs_code,
    flow_code = parts$flow_code,
    reporter_name = parts$reporter_iso3,
    partner_name = parts$partner_iso3,
    commodity_description = parts$hs_code,
    reporter_code = "276",
    partner_code = "156",
    flow_name = if (parts$flow_code == "M") "Import" else "Export",
    hs_revision = "HS",
    trade_value_usd = vals,
    value_observed = !is.na(vals),
    value_missing = is.na(vals),
    value_zero = !is.na(vals) & vals == 0,
    imputation_status = "none",
    source_row_count = as.integer(!is.na(vals)),
    source_updated_at = NA_character_,
    ingested_at = utc_now(),
    request_id = paste0("fixture_", series_id),
    universe_version = "uv_262deb46e00d2f216a5a"
  )
}

make_forecast_fixture_bundle <- function(n_series = 12L) {
  specs <- list(
    list(id = "DEU__CHN__8542__M", base = 2e7, seed = 1L),
    list(id = "DEU__USA__8517__M", base = 1.5e7, seed = 2L),
    list(id = "IND__CHN__8504__M", base = 1.2e7, seed = 3L),
    list(id = "IND__USA__8542__X", base = 9e6, seed = 4L),
    list(id = "KOR__CHN__8541__M", base = 1.8e7, seed = 5L),
    list(id = "KOR__USA__8525__M", base = 8e6, seed = 6L),
    list(id = "SGP__CHN__8542__M", base = 2.2e7, seed = 7L),
    list(id = "SGP__MYS__8536__M", base = 7e6, seed = 8L),
    list(id = "THA__CHN__8501__M", base = 6e6, seed = 9L),
    list(id = "THA__JPN__8507__M", base = 5e6, seed = 10L),
    list(id = "ITA__CHN__8542__M", base = 1.1e7, seed = 11L, missing = 5:8),
    list(id = "ITA__USA__8517__X", base = 4e6, seed = 12L, missing = c(20:30))
  )
  specs <- specs[seq_len(min(n_series, length(specs)))]
  long <- data.table::rbindlist(lapply(specs, function(s) {
    make_monthly_fixture_series(
      series_id = s$id,
      base = s$base,
      seed = s$seed,
      missing_months = s$missing %||% integer()
    )
  }), fill = TRUE)
  cand <- unique(long[, .(
    series_id, reporter_iso3, partner_iso3, hs_code, flow_code,
    reporter_name, partner_name, commodity_description,
    reporter_code, partner_code
  )])
  cand[, `:=`(
    annual_value_usd = 1e8,
    year_count = 6L,
    latest_year = 2024L,
    earliest_year = 2019L,
    recent_activity = 1L,
    continuity_score = 1,
    candidate_version = FORECAST_CANDIDATE_VERSION,
    universe_version = "uv_262deb46e00d2f216a5a",
    production_status = "fixture",
    pre_forecast_score = seq_len(.N) * 1.0,
    candidate_rank = seq_len(.N)
  )]
  long[, `:=`(
    data_mode = FORECAST_DATA_MODE_FIXTURE,
    data_source = FORECAST_DATA_SOURCE_FIXTURE
  )]
  list(monthly_long = long, candidates = cand)
}
