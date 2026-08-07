make_dependency_fixture <- function() {
  data.table::data.table(
    year = c(2019L, 2019L, 2019L, 2020L, 2020L, 2019L, 2019L, 2019L, 2019L, 2019L, 2019L, 2020L),
    reporter_iso3 = c("DEU", "DEU", "DEU", "DEU", "DEU", "IND", "IND", "DEU", "DEU", "DEU", "KOR", "DEU"),
    reporter_name = c("Germany", "Germany", "Germany", "Germany", "Germany", "India", "India", "Germany", "Germany", "Germany", "Korea", "Germany"),
    partner_iso3 = c("CHN", "USA", "JPN", "CHN", "USA", "CHN", "USA", "DEU", "WLD", "EUR", "CHN", "CHN"),
    partner_name = c("China", "USA", "Japan", "China", "USA", "China", "USA", "Germany", "World", "Euro Area", "China", "China"),
    flow_code = c("M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "M", "X"),
    flow_name = c("Import", "Import", "Import", "Import", "Import", "Import", "Import", "Import", "Import", "Import", "Import", "Export"),
    hs_code = c("8542", "8542", "8542", "8542", "8542", "8542", "8517", "8542", "8542", "8542", "8542", "8542"),
    commodity_description = c("ICs", "ICs", "ICs", "ICs", "ICs", "ICs", "Phones", "ICs", "ICs", "ICs", "ICs", "ICs"),

    trade_value_usd = c(50, 25, 25, 60, 40, 80, 30, 15, 9999, 8888, 70, 500),
    reporter_gdp_current_usd = c(4e12, 4e12, 4e12, 4.1e12, 4.1e12, 3e12, 3e12, 4e12, 4e12, 4e12, 1.6e12, 4.1e12),
    reporter_population_total = 83e6,
    partner_gdp_current_usd = 10e12,
    partner_population_total = 1e9,
    ingested_at = "2024-01-01T00:00:00Z",
    universe_checksum = "uv_262deb46e00d2f216a5a",
    production_status = "partial",
    raw_file = "/secret/cache/path.json",
    request_id = "req_secret_dep"
  )
}

make_dependency_snap_fixture <- function(status = "partial") {
  det <- make_dependency_fixture()
  list(
    trade_detailed_enriched = prepare_detailed_trade(det),
    trade_detailed = det,
    analytical_universe = list(
      universe_checksum = "uv_262deb46e00d2f216a5a",
      top_reporters = data.table::data.table(
        reporter_iso3 = c("DEU", "IND", "ITA", "KOR", "SGP", "THA", "USA", "CHN")
      ),
      top_partners = data.table::data.table(partner_iso3 = c("CHN", "USA", "JPN", "DEU")),
      top_hs4 = data.table::data.table(hs_code = c("8542", "8517"))
    ),
    production_manifest = list(production_status = status),
    detailed_coverage = list(
      production_status = status,
      selected_reporter_count = 8L,
      represented_reporter_count = 3L,
      missing_reporter_count = 5L,
      selected_reporters = c("DEU", "IND", "ITA", "KOR", "SGP", "THA", "USA", "CHN"),
      represented_reporters = c("DEU", "IND", "KOR"),
      missing_reporters = c("ITA", "SGP", "THA", "USA", "CHN"),
      universe_checksum = "uv_262deb46e00d2f216a5a",
      checksum_stale = FALSE,
      validation_warnings = 0L,
      latest_ingested_at = "2024-01-01T00:00:00Z",
      n_rows = nrow(det)
    ),
    pipeline_status = list(detailed_trade = status, global_trade = "complete")
  )
}
