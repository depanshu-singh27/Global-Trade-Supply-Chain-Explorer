make_ts_global_fixture <- function() {
  data.table::data.table(
    reporter_iso3 = rep(c("DEU", "USA", "CHN", "IND", "EUR"), each = 4),
    reporter_name = rep(c("Germany", "USA", "China", "India", "Euro Area"), each = 4),
    year = rep(2019:2022, 5),
    imports_value_usd = c(100, 110, 120, 130, 200, 210, 190, 180, 150, 160, 170, 180, 80, 90, 100, 0, 900, 910, 920, 930),
    exports_value_usd = c(120, 125, 140, 150, 180, 175, 170, 160, 200, 210, 220, 230, 50, 55, 60, 70, 800, 810, 820, 830),
    total_trade_value_usd = c(220, 235, 260, 280, 380, 385, 360, 340, 350, 370, 390, 410, 130, 145, 160, 70, 1700, 1720, 1740, 1760),
    trade_balance_usd = c(20, 15, 20, 20, -20, -35, -20, -20, 50, 50, 50, 50, -30, -35, -40, 70, -100, -100, -100, -100),
    gdp_current_usd = c(4e12, 4.1e12, 4.2e12, 4.3e12, 20e12, 21e12, 22e12, 23e12, 14e12, 15e12, NA_real_, 16e12, 3e12, 3.1e12, 3.2e12, 3.3e12, NA, NA, NA, NA),
    population_total = c(83e6, 83e6, 84e6, 84e6, 330e6, 331e6, 332e6, 333e6, 1.4e9, 1.4e9, 1.41e9, 1.41e9, 1.3e9, 1.3e9, 0, 1.4e9, NA, NA, NA, NA),
    total_trade_pct_gdp = c(5, 5.5, 6, 6.5, 1.9, 1.8, 1.6, 1.5, 2.5, 2.4, NA, 2.5, 4, 4.5, 5, 2, NA, NA, NA, NA),
    trade_balance_pct_gdp = c(0.5, 0.4, 0.5, 0.5, -0.1, -0.2, -0.1, -0.1, 0.3, 0.3, NA, 0.3, -1, -1.1, -1.2, 2, NA, NA, NA, NA),
    total_trade_per_capita_usd = c(2650, 2831, 3095, 3333, 1151, 1163, 1084, 1021, 250, 264, 276, 290, 100, 111, NA, 50, NA, NA, NA, NA)
  )
}

make_ts_detailed_fixture <- function() {
  data.table::data.table(
    year = c(2019L, 2020L, 2021L, 2022L, 2019L, 2022L, 2019L, 2022L, 2020L),
    reporter_iso3 = c("DEU", "DEU", "DEU", "DEU", "DEU", "DEU", "IND", "IND", "DEU"),
    reporter_name = c("Germany", "Germany", "Germany", "Germany", "Germany", "Germany", "India", "India", "Germany"),
    partner_iso3 = c("CHN", "CHN", "CHN", "CHN", "USA", "USA", "CHN", "CHN", "WLD"),
    partner_name = c("China", "China", "China", "China", "USA", "USA", "China", "China", "World"),
    flow_code = c("M", "M", "X", "X", "M", "M", "X", "X", "M"),
    flow_name = c("Import", "Import", "Export", "Export", "Import", "Import", "Export", "Export", "Import"),
    hs_code = c("8542", "8542", "8542", "8542", "8517", "8517", "8507", "8507", "8542"),
    commodity_description = c("ICs", "ICs", "ICs", "ICs", "Phones", "Phones", "Batteries", "Batteries", "ICs"),
    trade_value_usd = c(100, 120, 80, 90, 50, 70, 40, 60, 9999),
    ingested_at = "2024-01-01T00:00:00Z",
    universe_checksum = "uv_262deb46e00d2f216a5a",
    production_status = "partial",
    raw_file = "/secret/path.json"
  )
}
