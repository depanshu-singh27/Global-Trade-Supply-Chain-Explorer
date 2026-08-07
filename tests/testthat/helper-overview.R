make_overview_fixture <- function() {
  data.table::data.table(
    reporter_code = c("276", "276", "356", "356", "380", "380", "999", "999"),
    reporter_iso3 = c("DEU", "DEU", "IND", "IND", "ITA", "ITA", "EUR", "EUR"),
    reporter_name = c("Germany", "Germany", "India", "India", "Italy", "Italy",
                      "Euro Area", "Euro Area"),
    year = c(2022L, 2023L, 2022L, 2023L, 2022L, 2023L, 2022L, 2023L),
    imports_value_usd = c(100, 110, 80, 90, 70, 60, 1000, 1100),
    exports_value_usd = c(120, 130, 50, 55, 90, 95, 900, 950),
    total_trade_value_usd = c(220, 240, 130, 145, 160, 155, 1900, 2050),
    trade_balance_usd = c(20, 20, -30, -35, 20, 35, -100, -150),
    gdp_current_usd = c(4e12, 4.2e12, 3e12, 3.2e12, 2e12, NA_real_, NA_real_, NA_real_),
    population_total = c(83e6, 84e6, 1.4e9, 1.41e9, 59e6, 59e6, NA_real_, NA_real_),
    cpi_index = c(100, 105, 110, 115, 102, 104, NA_real_, NA_real_),
    inflation_annual_pct = c(5, 4, 6, 5, 3, 2, NA_real_, NA_real_),
    gdp_per_capita_usd = c(48000, 50000, 2100, 2270, 34000, NA_real_, NA_real_, NA_real_),
    imports_pct_gdp = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    exports_pct_gdp = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    total_trade_pct_gdp = c(5.5, 5.7, 4.3, 4.5, 8, NA_real_, NA_real_, NA_real_),
    trade_balance_pct_gdp = c(0.5, 0.5, -1, -1.1, 1, NA_real_, NA_real_, NA_real_),
    imports_per_capita_usd = c(1200, 1310, 57, 64, 1186, 1017, NA_real_, NA_real_),
    exports_per_capita_usd = c(1446, 1548, 36, 39, 1525, 1610, NA_real_, NA_real_),
    total_trade_per_capita_usd = c(2651, 2857, 93, 103, 2712, 2627, NA_real_, NA_real_),
    trade_balance_per_capita_usd = c(241, 238, -21, -25, 339, 593, NA_real_, NA_real_),
    imports_yoy_pct = c(NA_real_, 10, NA_real_, 12.5, NA_real_, -14.3, NA_real_, NA_real_),
    exports_yoy_pct = c(NA_real_, 8.3, NA_real_, 10, NA_real_, 5.6, NA_real_, NA_real_),
    total_trade_yoy_pct = c(NA_real_, 9.1, NA_real_, 11.5, NA_real_, -3.1, NA_real_, NA_real_),
    trade_balance_change_usd = c(NA_real_, 0, NA_real_, -5, NA_real_, 15, NA_real_, NA_real_),
    latest_source_updated_at = "2024-01-01T00:00:00Z",
    latest_ingested_at = "2024-01-02T00:00:00Z"
  )
}

make_tie_fixture <- function() {
  data.table::data.table(
    reporter_iso3 = c("AAA", "BBB", "CCC"),
    reporter_name = c("Alpha", "Beta", "Charlie"),
    year = 2023L,
    imports_value_usd = c(50, 50, 40),
    exports_value_usd = c(50, 50, 60),
    total_trade_value_usd = c(100, 100, 100),
    trade_balance_usd = c(0, 0, 20),
    gdp_current_usd = c(1e9, 1e9, 1e9),
    population_total = c(1e6, 1e6, 1e6),
    total_trade_pct_gdp = c(10, 10, 10)
  )
}
