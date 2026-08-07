make_map_analytics_fixture <- function() {
  data.table::data.table(
    reporter_code = c("276", "276", "842", "842", "156", "156", "356", "356", "999", "999"),
    reporter_iso3 = c("DEU", "DEU", "USA", "USA", "CHN", "CHN", "IND", "IND", "EUR", "EUR"),
    reporter_name = c("Germany", "Germany", "USA", "USA", "China", "China", "India", "India",
                      "Euro Area", "Euro Area"),
    year = c(2023L, 2024L, 2023L, 2024L, 2023L, 2024L, 2023L, 2024L, 2023L, 2024L),
    imports_value_usd = c(100, 110, 200, 210, 150, 160, 80, 90, 1000, 1100),
    exports_value_usd = c(120, 130, 180, 170, 200, 220, 50, 40, 900, 950),
    total_trade_value_usd = c(220, 240, 380, 380, 350, 380, 130, 130, 1900, 2050),
    trade_balance_usd = c(20, 20, -20, -40, 50, 60, -30, -50, -100, -150),
    gdp_current_usd = c(4e12, 4.2e12, 25e12, 26e12, 17e12, NA_real_, 3e12, 3.2e12, NA_real_, NA_real_),
    population_total = c(83e6, 84e6, 330e6, 331e6, 1.4e9, 1.41e9, 1.4e9, 0, NA_real_, NA_real_),
    inflation_annual_pct = c(5, 4, 3, 2.5, 2, NA_real_, 6, 5, NA_real_, NA_real_),
    gdp_per_capita_usd = c(48000, 50000, 75000, 78000, 12000, NA_real_, 2100, NA_real_, NA_real_, NA_real_),
    total_trade_pct_gdp = c(5.5, 5.7, 1.5, 1.5, 2.0, NA_real_, 4.3, 4.0, NA_real_, NA_real_),
    trade_balance_pct_gdp = c(0.5, 0.5, -0.1, -0.2, 0.3, NA_real_, -1, -1.5, NA_real_, NA_real_),
    total_trade_per_capita_usd = c(2650, 2857, 1151, 1148, 250, 269, 93, NA_real_, NA_real_, NA_real_),
    total_trade_yoy_pct = c(NA_real_, 9.1, NA_real_, 0, NA_real_, 8.6, NA_real_, 0, NA_real_, NA_real_),
    latest_ingested_at = "2024-01-02T00:00:00Z"
  )
}
