make_shock_detailed_fixture <- function() {
  data.table::data.table(
    year = c(2024L, 2024L, 2024L, 2024L, 2024L, 2024L, 2024L, 2023L),
    reporter_iso3 = c("DEU", "DEU", "DEU", "IND", "IND", "KOR", "DEU", "DEU"),
    reporter_name = c("Germany", "Germany", "Germany", "India", "India", "Korea", "Germany", "Germany"),
    partner_iso3 = c("CHN", "USA", "JPN", "CHN", "USA", "CHN", "DEU", "WLD"),
    partner_name = c("China", "USA", "Japan", "China", "USA", "China", "Germany", "World"),
    flow_code = c("M", "M", "M", "M", "M", "M", "M", "M"),
    flow_name = "Import",
    hs_code = c("8542", "8542", "8542", "8542", "8542", "8542", "8542", "8542"),
    commodity_description = "ICs",
    trade_value_usd = c(70, 20, 10, 80, 20, 50, 5, 9999),
    reporter_gdp_current_usd = c(4e12, 4e12, 4e12, 3e12, 3e12, 1.6e12, 4e12, 4e12),
    ingested_at = "2024-01-01T00:00:00Z",
    universe_checksum = "uv_262deb46e00d2f216a5a",
    production_status = "partial",
    raw_file = "/secret/path.json",
    request_id = "req_secret"
  )
}

make_shock_propagation_fixture <- function() {
  data.table::data.table(
    year = 2024L,
    reporter_iso3 = c("DEU", "DEU", "IND", "IND"),
    reporter_name = c("Germany", "Germany", "India", "India"),
    partner_iso3 = c("CHN", "USA", "DEU", "USA"),
    partner_name = c("China", "USA", "Germany", "USA"),
    flow_code = "M",
    flow_name = "Import",
    hs_code = c("8542", "8542", "8542", "8517"),
    commodity_description = c("ICs", "ICs", "ICs", "Phones"),
    trade_value_usd = c(100, 50, 40, 30),
    reporter_gdp_current_usd = 4e12
  )
}

make_shock_coverage <- function(status = "partial") {
  list(
    production_status = status,
    selected_reporter_count = 20L,
    represented_reporter_count = 3L,
    missing_reporter_count = 17L,
    selected_reporters = character(),
    represented_reporters = c("DEU", "IND", "KOR"),
    missing_reporters = character(),
    universe_checksum = "uv_262deb46e00d2f216a5a",
    checksum_stale = FALSE
  )
}

make_base_scenario <- function(...) {
  sc <- list(
    scenario_name = "test scenario",
    baseline_year_start = 2024L,
    baseline_year_end = 2024L,
    target_supplier_iso3 = "CHN",
    target_hs_codes = "8542",
    shock_size_pct = 30,
    substitution_mode = "capacity_constrained",
    substitution_capacity_pct = 25,
    maximum_substitute_supplier_share = 1,
    propagation_mode = "direct_only",
    acknowledge_partial_coverage = TRUE,
    universe_version = "uv_262deb46e00d2f216a5a",
    engine_version = SHOCK_ENGINE_VERSION
  )
  dots <- list(...)
  for (nm in names(dots)) sc[[nm]] <- dots[[nm]]
  sc
}
