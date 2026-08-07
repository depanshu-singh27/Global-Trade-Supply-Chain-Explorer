make_trade_flow_fixture <- function() {
  data.table::data.table(
    year = c(2022L, 2022L, 2023L, 2023L, 2023L, 2023L, 2024L, 2024L, 2023L, 2023L),
    reporter_iso3 = c("DEU", "DEU", "DEU", "DEU", "IND", "IND", "DEU", "KOR", "ITA", "ITA"),
    reporter_name = c("Germany", "Germany", "Germany", "Germany", "India", "India",
                      "Germany", "Rep. of Korea", "Italy", "Italy"),
    partner_iso3 = c("CHN", "USA", "CHN", "USA", "CHN", "USA", "CHN", "CHN", "USA", "WLD"),
    partner_name = c("China", "USA", "China", "USA", "China", "USA", "China", "China", "USA", "World"),
    flow_code = c("M", "X", "M", "X", "M", "X", "M", "X", "M", "M"),
    flow_name = c("Import", "Export", "Import", "Export", "Import", "Export", "Import", "Export", "Import", "Import"),
    hs_code = c("8542", "8542", "8517", "8542", "8542", "8507", "8542", "8517", "8542", "8542"),
    commodity_description = c(
      "Electronic integrated circuits", "Electronic integrated circuits",
      "Telephone sets", "Electronic integrated circuits",
      "Electronic integrated circuits", "Electric accumulators",
      "Electronic integrated circuits", "Telephone sets",
      "Electronic integrated circuits", "Electronic integrated circuits"
    ),
    trade_value_usd = c(100, 80, 120, 90, 50, 40, 130, 70, 55, 9999),
    net_weight_kg = as.numeric(1:10),
    quantity = as.numeric(10 * 1:10),
    reporter_gdp_current_usd = c(4e12, 4e12, 4.1e12, 4.1e12, 3e12, 3e12, NA_real_, 1.8e12, 2e12, 2e12),
    partner_gdp_current_usd = c(14e12, 23e12, 14e12, 23e12, 14e12, 23e12, 14e12, 14e12, 23e12, NA_real_),
    ingested_at = "2024-01-01T00:00:00Z",
    request_id = "req_test",
    universe_checksum = "uv_262deb46e00d2f216a5a",
    production_status = "partial",
    raw_file = "/secret/path/should_drop.json"
  )
}

make_trade_flow_universe <- function() {
  list(
    top_reporters = data.frame(
      reporter_iso3 = c("DEU", "IND", "ITA", "KOR", "CHN", "USA"),
      reporter_name = c("Germany", "India", "Italy", "Korea", "China", "USA"),
      stringsAsFactors = FALSE
    ),
    top_partners = data.frame(
      partner_iso3 = c("CHN", "USA"),
      partner_name = c("China", "USA"),
      stringsAsFactors = FALSE
    ),
    top_hs4 = data.frame(
      hs_code = c("8542", "8517", "8507"),
      commodity_description = c("ICs", "Phones", "Batteries"),
      stringsAsFactors = FALSE
    ),
    universe_checksum = "uv_262deb46e00d2f216a5a",
    universe_version = "uv_262deb46e00d2f216a5a"
  )
}

make_trade_flow_snap <- function(complete = FALSE) {
  det <- make_trade_flow_fixture()
  uni <- make_trade_flow_universe()
  if (isTRUE(complete)) {
    uni$top_reporters <- data.frame(
      reporter_iso3 = c("DEU", "IND", "ITA", "KOR"),
      reporter_name = c("Germany", "India", "Italy", "Korea"),
      stringsAsFactors = FALSE
    )
  }
  prepared <- prepare_detailed_trade(det)
  list(
    trade_detailed_enriched = prepared,
    trade_detailed = prepared,
    analytical_universe = uni,
    production_manifest = list(
      production_status = if (isTRUE(complete)) "complete" else "partial",
      selected_reporter_count = nrow(uni$top_reporters),
      represented_reporter_count = data.table::uniqueN(prepared$reporter_iso3),
      universe_version = uni$universe_checksum,
      planned_request_count = 120L,
      active_request_count = 120L,
      succeeded_request_count = if (isTRUE(complete)) 120L else 36L,
      quota_blocked_count = if (isTRUE(complete)) 0L else 10L
    ),
    production_validation = data.table::data.table(status = c("pass", "warning")),
    phase3_validation = data.table::data.table(status = "pass"),
    pipeline_status = list(detailed_trade = if (isTRUE(complete)) "complete" else "partial")
  )
}
