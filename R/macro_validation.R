run_phase3_validation <- function(cfg,
                                    universe,
                                    wdi_long,
                                    wdi_wide,
                                    trade_global,
                                    trade_global_enriched,
                                    trade_detailed,
                                    trade_detailed_enriched,
                                    country_year,
                                    production_status = "partial",
                                    universe_checksum = NA_character_) {
  results <- list()
  add <- function(row) results[[length(results) + 1L]] <<- row

  add(validate_required_columns(
    universe,
    c("iso3", "entity_type", "included", "exclusion_reason"),
    "macro_country_universe"
  ))
  included <- universe[included == TRUE]
  add(.validation_row(
    "universe_no_aggregates", "macro_country_universe",
    if (any(included$entity_type %in% c("aggregate", "group", "special"), na.rm = TRUE) ||
        any(included$iso3 %in% c("EUR", "WLD", "W00", "ASE"), na.rm = TRUE)) "error" else "pass",
    "No aggregate/group/special entities included",
    sum(included$entity_type %in% c("aggregate", "group", "special") |
          included$iso3 %in% c("EUR", "WLD", "W00", "ASE"), na.rm = TRUE)
  ))
  add(validate_iso3(included$iso3, "macro_country_universe", "iso3"))
  add(.validation_row(
    "universe_unique_iso3", "macro_country_universe",
    if (anyDuplicated(included$iso3)) "error" else "pass",
    "Unique included ISO3",
    sum(duplicated(included$iso3))
  ))

  top_rep_path <- file.path(cfg[['paths']]$processed, "top_reporters.parquet")
  if (file.exists(top_rep_path)) {
    top_rep <- data.table::as.data.table(arrow::read_parquet(top_rep_path))
    missing_top <- setdiff(as.character(top_rep$reporter_iso3), as.character(included$iso3))
    add(.validation_row(
      "universe_top_reporters", "macro_country_universe",
      if (length(missing_top)) "error" else "pass",
      sprintf("Missing top reporters in universe: %s", paste(missing_top, collapse = ",")),
      length(missing_top)
    ))
  }

  expected_inds <- vapply(cfg$wdi$indicators, function(x) x$code, character(1))
  add(validate_required_columns(
    wdi_long,
    c("iso3", "year", "indicator_code", "value", "request_id"),
    "wdi_production_long"
  ))
  add(validate_year_range(wdi_long$year, 2019, 2024, "wdi_production_long"))
  add(validate_wdi_indicators(wdi_long$indicator_code, expected_inds, "wdi_production_long"))
  add(validate_unique_keys(wdi_long, c("iso3", "year", "indicator_code"),
                           "wdi_production_long", "wdi_long_unique"))
  add(.validation_row(
    "wdi_long_no_aggregates", "wdi_production_long",
    if (any(wdi_long$iso3 %in% c("EUR", "WLD", "W00", "ASE"), na.rm = TRUE)) "error" else "pass",
    "No aggregate ISO3 in WDI long",
    sum(wdi_long$iso3 %in% c("EUR", "WLD", "W00", "ASE"), na.rm = TRUE)
  ))
  miss_val <- sum(is.na(wdi_long$value))
  add(.validation_row(
    "wdi_long_missing_values", "wdi_production_long",
    if (miss_val > 0) "warning" else "pass",
    sprintf("Missing WDI values (preserved as NA)=%d", miss_val),
    miss_val
  ))

  add(validate_unique_keys(wdi_wide, c("iso3", "year"), "wdi_production_wide", "wdi_wide_unique"))
  bad_pop <- sum(!is.na(wdi_wide$population_total) & wdi_wide$population_total <= 0)
  add(.validation_row(
    "wdi_wide_population", "wdi_production_wide",
    if (bad_pop) "error" else "pass",
    "Population non-positive when present",
    bad_pop
  ))
  inf_gpc <- sum(is.infinite(wdi_wide$gdp_per_capita_usd), na.rm = TRUE)
  add(.validation_row(
    "wdi_wide_gdp_per_capita", "wdi_production_wide",
    if (inf_gpc) "error" else "pass",
    "No infinite GDP per capita",
    inf_gpc
  ))

  add(.validation_row(
    "global_enrich_row_preserve", "trade_global_enriched",
    if (nrow(trade_global_enriched) == nrow(trade_global)) "pass" else "error",
    sprintf("before=%d after=%d", nrow(trade_global), nrow(trade_global_enriched)),
    abs(nrow(trade_global_enriched) - nrow(trade_global))
  ))
  inf_derived <- sum(is.infinite(trade_global_enriched$trade_value_pct_gdp), na.rm = TRUE) +
    sum(is.infinite(trade_global_enriched$trade_value_per_capita_usd), na.rm = TRUE)
  add(.validation_row(
    "global_enrich_no_infinite", "trade_global_enriched",
    if (inf_derived) "error" else "pass",
    "No infinite derived global enrich values",
    inf_derived
  ))

  add(.validation_row(
    "detailed_enrich_row_preserve", "trade_detailed_enriched",
    if (nrow(trade_detailed_enriched) == nrow(trade_detailed)) "pass" else "error",
    sprintf("before=%d after=%d", nrow(trade_detailed), nrow(trade_detailed_enriched)),
    abs(nrow(trade_detailed_enriched) - nrow(trade_detailed))
  ))
  add(.validation_row(
    "detailed_status_partial", "trade_detailed_enriched",
    {
      st <- unique(as.character(trade_detailed_enriched$production_status))
      if (identical(production_status, "partial") && all(st == "partial" | is.na(st))) "pass"
      else if (any(st == "complete", na.rm = TRUE) && identical(production_status, "partial")) "error"
      else "pass"
    },
    sprintf("production_status=%s preserved", production_status),
    0L
  ))
  if (!is.na(universe_checksum) && nzchar(universe_checksum) &&
      "universe_checksum" %in% names(trade_detailed_enriched)) {
    mismatch <- !all(trade_detailed_enriched$universe_checksum == universe_checksum, na.rm = TRUE)
    add(.validation_row(
      "detailed_universe_checksum", "trade_detailed_enriched",
      if (mismatch) "error" else "pass",
      sprintf("checksum=%s", universe_checksum),
      as.integer(mismatch)
    ))
  }

  add(validate_unique_keys(country_year, c("reporter_iso3", "year"),
                           "country_year_analytics", "country_year_unique"))
  if (nrow(country_year)) {
    bal_ok <- all(abs(
      ifelse(is.na(country_year$exports_value_usd), 0, country_year$exports_value_usd) -
        ifelse(is.na(country_year$imports_value_usd), 0, country_year$imports_value_usd) -
        ifelse(is.na(country_year$trade_balance_usd), 0, country_year$trade_balance_usd)
    ) < 1e-4 | (is.na(country_year$trade_balance_usd) &
                  is.na(country_year$exports_value_usd) & is.na(country_year$imports_value_usd)))
    add(.validation_row(
      "country_year_balance", "country_year_analytics",
      if (isTRUE(bal_ok)) "pass" else "error",
      "trade_balance = exports - imports",
      0L
    ))
    tot_ok <- all(abs(
      ifelse(is.na(country_year$exports_value_usd), 0, country_year$exports_value_usd) +
        ifelse(is.na(country_year$imports_value_usd), 0, country_year$imports_value_usd) -
        ifelse(is.na(country_year$total_trade_value_usd), 0, country_year$total_trade_value_usd)
    ) < 1e-4 | is.na(country_year$total_trade_value_usd))
    add(.validation_row(
      "country_year_total", "country_year_analytics",
      if (isTRUE(tot_ok)) "pass" else "error",
      "total_trade = exports + imports",
      0L
    ))
  }

  data.table::rbindlist(results, fill = TRUE)
}
