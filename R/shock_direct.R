build_shock_baseline <- function(detailed,
                                   year_min = NULL,
                                   year_max = NULL,
                                   reporters = NULL,
                                   partners = NULL,
                                   hs_codes = NULL,
                                   coverage = NULL,
                                   universe_version = EXPECTED_UNIVERSE_CHECKSUM) {
  built <- construct_dependency_table(
    detailed,
    year_min = year_min,
    year_max = year_max,
    reporters = reporters,
    partners = partners,
    hs_codes = hs_codes,
    exclude_self = TRUE
  )
  shares <- data.table::as.data.table(built$shares)
  if (!nrow(shares)) {
    return(list(
      baseline = data.table::data.table(),
      diagnostics = built$diagnostics,
      reconciliation = built$reconciliation,
      universe_version = universe_version,
      production_status = coverage$production_status %||% "absent",
      cache_key = shock_baseline_cache_key(
        year_min, year_max, universe_version, coverage$production_status %||% "absent"
      )
    ))
  }
  gc <- add_commodity_importance(supplier_concentration_by_group(shares))
  bl <- merge(
    shares,
    gc[, .(
      reporter_iso3, hs_code,
      supplier_count, supplier_hhi, top_1_share, top_3_share,
      commodity_import_share, reporter_total_imports
    )],
    by = c("reporter_iso3", "hs_code"),
    all.x = TRUE
  )
  data.table::setnames(
    bl,
    c("partner_iso3", "partner_name", "partner_import_value", "partner_share",
      "reporter_commodity_total"),
    c("supplier_iso3", "supplier_name", "baseline_import_value_usd", "supplier_share",
      "reporter_commodity_total_imports_usd"),
    skip_absent = TRUE
  )
  if (!"reporter_gdp_current_usd" %in% names(bl)) {
    bl[, reporter_gdp_current_usd := NA_real_]
  }
  bl[, `:=`(
    universe_version = as.character(universe_version),
    production_status = as.character(coverage$production_status %||% "partial"),
    source_observation_count = as.integer(observation_count %||% NA_integer_),
    baseline_year_start = as.integer(year_start),
    baseline_year_end = as.integer(year_end),
    reporter_total_imports_usd = as.numeric(reporter_total_imports %||% NA_real_)
  )]
  keep <- c(
    "reporter_iso3", "reporter_name", "supplier_iso3", "supplier_name",
    "hs_code", "commodity_description",
    "baseline_import_value_usd", "reporter_commodity_total_imports_usd",
    "supplier_share", "supplier_rank", "supplier_count", "supplier_hhi",
    "commodity_import_share", "reporter_total_imports_usd",
    "reporter_gdp_current_usd", "universe_version", "production_status",
    "source_observation_count", "baseline_year_start", "baseline_year_end"
  )
  keep <- intersect(keep, names(bl))
  baseline <- bl[, keep, with = FALSE]
  data.table::setorderv(
    baseline,
    c("reporter_iso3", "hs_code", "supplier_rank", "supplier_iso3")
  )
  list(
    baseline = baseline,
    diagnostics = built$diagnostics,
    reconciliation = built$reconciliation,
    universe_version = universe_version,
    production_status = coverage$production_status %||% "partial",
    cache_key = shock_baseline_cache_key(
      year_min, year_max, universe_version, coverage$production_status %||% "partial"
    )
  )
}

shock_baseline_cache_key <- function(year_min, year_max, universe_version, production_status) {
  paste(
    "shock_baseline",
    as.character(universe_version %||% "na"),
    as.character(year_min %||% "na"),
    as.character(year_max %||% "na"),
    as.character(production_status %||% "na"),
    sep = "_"
  )
}

select_shock_target_edges <- function(baseline, scenario) {
  sc <- normalize_shock_scenario(scenario)
  dt <- data.table::as.data.table(baseline)
  if (!nrow(dt)) return(dt[0])
  out <- dt[supplier_iso3 %in% sc$target_supplier_iso3]
  if (length(sc$target_hs_codes)) {
    out <- out[hs_code %in% sc$target_hs_codes]
  }
  if (length(sc$target_reporter_iso3)) {
    out <- out[reporter_iso3 %in% sc$target_reporter_iso3]
  }

  unique(out, by = c("reporter_iso3", "supplier_iso3", "hs_code"))
}

apply_direct_shock <- function(baseline, scenario) {
  sc <- normalize_shock_scenario(scenario)
  dt <- data.table::copy(data.table::as.data.table(baseline))
  if (!nrow(dt)) {
    return(list(
      edges = dt,
      targets = dt,
      shock_size = 0,
      targeted_baseline_value = 0,
      direct_disrupted_value = 0
    ))
  }
  shock_size <- sc$shock_size_pct / 100
  targets <- select_shock_target_edges(dt, sc)
  key <- c("reporter_iso3", "supplier_iso3", "hs_code")
  dt[, is_targeted := FALSE]
  if (nrow(targets)) {
    tg <- unique(targets[, key, with = FALSE])
    tg[, is_targeted := TRUE]
    dt[, is_targeted := NULL]
    dt <- merge(dt, tg, by = key, all.x = TRUE)
    dt[is.na(is_targeted), is_targeted := FALSE]
  }
  dt[, direct_disrupted_value_usd := data.table::fifelse(
    is_targeted,
    sanitize_chart_numeric(baseline_import_value_usd * shock_size),
    0
  )]
  dt[direct_disrupted_value_usd < 0, direct_disrupted_value_usd := 0]
  dt[, post_shock_supplier_value_usd := sanitize_chart_numeric(
    baseline_import_value_usd - direct_disrupted_value_usd
  )]
  dt[post_shock_supplier_value_usd < 0, post_shock_supplier_value_usd := 0]

  dt[, disrupted_check := sanitize_chart_numeric(
    baseline_import_value_usd - post_shock_supplier_value_usd
  )]
  list(
    edges = dt,
    targets = dt[is_targeted == TRUE],
    shock_size = shock_size,
    targeted_baseline_value = sum(dt[is_targeted == TRUE]$baseline_import_value_usd, na.rm = TRUE),
    direct_disrupted_value = sum(dt$direct_disrupted_value_usd, na.rm = TRUE)
  )
}
