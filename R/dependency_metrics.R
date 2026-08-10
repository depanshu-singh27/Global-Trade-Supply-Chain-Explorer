supplier_concentration_by_group <- function(shares) {
  dt <- data.table::as.data.table(shares)
  if (!nrow(dt)) {
    return(data.table::data.table(
      reporter_iso3 = character(), reporter_name = character(),
      hs_code = character(), commodity_description = character(),
      reporter_commodity_total = numeric(),
      supplier_count = integer(),
      top_1_share = numeric(), top_3_share = numeric(),
      supplier_hhi = numeric(), effective_supplier_count = numeric(),
      top_partner_iso3 = character(), top_partner_name = character(),
      year_start = integer(), year_end = integer(),
      reporter_gdp_current_usd = numeric(),
      imports_pct_gdp = numeric(),
      hhi_band = character(), top1_band = character()
    ))
  }
  out <- dt[, {
    sh <- partner_share[is.finite(partner_share) & partner_share > 0]
    sh <- sort(sh, decreasing = TRUE)
    hhi <- if (length(sh)) sum(sh^2) else NA_real_
    top1 <- if (length(sh)) sh[1] else NA_real_
    top3 <- if (length(sh)) sum(sh[seq_len(min(3L, length(sh)))]) else NA_real_
    eff <- if (is.finite(hhi) && hhi > 0) 1 / hhi else NA_real_
    .(
      reporter_name = reporter_name[1],
      commodity_description = commodity_description[1],
      reporter_commodity_total = reporter_commodity_total[1],
      supplier_count = as.integer(length(sh)),
      top_1_share = sanitize_chart_numeric(top1),
      top_3_share = sanitize_chart_numeric(top3),
      supplier_hhi = sanitize_chart_numeric(hhi),
      effective_supplier_count = sanitize_chart_numeric(eff),
      top_partner_iso3 = partner_iso3[which.max(partner_share)][1],
      top_partner_name = partner_name[which.max(partner_share)][1],
      year_start = min(year_start, na.rm = TRUE),
      year_end = max(year_end, na.rm = TRUE),
      reporter_gdp_current_usd = {
        g <- reporter_gdp_current_usd
        g <- g[is.finite(g)]
        if (length(g)) stats::median(g) else NA_real_
      }
    )
  }, by = .(reporter_iso3, hs_code)]

  out[, imports_pct_gdp := {
    ifelse(
      is.finite(reporter_gdp_current_usd) & reporter_gdp_current_usd > 0,
      100 * reporter_commodity_total / reporter_gdp_current_usd,
      NA_real_
    )
  }]
  out[, hhi_band := vapply(supplier_hhi, classify_hhi_band, character(1))]
  out[, top1_band := vapply(top_1_share, classify_share_band, character(1))]
  out[, concentrated_import_value := sanitize_chart_numeric(
    reporter_commodity_total * top_1_share
  )]
  data.table::setorderv(out, c("reporter_iso3", "hs_code"))
  out
}

add_commodity_importance <- function(group_conc) {
  dt <- data.table::as.data.table(group_conc)
  if (!nrow(dt)) return(dt)
  if ("reporter_total_imports" %in% names(dt)) dt[, reporter_total_imports := NULL]
  if ("commodity_import_share" %in% names(dt)) dt[, commodity_import_share := NULL]
  rep_tot <- dt[, .(reporter_total_imports = sum(reporter_commodity_total, na.rm = TRUE)),
                by = reporter_iso3]
  dt <- merge(dt, rep_tot, by = "reporter_iso3", all.x = TRUE)
  dt[, commodity_import_share := data.table::fifelse(
    is.finite(reporter_total_imports) & reporter_total_imports > 0,
    reporter_commodity_total / reporter_total_imports,
    NA_real_
  )]
  dt[, commodity_import_share := sanitize_chart_numeric(commodity_import_share)]
  dt
}

reporter_weighted_concentration <- function(group_conc) {
  dt <- add_commodity_importance(group_conc)
  if (!nrow(dt)) {
    return(data.table::data.table(
      reporter_iso3 = character(), reporter_name = character(),
      reporter_total_imports = numeric(),
      commodity_count = integer(),
      weighted_top_1_share = numeric(),
      weighted_top_3_share = numeric(),
      weighted_hhi = numeric(),
      effective_supplier_count = numeric(),
      supplier_partners = integer(),
      most_concentrated_hs = character(),
      largest_hs = character(),
      largest_supplier_iso3 = character()
    ))
  }
  out <- dt[, {
    w <- commodity_import_share
    w[!is.finite(w)] <- 0
    .(
      reporter_name = reporter_name[1],
      reporter_total_imports = reporter_total_imports[1],
      commodity_count = .N,
      weighted_top_1_share = sanitize_chart_numeric(sum(w * top_1_share, na.rm = TRUE)),
      weighted_top_3_share = sanitize_chart_numeric(sum(w * top_3_share, na.rm = TRUE)),
      weighted_hhi = sanitize_chart_numeric(sum(w * supplier_hhi, na.rm = TRUE)),
      most_concentrated_hs = hs_code[which.max(supplier_hhi)][1],
      largest_hs = hs_code[which.max(reporter_commodity_total)][1],
      largest_supplier_iso3 = top_partner_iso3[which.max(reporter_commodity_total)][1]
    )
  }, by = reporter_iso3]
  out[, effective_supplier_count := ifelse(
    is.finite(weighted_hhi) & weighted_hhi > 0,
    sanitize_chart_numeric(1 / weighted_hhi),
    NA_real_
  )]
  out[, hhi_band := vapply(weighted_hhi, classify_hhi_band, character(1))]
  out
}

commodity_concentration_summary <- function(group_conc) {
  dt <- data.table::as.data.table(group_conc)
  if (!nrow(dt)) return(data.table::data.table())
  dt[, .(
    commodity_description = commodity_description[1],
    total_import_value = sum(reporter_commodity_total, na.rm = TRUE),
    reporter_count = data.table::uniqueN(reporter_iso3),
    median_top_1_share = stats::median(top_1_share, na.rm = TRUE),
    median_hhi = stats::median(supplier_hhi, na.rm = TRUE),
    max_hhi = max(supplier_hhi, na.rm = TRUE),
    max_hhi_reporter = reporter_iso3[which.max(supplier_hhi)][1]
  ), by = hs_code]
}

dependency_trend_by_year <- function(detailed,
                                       reporters = NULL,
                                       hs_codes = NULL,
                                       year_min = 2019L,
                                       year_max = 2024L) {
  year_min <- as.integer(year_min)
  year_max <- as.integer(year_max)
  years <- seq.int(year_min, year_max)

  empty_result <- function() {
    data.table::data.table(
      year = years,
      weighted_hhi = NA_real_,
      weighted_top_1_share = NA_real_,
      supplier_count = NA_integer_,
      effective_supplier_count = NA_real_,
      import_value = NA_real_
    )
  }

  raw <- prepare_detailed_trade(detailed)

  if (is.null(raw) || !nrow(raw)) {
    return(empty_result())
  }

  dt <- raw[
    flow_code == "M" &
      year >= year_min &
      year <= year_max &
      !is.na(reporter_iso3) &
      nzchar(reporter_iso3) &
      !is.na(partner_iso3) &
      nzchar(partner_iso3) &
      reporter_iso3 != partner_iso3 &
      !is.na(hs_code) &
      nzchar(hs_code) &
      is.finite(trade_value_usd) &
      trade_value_usd > 0,
    .(
      year,
      reporter_iso3,
      partner_iso3,
      hs_code,
      trade_value_usd
    )
  ]

  if (!is.null(reporters) &&
      length(reporters) &&
      !all(reporters %in% c("__ALL__", ""))) {
    reps <- setdiff(as.character(reporters), c("__ALL__", ""))
    if (length(reps)) {
      dt <- dt[reporter_iso3 %in% reps]
    }
  }

  if (!is.null(hs_codes) &&
      length(hs_codes) &&
      !all(hs_codes %in% c("__ALL__", ""))) {
    hs <- setdiff(as.character(hs_codes), c("__ALL__", ""))
    if (length(hs)) {
      dt <- dt[hs_code %in% hs]
    }
  }

  if (!nrow(dt)) {
    return(empty_result())
  }

  links <- dt[
    ,
    .(
      partner_import_value = sum(trade_value_usd, na.rm = TRUE)
    ),
    by = .(
      year,
      reporter_iso3,
      partner_iso3,
      hs_code
    )
  ]

  links <- links[
    is.finite(partner_import_value) &
      partner_import_value > 0
  ]

  if (!nrow(links)) {
    return(empty_result())
  }

  links[
    ,
    reporter_commodity_total :=
      sum(partner_import_value, na.rm = TRUE),
    by = .(
      year,
      reporter_iso3,
      hs_code
    )
  ]

  links[
    ,
    partner_share :=
      partner_import_value / reporter_commodity_total
  ]

  groups <- links[
    is.finite(partner_share) &
      partner_share > 0,
    .(
      reporter_commodity_total = reporter_commodity_total[1L],
      supplier_count = .N,
      top_1_share = max(partner_share, na.rm = TRUE),
      supplier_hhi = sum(partner_share^2, na.rm = TRUE)
    ),
    by = .(
      year,
      reporter_iso3,
      hs_code
    )
  ]

  groups[
    ,
    reporter_total_imports :=
      sum(reporter_commodity_total, na.rm = TRUE),
    by = .(
      year,
      reporter_iso3
    )
  ]

  groups[
    ,
    commodity_import_share :=
      data.table::fifelse(
        is.finite(reporter_total_imports) &
          reporter_total_imports > 0,
        reporter_commodity_total / reporter_total_imports,
        NA_real_
      )
  ]

  reporter_year <- groups[
    ,
    .(
      weighted_hhi = sum(
        commodity_import_share * supplier_hhi,
        na.rm = TRUE
      ),
      weighted_top_1_share = sum(
        commodity_import_share * top_1_share,
        na.rm = TRUE
      )
    ),
    by = .(
      year,
      reporter_iso3
    )
  ]

  reporter_year[
    ,
    effective_supplier_count :=
      data.table::fifelse(
        is.finite(weighted_hhi) &
          weighted_hhi > 0,
        1 / weighted_hhi,
        NA_real_
      )
  ]

  annual_reporter <- reporter_year[
    ,
    .(
      weighted_hhi = mean(weighted_hhi, na.rm = TRUE),
      weighted_top_1_share = mean(
        weighted_top_1_share,
        na.rm = TRUE
      ),
      effective_supplier_count = mean(
        effective_supplier_count,
        na.rm = TRUE
      )
    ),
    by = year
  ]

  annual_groups <- groups[
    ,
    .(
      supplier_count = as.integer(
        round(mean(supplier_count, na.rm = TRUE))
      ),
      import_value = sum(
        reporter_commodity_total,
        na.rm = TRUE
      )
    ),
    by = year
  ]

  out <- merge(
    data.table::data.table(year = years),
    annual_reporter,
    by = "year",
    all.x = TRUE,
    sort = FALSE
  )

  out <- merge(
    out,
    annual_groups,
    by = "year",
    all.x = TRUE,
    sort = FALSE
  )

  data.table::setorderv(out, "year")

  out[
    ,
    .(
      year,
      weighted_hhi,
      weighted_top_1_share,
      supplier_count,
      effective_supplier_count,
      import_value
    )
  ]
}
