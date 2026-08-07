aggregate_shock_reporter_impacts <- function(edges, scenario) {
  sc <- normalize_shock_scenario(scenario)
  dt <- data.table::as.data.table(edges)
  if (!nrow(dt)) return(data.table::data.table())
  out <- dt[, .(
    reporter_name = reporter_name[1],
    baseline_total_imports_usd = {
      v <- reporter_total_imports_usd
      v <- v[is.finite(v)]
      if (length(v)) v[1] else sum(baseline_import_value_usd, na.rm = TRUE)
    },
    targeted_baseline_imports_usd = sum(baseline_import_value_usd[is_targeted], na.rm = TRUE),
    direct_disrupted_value_usd = sum(direct_disrupted_value_usd, na.rm = TRUE),
    substitution_allocated_value_usd = sum(substitution_allocated_usd, na.rm = TRUE),
    residual_unmet_value_usd = sum(residual_unmet_value_usd, na.rm = TRUE),
    affected_supplier_count = data.table::uniqueN(
      supplier_iso3[direct_disrupted_value_usd > 0 | residual_unmet_value_usd > 0]
    ),
    affected_hs4_count = data.table::uniqueN(
      hs_code[residual_unmet_value_usd > 0 | direct_disrupted_value_usd > 0]
    ),
    reporter_gdp_current_usd = {
      g <- reporter_gdp_current_usd
      g <- g[is.finite(g)]
      if (length(g)) stats::median(g) else NA_real_
    },
    production_status = production_status[1],
    universe_version = universe_version[1]
  ), by = reporter_iso3]

  com <- dt[, .(residual = sum(residual_unmet_value_usd, na.rm = TRUE)),
            by = .(reporter_iso3, hs_code)]
  topc <- com[, .SD[which.max(residual)], by = reporter_iso3]
  out[topc, highest_commodity_residual_usd := i.residual, on = "reporter_iso3"]

  sup <- dt[, .(residual = sum(residual_unmet_value_usd, na.rm = TRUE)),
            by = .(reporter_iso3, supplier_iso3)]
  tops <- sup[, .SD[which.max(residual)], by = reporter_iso3]
  out[tops, highest_supplier_residual_usd := i.residual, on = "reporter_iso3"]

  out[, residual_unmet_pct_targeted_imports := data.table::fifelse(
    targeted_baseline_imports_usd > 0,
    100 * residual_unmet_value_usd / targeted_baseline_imports_usd,
    NA_real_
  )]
  out[, residual_unmet_pct_total_hs85_imports := data.table::fifelse(
    baseline_total_imports_usd > 0,
    100 * residual_unmet_value_usd / baseline_total_imports_usd,
    NA_real_
  )]
  out[, residual_unmet_pct_gdp := data.table::fifelse(
    isTRUE(sc$include_macro_normalisation) &
      is.finite(reporter_gdp_current_usd) & reporter_gdp_current_usd > 0,
    100 * residual_unmet_value_usd / reporter_gdp_current_usd,
    NA_real_
  )]
  data.table::setorderv(out, c("residual_unmet_value_usd", "reporter_iso3"), c(-1L, 1L))
  out[, scenario_rank := seq_len(.N)]
  out
}

aggregate_shock_commodity_impacts <- function(edges) {
  dt <- data.table::as.data.table(edges)
  if (!nrow(dt)) return(data.table::data.table())
  dt[, .(
    commodity_description = commodity_description[1],
    baseline_imports_usd = sum(baseline_import_value_usd, na.rm = TRUE),
    direct_disrupted_value_usd = sum(direct_disrupted_value_usd, na.rm = TRUE),
    substitution_allocated_value_usd = sum(substitution_allocated_usd, na.rm = TRUE),
    residual_unmet_value_usd = sum(residual_unmet_value_usd, na.rm = TRUE),
    affected_reporter_count = data.table::uniqueN(reporter_iso3[residual_unmet_value_usd > 0 | direct_disrupted_value_usd > 0]),
    affected_supplier_count = data.table::uniqueN(supplier_iso3[direct_disrupted_value_usd > 0]),
    residual_share_of_baseline = data.table::fifelse(
      sum(baseline_import_value_usd, na.rm = TRUE) > 0,
      sum(residual_unmet_value_usd, na.rm = TRUE) / sum(baseline_import_value_usd, na.rm = TRUE),
      NA_real_
    ),
    largest_affected_reporter = {
      x <- .SD[, .(r = sum(residual_unmet_value_usd, na.rm = TRUE)), by = reporter_iso3]
      if (!nrow(x)) NA_character_ else x$reporter_iso3[which.max(x$r)]
    },
    largest_affected_supplier = {
      x <- .SD[, .(r = sum(direct_disrupted_value_usd, na.rm = TRUE)), by = supplier_iso3]
      if (!nrow(x)) NA_character_ else x$supplier_iso3[which.max(x$r)]
    }
  ), by = hs_code]
}

aggregate_shock_supplier_impacts <- function(edges) {
  dt <- data.table::as.data.table(edges)
  if (!nrow(dt)) return(data.table::data.table())
  out <- dt[, {
    shocked <- any(is_targeted)
    subst <- any(substitution_received_usd > 0)
    role <- if (shocked) "shocked_supplier" else if (subst) "substitute_supplier" else "unaffected_supplier"
    .(
      supplier_name = supplier_name[1],
      baseline_supplied_value_usd = sum(baseline_import_value_usd, na.rm = TRUE),
      direct_value_removed_usd = sum(direct_disrupted_value_usd, na.rm = TRUE),
      additional_substitution_received_usd = sum(substitution_received_usd, na.rm = TRUE),
      net_post_shock_supplied_value_usd = sum(post_shock_observed_import_value_usd, na.rm = TRUE),
      reporter_count_affected = data.table::uniqueN(
        reporter_iso3[direct_disrupted_value_usd > 0 | substitution_received_usd > 0]
      ),
      hs4_count_affected = data.table::uniqueN(
        hs_code[direct_disrupted_value_usd > 0 | substitution_received_usd > 0]
      ),
      role = role
    )
  }, by = supplier_iso3]
  data.table::setorderv(out, c("direct_value_removed_usd", "supplier_iso3"), c(-1L, 1L))
  out
}

post_shock_concentration <- function(edges) {
  dt <- data.table::as.data.table(edges)
  if (!nrow(dt)) return(data.table::data.table())

  out <- dt[, {
    vals <- post_shock_observed_import_value_usd
    vals[!is.finite(vals) | vals < 0] <- 0
    tot <- sum(vals)
    if (!is.finite(tot) || tot <= 0) {
      .(
        post_shock_total = 0,
        post_top_1_share = NA_real_,
        post_top_3_share = NA_real_,
        post_supplier_hhi = NA_real_,
        post_effective_supplier_count = NA_real_,
        pre_supplier_hhi = {
          h <- unique(supplier_hhi)
          h <- h[is.finite(h)]
          if (length(h)) h[1] else NA_real_
        },
        delta_hhi = NA_real_
      )
    } else {
      sh <- sort(vals[vals > 0] / tot, decreasing = TRUE)
      hhi <- sum(sh^2)
      .(
        post_shock_total = tot,
        post_top_1_share = sh[1],
        post_top_3_share = sum(sh[seq_len(min(3L, length(sh)))]),
        post_supplier_hhi = hhi,
        post_effective_supplier_count = if (hhi > 0) 1 / hhi else NA_real_,
        pre_supplier_hhi = {
          h <- unique(supplier_hhi)
          h <- h[is.finite(h)]
          if (length(h)) h[1] else NA_real_
        },
        delta_hhi = {
          h <- unique(supplier_hhi)
          h <- h[is.finite(h)]
          if (length(h)) hhi - h[1] else NA_real_
        }
      )
    }
  }, by = .(reporter_iso3, hs_code)]
  out
}

rank_shock_impacts <- function(reporter_impacts, metric = "residual_unmet_value_usd", top_n = 20L) {
  dt <- data.table::as.data.table(reporter_impacts)
  if (!nrow(dt) || !metric %in% names(dt)) return(data.table::data.table())
  dt <- dt[is.finite(get(metric))]
  data.table::setorderv(dt, c(metric, "reporter_iso3"), c(-1L, 1L))
  head(dt, min(as.integer(top_n %||% 20L), nrow(dt)))
}
