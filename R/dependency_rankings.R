rank_dependency_groups <- function(group_conc, metric = "top_1_share", top_n = 20L) {
  dt <- data.table::as.data.table(group_conc)
  if (!nrow(dt) || !metric %in% names(dt)) return(data.table::data.table())
  dt <- dt[is.finite(get(metric))]
  data.table::setorderv(dt, c(metric, "reporter_iso3", "hs_code"), c(-1L, 1L, 1L))
  head(dt, min(as.integer(top_n %||% 20L), nrow(dt)))
}

rank_reporters_by_concentration <- function(reporter_conc, metric = "weighted_hhi", top_n = 20L) {
  dt <- data.table::as.data.table(reporter_conc)
  if (!nrow(dt) || !metric %in% names(dt)) return(data.table::data.table())
  dt <- dt[is.finite(get(metric))]
  data.table::setorderv(dt, c(metric, "reporter_iso3"), c(-1L, 1L))
  head(dt, min(as.integer(top_n %||% 20L), nrow(dt)))
}

supplier_exposure_summary <- function(shares) {
  dt <- data.table::as.data.table(shares)
  if (!nrow(dt)) {
    return(data.table::data.table(
      partner_iso3 = character(), partner_name = character(),
      total_import_value_supplied = numeric(),
      dependent_group_count = integer(),
      sum_partner_shares = numeric(),
      top_supplier_event_count = integer(),
      share_gt_50_count = integer()
    ))
  }
  out <- dt[, .(
    partner_name = partner_name[1],
    total_import_value_supplied = sum(partner_import_value, na.rm = TRUE),
    dependent_group_count = .N,
    sum_partner_shares = sum(partner_share, na.rm = TRUE),
    top_supplier_event_count = sum(supplier_rank == 1L, na.rm = TRUE),
    share_gt_50_count = sum(partner_share > 0.5, na.rm = TRUE)
  ), by = partner_iso3]
  data.table::setorderv(
    out,
    c("total_import_value_supplied", "partner_iso3"),
    c(-1L, 1L)
  )
  out
}

selected_reporter_profile <- function(shares, group_conc, reporter_iso3) {
  iso <- as.character(reporter_iso3 %||% "")
  sh <- data.table::as.data.table(shares)
  gc <- add_commodity_importance(group_conc)
  if (!nzchar(iso) || !nrow(sh)) return(NULL)
  sh <- sh[reporter_iso3 == iso]
  gc <- gc[reporter_iso3 == iso]
  if (!nrow(sh)) return(NULL)
  rw <- reporter_weighted_concentration(gc)
  top_suppliers <- sh[, .(
    partner_import_value = sum(partner_import_value, na.rm = TRUE)
  ), by = .(partner_iso3, partner_name)]
  data.table::setorderv(top_suppliers, c("partner_import_value", "partner_iso3"), c(-1L, 1L))
  list(
    reporter_iso3 = iso,
    reporter_name = sh$reporter_name[1],
    total_imports = sum(sh$partner_import_value, na.rm = TRUE),
    partner_count = data.table::uniqueN(sh$partner_iso3),
    commodity_count = data.table::uniqueN(sh$hs_code),
    weighted = if (nrow(rw)) rw[1] else NULL,
    top_suppliers = head(top_suppliers, 10L),
    top_commodities = head(
      gc[order(-supplier_hhi, reporter_iso3, hs_code)],
      10L
    ),
    important_concentrated = {
      if (!nrow(gc) || !"commodity_import_share" %in% names(gc)) gc[0]
      else {
        x <- gc[is.finite(commodity_import_share) & is.finite(supplier_hhi)]
        data.table::setorderv(x, c("commodity_import_share", "supplier_hhi", "hs_code"), c(-1L, -1L, 1L))
        head(x, 5L)
      }
    }
  )
}

selected_commodity_profile <- function(shares, group_conc, hs_code) {
  hs <- as.character(hs_code %||% "")
  sh <- data.table::as.data.table(shares)
  gc <- data.table::as.data.table(group_conc)
  if (!nzchar(hs) || !nrow(sh)) return(NULL)
  sh <- sh[hs_code == hs]
  gc <- gc[hs_code == hs]
  if (!nrow(sh)) return(NULL)
  top_partners <- sh[, .(
    partner_import_value = sum(partner_import_value, na.rm = TRUE)
  ), by = .(partner_iso3, partner_name)]
  data.table::setorderv(top_partners, c("partner_import_value", "partner_iso3"), c(-1L, 1L))
  list(
    hs_code = hs,
    commodity_description = sh$commodity_description[1],
    total_imports = sum(sh$partner_import_value, na.rm = TRUE),
    reporter_count = data.table::uniqueN(sh$reporter_iso3),
    partner_count = data.table::uniqueN(sh$partner_iso3),
    reporter_concentration = gc[order(-supplier_hhi, reporter_iso3)],
    top_partners = head(top_partners, 10L)
  )
}

dependency_accessibility_summary <- function(built, group_conc, reporter_conc, coverage = NULL) {
  if (is.null(built) || !nrow(built$shares)) {
    return("No eligible reported-import dependency observations for the current filters.")
  }
  n_rep <- data.table::uniqueN(built$shares$reporter_iso3)
  n_hs <- data.table::uniqueN(built$shares$hs_code)
  wh <- if (!is.null(reporter_conc) && nrow(reporter_conc)) {
    stats::median(reporter_conc$weighted_hhi, na.rm = TRUE)
  } else NA_real_
  sprintf(
    paste0(
      "Direct reported-import dependency for %d reporting economies and %d HS4 commodities. ",
      "Median weighted supplier HHI: %s. ",
      "Coverage: %d of %d selected reporting economies. ",
      "Measures describe observed import concentration only — not domestic production, ",
      "substitutability, or indirect supply-chain effects."
    ),
    n_rep, n_hs,
    format_dependency_hhi(wh),
    coverage$represented_reporter_count %||% n_rep,
    coverage$selected_reporter_count %||% n_rep
  )
}
