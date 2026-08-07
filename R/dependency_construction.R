filter_dependency_imports <- function(detailed,
                                        year_min = NULL,
                                        year_max = NULL,
                                        reporters = NULL,
                                        partners = NULL,
                                        hs_codes = NULL,
                                        min_link_value = 0,
                                        exclude_self = TRUE) {
  raw <- prepare_detailed_trade(detailed)
  diagnostics <- list(
    source_rows = nrow(raw),
    excluded = data.table::data.table(
      reason = character(), n_rows = integer(), value = numeric()
    )
  )
  empty <- data.table::data.table()
  if (!nrow(raw)) {
    return(list(eligible = empty, diagnostics = diagnostics))
  }

  add_excl <- function(reason, dt) {
    data.table::data.table(
      reason = reason,
      n_rows = nrow(dt),
      value = sum(dt$trade_value_usd, na.rm = TRUE)
    )
  }

  excl <- list()

  exp_rows <- raw[flow_code != "M"]
  if (nrow(exp_rows)) excl <- c(excl, list(add_excl("exports", exp_rows)))
  dt <- raw[flow_code == "M"]

  miss_id <- dt[
    is.na(reporter_iso3) | !nzchar(reporter_iso3) |
      is.na(partner_iso3) | !nzchar(partner_iso3) |
      is.na(hs_code) | !nzchar(as.character(hs_code))
  ]
  if (nrow(miss_id)) excl <- c(excl, list(add_excl("missing_identifiers", miss_id)))
  dt <- dt[
    !is.na(reporter_iso3) & nzchar(reporter_iso3) &
      !is.na(partner_iso3) & nzchar(partner_iso3) &
      !is.na(hs_code) & nzchar(as.character(hs_code))
  ]

  nonpos <- dt[!is.finite(trade_value_usd) | trade_value_usd <= 0]
  if (nrow(nonpos)) excl <- c(excl, list(add_excl("non_positive_value", nonpos)))
  dt <- dt[is.finite(trade_value_usd) & trade_value_usd > 0]

  agg <- dt[partner_iso3 %in% TF_AGGREGATE_PARTNER_ISO3]
  if (nrow(agg)) excl <- c(excl, list(add_excl("aggregate_partner", agg)))
  dt <- dt[!(partner_iso3 %in% TF_AGGREGATE_PARTNER_ISO3)]

  if (isTRUE(exclude_self)) {
    self <- dt[reporter_iso3 == partner_iso3]
    if (nrow(self)) excl <- c(excl, list(add_excl("self_partner", self)))
    dt <- dt[reporter_iso3 != partner_iso3]
  }

  if (!is.null(year_min) && !is.na(year_min)) dt <- dt[year >= as.integer(year_min)]
  if (!is.null(year_max) && !is.na(year_max)) dt <- dt[year <= as.integer(year_max)]
  if (!is.null(reporters) && length(reporters) && !all(reporters %in% c("__ALL__", ""))) {
    reps <- setdiff(as.character(reporters), c("__ALL__", ""))
    if (length(reps)) dt <- dt[reporter_iso3 %in% reps]
  }
  if (!is.null(partners) && length(partners) && !all(partners %in% c("__ALL__", ""))) {
    pars <- setdiff(as.character(partners), c("__ALL__", ""))
    if (length(pars)) dt <- dt[partner_iso3 %in% pars]
  }
  if (!is.null(hs_codes) && length(hs_codes) && !all(hs_codes %in% c("__ALL__", ""))) {
    hs <- setdiff(as.character(hs_codes), c("__ALL__", ""))
    if (length(hs)) dt <- dt[hs_code %in% hs]
  }

  min_link_value <- as.numeric(min_link_value %||% 0)
  if (is.finite(min_link_value) && min_link_value > 0) {
    low <- dt[trade_value_usd < min_link_value]
    if (nrow(low)) excl <- c(excl, list(add_excl("below_min_link_value", low)))
    dt <- dt[trade_value_usd >= min_link_value]
  }

  if (length(excl)) {
    diagnostics$excluded <- data.table::rbindlist(excl)
  }
  diagnostics$eligible_rows <- nrow(dt)
  diagnostics$eligible_value <- sum(dt$trade_value_usd, na.rm = TRUE)
  diagnostics$self_partner_count <- sum(diagnostics$excluded$reason == "self_partner")
  diagnostics$self_partner_value <- sum(
    diagnostics$excluded[reason == "self_partner"]$value, na.rm = TRUE
  )

  list(eligible = dt, diagnostics = diagnostics)
}

aggregate_dependency_links <- function(eligible_imports) {
  dt <- data.table::as.data.table(eligible_imports)
  if (!nrow(dt)) {
    return(data.table::data.table(
      reporter_iso3 = character(), reporter_name = character(),
      partner_iso3 = character(), partner_name = character(),
      hs_code = character(), commodity_description = character(),
      year_start = integer(), year_end = integer(),
      partner_import_value = numeric(), observation_count = integer(),
      reporter_gdp_current_usd = numeric()
    ))
  }
  out <- dt[, .(
    reporter_name = reporter_name[1],
    partner_name = partner_name[1],
    commodity_description = commodity_description[1],
    year_start = min(year, na.rm = TRUE),
    year_end = max(year, na.rm = TRUE),
    partner_import_value = sum(trade_value_usd, na.rm = TRUE),
    observation_count = .N,
    reporter_gdp_current_usd = {
      if ("reporter_gdp_current_usd" %in% names(dt)) {
        stats::median(reporter_gdp_current_usd, na.rm = TRUE)
      } else NA_real_
    }
  ), by = .(reporter_iso3, partner_iso3, hs_code)]
  out <- out[partner_import_value > 0 & is.finite(partner_import_value)]
  data.table::setorderv(
    out,
    c("reporter_iso3", "hs_code", "partner_import_value", "partner_iso3"),
    c(1L, 1L, -1L, 1L)
  )
  out
}

build_dependency_shares <- function(links) {
  dt <- data.table::as.data.table(links)
  if (!nrow(dt)) {
    return(data.table::data.table())
  }
  totals <- dt[, .(
    reporter_commodity_total = sum(partner_import_value, na.rm = TRUE)
  ), by = .(reporter_iso3, hs_code)]
  dt <- merge(dt, totals, by = c("reporter_iso3", "hs_code"), all.x = TRUE)
  dt <- dt[reporter_commodity_total > 0 & is.finite(reporter_commodity_total)]
  dt[, partner_share := partner_import_value / reporter_commodity_total]
  dt[, partner_share := sanitize_chart_numeric(partner_share)]
  dt[partner_share < 0 | partner_share > 1, partner_share := NA_real_]

  data.table::setorderv(
    dt,
    c("reporter_iso3", "hs_code", "partner_import_value", "partner_iso3"),
    c(1L, 1L, -1L, 1L)
  )
  dt[, supplier_rank := seq_len(.N), by = .(reporter_iso3, hs_code)]
  dt
}

reconcile_dependency_shares <- function(shares, tol = DEP_SHARE_TOLERANCE) {
  dt <- data.table::as.data.table(shares)
  if (!nrow(dt)) {
    return(list(ok = TRUE, max_abs_error = 0, n_groups = 0L, failures = 0L))
  }
  chk <- dt[, .(
    share_sum = sum(partner_share, na.rm = TRUE)
  ), by = .(reporter_iso3, hs_code)]
  chk[, abs_err := abs(share_sum - 1)]
  list(
    ok = all(chk$abs_err <= tol, na.rm = TRUE),
    max_abs_error = if (nrow(chk)) max(chk$abs_err, na.rm = TRUE) else 0,
    n_groups = nrow(chk),
    failures = sum(chk$abs_err > tol, na.rm = TRUE)
  )
}

construct_dependency_table <- function(detailed,
                                         year_min = NULL,
                                         year_max = NULL,
                                         reporters = NULL,
                                         partners = NULL,
                                         hs_codes = NULL,
                                         min_link_value = 0,
                                         exclude_self = TRUE) {
  filt <- filter_dependency_imports(
    detailed,
    year_min = year_min,
    year_max = year_max,
    reporters = reporters,
    partners = partners,
    hs_codes = hs_codes,
    min_link_value = min_link_value,
    exclude_self = exclude_self
  )
  links <- aggregate_dependency_links(filt$eligible)
  shares <- build_dependency_shares(links)
  recon <- reconcile_dependency_shares(shares)
  list(
    eligible = filt$eligible,
    links = links,
    shares = shares,
    diagnostics = filt$diagnostics,
    reconciliation = recon,
    year_min = year_min,
    year_max = year_max
  )
}
