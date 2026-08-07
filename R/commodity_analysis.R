aggregate_detailed_trend <- function(detailed,
                                       year_min = NULL,
                                       year_max = NULL,
                                       reporters = NULL,
                                       partners = NULL,
                                       flows = c("M", "X"),
                                       hs_codes = NULL,
                                       group_by = c("year", "flow_code")) {
  dt <- prepare_detailed_trade(detailed)
  if (!nrow(dt)) {
    return(data.table::data.table())
  }
  dt <- filter_detailed_trade(
    dt,
    year_min = year_min,
    year_max = year_max,
    reporters = reporters,
    partners = partners,
    flows = flows,
    hs_codes = hs_codes
  )
  if (!nrow(dt)) return(data.table::data.table())
  group_by <- intersect(group_by, names(dt))
  if (!length(group_by)) group_by <- "year"
  out <- dt[, .(
    value = sum(trade_value_usd, na.rm = TRUE),
    n_obs = .N
  ), by = group_by]
  if ("flow_code" %in% names(out)) out[, series := flow_label(flow_code)]
  data.table::setorderv(out, intersect(c("year", "series", "hs_code", "partner_iso3"), names(out)))
  out[, value := sanitize_chart_numeric(value)]
  out
}

select_top_detailed_series <- function(trend_dt, series_col = "series",
                                         top_n = 6L, value_col = "value") {
  dt <- data.table::as.data.table(trend_dt)
  if (!nrow(dt) || !series_col %in% names(dt)) {
    return(list(visible = dt, other_value = 0, coverage_pct = NA_real_, n_total = 0L))
  }
  ranks <- dt[, .(tot = sum(get(value_col), na.rm = TRUE)), by = c(series_col)]
  data.table::setorderv(ranks, c("tot", series_col), c(-1L, 1L))
  n_total <- nrow(ranks)
  keep <- ranks[[series_col]][seq_len(min(as.integer(top_n), n_total))]
  vis <- dt[get(series_col) %in% keep]
  tot_all <- sum(dt[[value_col]], na.rm = TRUE)
  tot_vis <- sum(vis[[value_col]], na.rm = TRUE)
  list(
    visible = vis,
    other_value = tot_all - tot_vis,
    coverage_pct = if (tot_all > 0) 100 * tot_vis / tot_all else NA_real_,
    n_total = n_total,
    n_visible = length(keep)
  )
}

prepare_commodity_treemap <- function(detailed,
                                        year_min = NULL,
                                        year_max = NULL,
                                        reporters = NULL,
                                        partners = NULL,
                                        flows = c("M", "X"),
                                        top_n = 15L,
                                        include_other = TRUE) {
  dt <- filter_detailed_trade(
    prepare_detailed_trade(detailed),
    year_min = year_min, year_max = year_max,
    reporters = reporters, partners = partners, flows = flows
  )
  if (!nrow(dt)) {
    return(list(
      data = data.table::data.table(),
      total = 0, other_value = 0, coverage_pct = NA_real_
    ))
  }
  agg <- dt[, .(
    value = sum(trade_value_usd, na.rm = TRUE),
    commodity_description = commodity_description[1]
  ), by = hs_code]
  agg <- agg[value > 0 & is.finite(value)]
  data.table::setorderv(agg, c("value", "hs_code"), c(-1L, 1L))
  total <- sum(agg$value, na.rm = TRUE)
  top_n <- as.integer(top_n)
  other <- 0
  if (nrow(agg) > top_n) {
    keep <- agg[seq_len(top_n)]
    other <- sum(agg$value[(top_n + 1L):nrow(agg)], na.rm = TRUE)
    if (isTRUE(include_other) && other > 0) {
      keep <- rbind(
        keep,
        data.table::data.table(
          hs_code = "OTHER",
          value = other,
          commodity_description = "Other HS4 (outside top-N)"
        ),
        fill = TRUE
      )
    }
  } else {
    keep <- agg
  }
  keep[, share_pct := if (total > 0) 100 * value / total else NA_real_]
  keep[, label := paste0(hs_code, ifelse(
    is.na(commodity_description) | !nzchar(commodity_description), "",
    paste0(" — ", substr(commodity_description, 1, 40))
  ))]
  list(
    data = keep,
    total = total,
    other_value = other,
    coverage_pct = if (total > 0) 100 * (total - other) / total else NA_real_
  )
}

commodity_mover_key <- function(hs_code, commodity_description, max_desc = 48L) {
  hs <- as.character(hs_code)
  desc <- as.character(commodity_description)
  desc[is.na(desc)] <- ""
  desc <- trimws(desc)
  out <- ifelse(
    !nzchar(desc),
    hs,
    paste0(hs, " — ", substr(desc, 1L, as.integer(max_desc)))
  )
  as.character(out)
}

empty_commodity_movers <- function(start_year = NA_integer_, end_year = NA_integer_) {
  list(
    absolute_increase = data.table::data.table(),
    absolute_decrease = data.table::data.table(),
    pct_increase = data.table::data.table(),
    latest_largest = data.table::data.table(),
    start_year = as.integer(start_year)[1],
    end_year = as.integer(end_year)[1]
  )
}

commodity_movers <- function(detailed,
                               start_year,
                               end_year,
                               reporters = NULL,
                               partners = NULL,
                               flows = c("M", "X"),
                               top_n = 10L) {
  start_year <- as.integer(start_year)
  end_year <- as.integer(end_year)
  if (length(start_year) != 1L || length(end_year) != 1L ||
      is.na(start_year) || is.na(end_year)) {
    return(empty_commodity_movers(start_year, end_year))
  }
  dt <- prepare_detailed_trade(detailed)
  base <- filter_detailed_trade(
    dt, year_min = start_year, year_max = end_year,
    reporters = reporters, partners = partners, flows = flows
  )
  if (!nrow(base)) {
    return(empty_commodity_movers(start_year, end_year))
  }

  base[, hs_code := as.character(hs_code)]
  agg <- base[, .(value = sum(trade_value_usd, na.rm = TRUE)), by = .(hs_code, year)]
  wide <- data.table::dcast(agg, hs_code ~ year, value.var = "value")
  s_col <- as.character(start_year)
  e_col <- as.character(end_year)
  if (!s_col %in% names(wide)) wide[, (s_col) := NA_real_]
  if (!e_col %in% names(wide)) wide[, (e_col) := NA_real_]
  wide[, `:=`(
    start_value = sanitize_chart_numeric(get(s_col)),
    end_value = sanitize_chart_numeric(get(e_col))
  )]

  both <- wide[!is.na(start_value) & !is.na(end_value)]
  if (!nrow(both)) {
    return(empty_commodity_movers(start_year, end_year))
  }
  both[, abs_change := end_value - start_value]
  both[, pct_change := ifelse(start_value > 0, 100 * (end_value / start_value - 1), NA_real_)]

  desc <- base[, .(
    commodity_description = {
      d <- as.character(commodity_description)
      d <- d[!is.na(d) & nzchar(trimws(d))]
      if (!length(d)) NA_character_ else d[[1]]
    }
  ), by = hs_code]
  both <- desc[both, on = "hs_code"]
  both[, commodity_key := commodity_mover_key(hs_code, commodity_description)]
  data.table::setorderv(both, c("hs_code"))
  stopifnot(!anyDuplicated(both$hs_code))

  top_n <- max(0L, as.integer(top_n)[1])
  pos <- both[is.finite(abs_change) & abs_change > 0]
  neg <- both[is.finite(abs_change) & abs_change < 0]
  data.table::setorderv(pos, c("abs_change", "hs_code"), c(-1L, 1L))
  data.table::setorderv(neg, c("abs_change", "hs_code"), c(1L, 1L))
  inc <- if (nrow(pos) && top_n > 0L) pos[seq_len(min(top_n, nrow(pos)))] else pos[0]
  dec <- if (nrow(neg) && top_n > 0L) neg[seq_len(min(top_n, nrow(neg)))] else neg[0]

  pct <- both[!is.na(pct_change) & is.finite(pct_change) & pct_change > 0]
  data.table::setorderv(pct, c("pct_change", "hs_code"), c(-1L, 1L))
  pct <- if (nrow(pct) && top_n > 0L) pct[seq_len(min(top_n, nrow(pct)))] else pct[0]

  latest <- base[year == end_year, .(value = sum(trade_value_usd, na.rm = TRUE)), by = hs_code]
  latest <- desc[latest, on = "hs_code"]
  latest[, commodity_key := commodity_mover_key(hs_code, commodity_description)]
  data.table::setorderv(latest, c("value", "hs_code"), c(-1L, 1L))
  latest <- if (nrow(latest) && top_n > 0L) latest[seq_len(min(top_n, nrow(latest)))] else latest[0]

  list(
    absolute_increase = inc,
    absolute_decrease = dec,
    pct_increase = pct,
    latest_largest = latest,
    start_year = start_year,
    end_year = end_year
  )
}

commodity_movers_empty_message <- function(movers,
                                             detailed = NULL,
                                             reporters = NULL,
                                             partners = NULL,
                                             represented_reporters = NULL,
                                             start_year = NULL,
                                             end_year = NULL) {
  start_year <- as.integer(start_year %||% movers$start_year %||% NA_integer_)
  end_year <- as.integer(end_year %||% movers$end_year %||% NA_integer_)
  if (is.null(detailed) || !nrow(detailed)) {
    return("No detailed bilateral observations are available in the session snapshot.")
  }
  if (is.finite(start_year) && is.finite(end_year) && identical(start_year, end_year)) {
    return("Commodity movers require two distinct years; from-year and to-year are the same.")
  }
  if (!is.null(reporters) && length(reporters) && nzchar(as.character(reporters[1]))) {
    rep <- as.character(reporters[1])
    if (!is.null(represented_reporters) && length(represented_reporters) &&
        !rep %in% as.character(represented_reporters)) {
      return(sprintf(
        "Selected economy %s is outside the represented detailed-coverage reporter set.",
        rep
      ))
    }
    if (!rep %in% as.character(detailed$reporter_iso3)) {
      return(sprintf("Selected economy %s has no detailed bilateral observations.", rep))
    }
  }
  partner_vals <- as.character(partners %||% character())
  partner_vals <- partner_vals[nzchar(partner_vals) & partner_vals != "__ALL__"]
  if (length(partner_vals)) {
    return("Selected partner filter removes all common HS4 observations for the mover comparison.")
  }
  sprintf(
    "No common HS4 observations exist across %s and %s for the current detailed filters.",
    start_year, end_year
  )
}

ts_detailed_kpis <- function(detailed_filtered, coverage = NULL) {
  dt <- data.table::as.data.table(detailed_filtered)
  list(
    filtered_trade_value = if (nrow(dt)) sum(dt$trade_value_usd, na.rm = TRUE) else 0,
    n_observations = nrow(dt),
    n_years = if (nrow(dt)) data.table::uniqueN(dt$year) else 0L,
    n_partners = if (nrow(dt)) data.table::uniqueN(dt$partner_iso3) else 0L,
    n_hs4 = if (nrow(dt)) data.table::uniqueN(dt$hs_code) else 0L,
    coverage_label = if (!is.null(coverage)) {
      sprintf("%d/%d", coverage$represented_reporter_count %||% 0L,
              coverage$selected_reporter_count %||% 0L)
    } else {
      "partial"
    },
    scope_note = if (!is.null(coverage) && coverage_is_selected_universe_complete(coverage)) {
      "Based on completed selected-universe detailed bilateral observations."
    } else {
      "Based on currently available detailed bilateral observations (may be partial)."
    }
  )
}
