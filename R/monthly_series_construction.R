monthly_calendar <- function(start_year = 2019L, end_year = 2024L) {
  tokens <- monthly_period_tokens(start_year, end_year)
  data.table::data.table(
    period = tokens,
    year = as.integer(substr(tokens, 1, 4)),
    month = as.integer(substr(tokens, 5, 6)),
    date = as.Date(paste0(substr(tokens, 1, 4), "-", substr(tokens, 5, 6), "-01"))
  )
}

build_monthly_series_long <- function(raw_rows,
                                        series_meta,
                                        start_year = 2019L,
                                        end_year = 2024L,
                                        universe_version = EXPECTED_UNIVERSE_CHECKSUM) {
  cal <- monthly_calendar(start_year, end_year)
  meta <- data.table::as.data.table(series_meta)
  obs <- data.table::as.data.table(raw_rows)
  if (!nrow(meta)) return(data.table::data.table())

  if (nrow(obs)) {
    obs <- obs[
      !is.na(period) & !is.na(trade_value_usd) & is.finite(trade_value_usd) & trade_value_usd >= 0
    ]
    if (nrow(obs)) {
      obs[, series_id := make_series_id(reporter_iso3, partner_iso3, hs_code, flow_code)]
      obs <- obs[, .(
        trade_value_usd = sum(trade_value_usd, na.rm = TRUE),
        source_row_count = .N,
        reporter_name = reporter_name[1],
        partner_name = partner_name[1],
        commodity_description = commodity_description[1],
        flow_name = flow_name[1],
        hs_revision = hs_revision[1],
        reporter_code = reporter_code[1],
        partner_code = partner_code[1],
        source_updated_at = source_updated_at[1],
        request_id = request_id[1]
      ), by = .(series_id, period, year, month, reporter_iso3, partner_iso3, hs_code, flow_code)]
    }
  }

  out_list <- lapply(seq_len(nrow(meta)), function(i) {
    m <- meta[i]
    sid <- m$series_id
    base <- data.table::copy(cal)
    base[, `:=`(
      series_id = sid,
      frequency = "M",
      reporter_iso3 = m$reporter_iso3,
      partner_iso3 = m$partner_iso3,
      hs_code = as.character(m$hs_code),
      flow_code = m$flow_code,
      reporter_name = m$reporter_name %||% m$reporter_iso3,
      partner_name = m$partner_name %||% m$partner_iso3,
      commodity_description = m$commodity_description %||% m$hs_code,
      reporter_code = m$reporter_code %||% NA_character_,
      partner_code = m$partner_code %||% NA_character_,
      universe_version = universe_version,
      ingested_at = utc_now()
    )]
    if (nrow(obs)) {
      o <- obs[series_id == sid]
      base <- merge(base, o[, .(period, trade_value_usd, source_row_count, flow_name,
                               hs_revision, source_updated_at, request_id)],
                    by = "period", all.x = TRUE)
    } else {
      base[, `:=`(
        trade_value_usd = NA_real_,
        source_row_count = 0L,
        flow_name = NA_character_,
        hs_revision = NA_character_,
        source_updated_at = NA_character_,
        request_id = NA_character_
      )]
    }
    base[, `:=`(
      value_observed = !is.na(trade_value_usd),
      value_missing = is.na(trade_value_usd),
      value_zero = !is.na(trade_value_usd) & trade_value_usd == 0,
      imputation_status = "none",
      source_row_count = as.integer(source_row_count %||% 0L)
    )]
    base[is.na(source_row_count), source_row_count := 0L]
    base
  })
  data.table::rbindlist(out_list, fill = TRUE)
}

longest_run <- function(flag) {
  flag <- as.logical(flag)
  if (!length(flag) || !any(flag, na.rm = TRUE)) return(0L)
  r <- rle(flag)
  max(c(0L, r$lengths[isTRUE(r$values) | r$values == TRUE]), na.rm = TRUE)
}

compute_monthly_series_quality <- function(monthly_long,
                                             min_obs = 24L,
                                             max_missing_rate = 0.35,
                                             max_missing_run = 6L,
                                             require_recent_12 = 9L) {
  dt <- data.table::as.data.table(monthly_long)
  if (!nrow(dt)) return(data.table::data.table())
  data.table::setorderv(dt, c("series_id", "date"))
  q <- dt[, {
    expected <- .N
    observed <- sum(value_observed, na.rm = TRUE)
    missing <- sum(value_missing, na.rm = TRUE)
    zeros <- sum(value_zero, na.rm = TRUE)
    vals <- trade_value_usd[value_observed & !value_zero]
    recent <- utils::tail(.SD[order(date)], 12L)
    cv <- if (length(vals) >= 2L && mean(vals) > 0) {
      stats::sd(vals) / mean(vals)
    } else {
      NA_real_
    }
    intermittency <- if (observed > 0) zeros / observed else NA_real_
    completeness <- observed / expected
    recent_obs <- sum(recent$value_observed, na.rm = TRUE)
    miss_run <- longest_run(value_missing)
    zero_run <- longest_run(value_zero)
    stable <- observed >= min_obs &&
      completeness >= (1 - max_missing_rate) &&
      miss_run <= max_missing_run &&
      recent_obs >= require_recent_12
    reason <- if (isTRUE(stable)) {
      "meets_stability_criteria"
    } else if (observed < min_obs) {
      "insufficient_observations"
    } else if (completeness < (1 - max_missing_rate)) {
      "excessive_missing_rate"
    } else if (miss_run > max_missing_run) {
      "excessive_missing_run"
    } else if (recent_obs < require_recent_12) {
      "insufficient_recent_coverage"
    } else {
      "rejected_other"
    }
    .(
      expected_months = expected,
      observed_months = as.integer(observed),
      missing_months = as.integer(missing),
      zero_months = as.integer(zeros),
      completeness_pct = 100 * completeness,
      missing_rate_pct = 100 * (missing / expected),
      zero_rate_pct = if (observed > 0) 100 * zeros / observed else NA_real_,
      longest_missing_run = as.integer(miss_run),
      longest_zero_run = as.integer(zero_run),
      recent_12_month_availability = as.integer(recent_obs),
      coefficient_of_variation = sanitize_chart_numeric(cv),
      intermittency = sanitize_chart_numeric(intermittency),
      median_monthly_value_usd = if (length(vals)) stats::median(vals) else NA_real_,
      total_historical_value_usd = sum(trade_value_usd, na.rm = TRUE),
      last_observation_date = {
        ok <- date[value_observed]
        if (length(ok)) as.character(max(ok)) else NA_character_
      },
      stable = stable,
      stability_class = if (isTRUE(stable)) "stable" else "rejected",
      selection_or_rejection_reason = reason,
      imputation_count = sum(imputation_status != "none", na.rm = TRUE)
    )
  }, by = series_id]
  data.table::setorderv(q, c("stable", "total_historical_value_usd", "series_id"), c(-1L, -1L, 1L))
  q
}

select_stable_forecast_series <- function(quality, candidates = NULL, n = 10L) {
  q <- data.table::as.data.table(quality)
  if (!nrow(q)) return(data.table::data.table())
  sel <- q[stable == TRUE]
  if (!is.null(candidates) && nrow(candidates) && "series_id" %in% names(candidates)) {

    sel <- merge(sel, candidates[, .(series_id, candidate_rank, pre_forecast_score)],
                 by = "series_id", all.x = TRUE)
    data.table::setorderv(
      sel,
      c("candidate_rank", "total_historical_value_usd", "series_id"),
      c(1L, -1L, 1L)
    )
  } else {
    data.table::setorderv(sel, c("total_historical_value_usd", "series_id"), c(-1L, 1L))
  }
  out <- utils::head(sel, as.integer(n))
  out[, selected_for_forecast := TRUE]
  out
}
