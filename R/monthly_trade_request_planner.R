monthly_period_tokens <- function(start_year = 2019L, end_year = 2024L) {
  ys <- seq.int(as.integer(start_year), as.integer(end_year))
  unlist(lapply(ys, function(y) sprintf("%04d%02d", y, 1:12)), use.names = FALSE)
}

monthly_request_id <- function(series_id, period_start, period_end, classification = "HS") {
  sprintf(
    "monthly_%s_%s_%s_%s",
    sanitize_download_token(series_id, "series"),
    as.character(period_start),
    as.character(period_end),
    as.character(classification)
  )
}

plan_monthly_forecast_requests <- function(candidates,
                                             start_year = 2019L,
                                             end_year = 2024L,
                                             strategy = c("full_period", "by_year"),
                                             classification = "HS",
                                             universe_version = EXPECTED_UNIVERSE_CHECKSUM) {
  strategy <- match.arg(strategy)
  cand <- data.table::as.data.table(candidates)
  if (!nrow(cand)) {
    return(data.table::data.table())
  }
  period_start <- sprintf("%04d01", as.integer(start_year))
  period_end <- sprintf("%04d12", as.integer(end_year))

  rows <- lapply(seq_len(nrow(cand)), function(i) {
    r <- cand[i]
    if (identical(strategy, "by_year")) {
      years <- seq.int(as.integer(start_year), as.integer(end_year))
      lapply(years, function(y) {
        ps <- sprintf("%04d01", y)
        pe <- sprintf("%04d12", y)
        data.table::data.table(
          request_id = monthly_request_id(r$series_id, ps, pe, classification),
          series_id = r$series_id,
          reporter_code = as.character(r$reporter_code %||% NA_character_),
          reporter_iso3 = r$reporter_iso3,
          partner_code = as.character(r$partner_code %||% NA_character_),
          partner_iso3 = r$partner_iso3,
          hs_code = as.character(r$hs_code),
          flow_code = as.character(r$flow_code),
          period_start = ps,
          period_end = pe,
          period = paste(sprintf("%04d%02d", y, 1:12), collapse = ","),
          frequency = "M",
          classification = classification,
          status = "planned",
          attempt_count = 0L,
          raw_output_path = file.path(
            "data", "raw", "comtrade", "monthly_forecasting",
            paste0(monthly_request_id(r$series_id, ps, pe, classification), ".json")
          ),
          universe_version = universe_version,
          candidate_version = r$candidate_version %||% FORECAST_CANDIDATE_VERSION,
          dataset_type = "monthly_forecast"
        )
      }) |> data.table::rbindlist(fill = TRUE)
    } else {
      periods <- monthly_period_tokens(start_year, end_year)
      data.table::data.table(
        request_id = monthly_request_id(r$series_id, period_start, period_end, classification),
        series_id = r$series_id,
        reporter_code = as.character(r$reporter_code %||% NA_character_),
        reporter_iso3 = r$reporter_iso3,
        partner_code = as.character(r$partner_code %||% NA_character_),
        partner_iso3 = r$partner_iso3,
        hs_code = as.character(r$hs_code),
        flow_code = as.character(r$flow_code),
        period_start = period_start,
        period_end = period_end,
        period = paste(periods, collapse = ","),
        frequency = "M",
        classification = classification,
        status = "planned",
        attempt_count = 0L,
        raw_output_path = file.path(
          "data", "raw", "comtrade", "monthly_forecasting",
          paste0(monthly_request_id(r$series_id, period_start, period_end, classification), ".json")
        ),
        universe_version = universe_version,
        candidate_version = r$candidate_version %||% FORECAST_CANDIDATE_VERSION,
        dataset_type = "monthly_forecast"
      )
    }
  })
  plan <- data.table::rbindlist(rows, fill = TRUE)
  data.table::setorderv(plan, c("series_id", "period_start", "request_id"))

  drop <- grep("key|token|secret|header|url", names(plan), ignore.case = TRUE, value = TRUE)
  if (length(drop)) plan[, (drop) := NULL]
  plan
}

summarise_monthly_plan <- function(plan) {
  dt <- data.table::as.data.table(plan)
  list(
    n_requests = nrow(dt),
    n_series = data.table::uniqueN(dt$series_id),
    strategy = if (nrow(dt) && data.table::uniqueN(dt$period_start) == 1L) "full_period" else "split",
    period_start = if (nrow(dt)) min(dt$period_start) else NA_character_,
    period_end = if (nrow(dt)) max(dt$period_end) else NA_character_,
    universe_version = if (nrow(dt)) dt$universe_version[1] else NA_character_,
    candidate_version = if (nrow(dt)) dt$candidate_version[1] else NA_character_,
    generated_at = utc_now(),
    contains_credentials = FALSE
  )
}

monthly_plan_file <- function(cfg = load_config()) {
  file.path(cfg$paths$interim, "monthly_forecast_request_plan.parquet")
}

monthly_plan_summary_file <- function(cfg = load_config()) {
  file.path(cfg$paths$interim, "monthly_forecast_request_plan_summary.json")
}

monthly_pipeline_state_file <- function(cfg = load_config()) {
  file.path(cfg$paths$interim, "monthly_forecast_pipeline_state.parquet")
}

write_monthly_plan <- function(plan, cfg = load_config()) {
  ensure_dir(cfg$paths$interim)
  atomic_write_parquet_dt(data.table::as.data.table(plan), monthly_plan_file(cfg))
  summary <- summarise_monthly_plan(plan)
  write_json_atomic(summary, monthly_plan_summary_file(cfg))
  st <- init_state_from_plan(plan, existing_state = safe_read_parquet_dt(monthly_pipeline_state_file(cfg)))
  atomic_write_parquet_dt(st, monthly_pipeline_state_file(cfg))
  invisible(list(plan = plan, summary = summary, state = st))
}
