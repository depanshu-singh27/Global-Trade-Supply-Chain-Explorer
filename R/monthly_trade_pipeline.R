comtrade_fetch_monthly_once <- function(cfg,
                                          reporter_code,
                                          partner_codes,
                                          period,
                                          flow_code,
                                          cmd_code,
                                          classification = "HS") {
  key <- comtrade_subscription_key()
  base <- cfg$comtrade$base_url
  type <- cfg$comtrade$type_code
  cl <- classification %||% cfg$comtrade$classification %||% "HS"
  reporter_code <- as.character(reporter_code)
  if (identical(reporter_code, "0") || !nzchar(reporter_code)) {
    stop("Invalid reporter_code for monthly final-data request.", call. = FALSE)
  }
  url_path <- paste(base, type, "M", cl, sep = "/")
  partners <- paste(unique(as.character(partner_codes)), collapse = ",")
  period_str <- if (length(period) > 1L) {
    paste(unique(as.character(period)), collapse = ",")
  } else {
    as.character(period)
  }
  query <- list(
    reporterCode = reporter_code,
    period = period_str,
    cmdCode = as.character(cmd_code),
    flowCode = as.character(flow_code),
    partnerCode = partners,
    partner2Code = "0",
    customsCode = "C00",
    motCode = "0",
    maxRecords = 50000,
    format = "JSON",
    includeDesc = "true"
  )
  timeout_sec <- max(as.numeric(cfg$api$timeout_seconds %||% 60), 180)
  req <- httr2::request(url_path) |>
    httr2::req_headers(`Ocp-Apim-Subscription-Key` = key)
  req <- do.call(httr2::req_url_query, c(list(req), query))
  req <- req |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_error(is_error = function(resp) FALSE)

  log_msg(sprintf(
    "Comtrade monthly request classification=%s reporter=%s period_tokens=%d flow=%s partners=%s cmd=%s",
    cl, reporter_code, length(strsplit(period_str, ",", fixed = TRUE)[[1]]),
    flow_code, partners, cmd_code
  ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  body_text <- httr2::resp_body_string(resp)
  list(
    status = status,
    body_text = body_text,
    request_meta = list(
      endpoint = sprintf("comtradeapi.un.org/data/v1/get/C/M/%s", cl),
      classification = as.character(cl),
      reporter_code = reporter_code,
      partner_codes = as.character(partner_codes),
      period = period_str,
      flow_code = as.character(flow_code),
      cmd_code = as.character(cmd_code),
      frequency = "M",
      http_status = status,
      requested_at = utc_now()
    )
  )
}

run_monthly_quota_preflight <- function(cfg = load_config()) {

  res <- tryCatch(
    comtrade_fetch_monthly_once(
      cfg,
      reporter_code = "276",
      partner_codes = "0",
      period = "202301",
      flow_code = "M",
      cmd_code = "8542",
      classification = "HS"
    ),
    error = function(e) list(status = 0L, body_text = conditionMessage(e))
  )
  status <- as.integer(res$status %||% 0L)
  cls <- classify_http_failure(status, body_text = res$body_text)
  if (!is.na(status) && status >= 200L && status < 300L) {
    cat("MONTHLY_QUOTA_AVAILABLE http_status=", status, "\n", sep = "")
    return(invisible(list(available = TRUE, http_status = status, category = "ok")))
  }
  cat(
    "MONTHLY_QUOTA_UNAVAILABLE http_status=", status,
    " category=", cls$category %||% "unknown", "\n",
    sep = ""
  )
  invisible(list(available = FALSE, http_status = status, category = cls$category))
}

parse_monthly_comtrade_json <- function(body_text, request_meta = list()) {
  parsed <- tryCatch(jsonlite::fromJSON(body_text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed)) {
    return(list(ok = FALSE, rows = data.table::data.table(), error = "malformed_json"))
  }
  data_list <- parsed$data %||% parsed$dataset %||% list()
  if (!length(data_list)) {
    return(list(ok = TRUE, rows = data.table::data.table(), empty = TRUE))
  }
  rows <- lapply(data_list, function(r) {
    period <- pluck_chr(r$period %||% r$Period)
    yr <- suppressWarnings(as.integer(substr(period, 1, 4)))
    mo <- suppressWarnings(as.integer(substr(period, 5, 6)))
    if (is.na(mo) && nchar(period) <= 4) mo <- NA_integer_
    data.table::data.table(
      period = period,
      year = yr,
      month = mo,
      reporter_code = pluck_chr(r$reporterCode %||% r$rtCode),
      reporter_iso3 = pluck_chr(r$reporterISO %||% r$rtISO),
      reporter_name = pluck_chr(r$reporterDesc %||% r$rtTitle),
      partner_code = pluck_chr(r$partnerCode %||% r$ptCode),
      partner_iso3 = pluck_chr(r$partnerISO %||% r$ptISO),
      partner_name = pluck_chr(r$partnerDesc %||% r$ptTitle),
      flow_code = pluck_chr(r$flowCode),
      flow_name = pluck_chr(r$flowDesc),
      hs_revision = pluck_chr(r$classificationCode %||% request_meta$classification),
      hs_code = as.character(pluck_chr(r$cmdCode %||% r$cmdCode)),
      commodity_description = pluck_chr(r$cmdDesc),
      trade_value_usd = pluck_num(r$primaryValue %||% r$TradeValue),
      source_updated_at = pluck_chr(r$isLatestPeriod %||% NA_character_),
      request_id = request_meta$request_id %||% NA_character_
    )
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  list(ok = TRUE, rows = out, empty = !nrow(out))
}

execute_monthly_forecast_fetch <- function(cfg = load_config(),
                                             max_requests = Inf,
                                             dry_run = FALSE,
                                             retry_failed_only = FALSE) {
  plan_path <- monthly_plan_file(cfg)
  state_path <- monthly_pipeline_state_file(cfg)
  plan <- safe_read_parquet_dt(plan_path)
  state <- safe_read_parquet_dt(state_path)
  if (is.null(plan) || !nrow(plan)) {
    stop("Monthly forecast request plan missing. Run the planner first.", call. = FALSE)
  }
  if (is.null(state) || !nrow(state)) {
    state <- init_state_from_plan(plan)
  }

  state <- recover_stale_running(state, stale_minutes = 120)

  eligible <- if (isTRUE(retry_failed_only)) {
    state[status %in% c("retryable_failed", "quota_blocked")]
  } else {
    state[status %in% c("planned", "retryable_failed", "quota_blocked")]
  }
  if (!nrow(eligible)) {
    cat("MONTHLY_FETCH_NOTHING_TO_DO\n")
    return(invisible(list(executed = 0L, quota_blocked = FALSE)))
  }

  n_done <- 0L
  quota_hit <- FALSE
  ensure_dir(file.path(cfg$paths$raw, "comtrade", "monthly_forecasting"))

  for (rid in eligible$request_id) {
    if (n_done >= max_requests) break
    row <- plan[request_id == rid][1]
    if (!nrow(row)) next
    if (isTRUE(dry_run)) {
      cat("DRY_RUN request_id=", rid, "\n", sep = "")
      n_done <- n_done + 1L
      next
    }

    raw_path <- file.path(find_project_root(), row$raw_output_path)

    if (file.exists(raw_path)) {
      chk <- safe_md5_file(raw_path)
      st_row <- state[request_id == rid]
      if (nrow(st_row) && identical(st_row$raw_checksum[1], chk) &&
          st_row$status[1] %in% c("succeeded", "empty", "skipped_cached")) {
        state[request_id == rid, status := "skipped_cached"]
        n_done <- n_done + 1L
        next
      }
    }

    state[request_id == rid, `:=`(
      status = "running",
      started_at = utc_now(),
      attempts = as.integer(attempts %||% 0L) + 1L
    )]
    atomic_write_parquet_dt(state, state_path)

    out <- tryCatch(
      comtrade_fetch_monthly_once(
        cfg,
        reporter_code = row$reporter_code,
        partner_codes = row$partner_code,
        period = strsplit(row$period, ",", fixed = TRUE)[[1]],
        flow_code = row$flow_code,
        cmd_code = row$hs_code,
        classification = row$classification %||% "HS"
      ),
      error = function(e) list(status = 0L, body_text = conditionMessage(e))
    )
    status <- as.integer(out$status %||% 0L)
    cls <- classify_http_failure(status, body_text = out$body_text)

    if (identical(cls$status, "quota_blocked")) {
      state[request_id == rid, `:=`(
        status = "quota_blocked",
        completed_at = utc_now(),
        http_status = status,
        error_category = cls$category,
        error_message = "quota_exhausted"
      )]
      atomic_write_parquet_dt(state, state_path)
      cat("MONTHLY_QUOTA_BLOCKED stopping after request_id=", rid, "\n", sep = "")
      quota_hit <- TRUE
      break
    }

    if (!is.na(status) && status >= 200L && status < 300L) {
      ensure_dir(dirname(raw_path))
      writeLines(out$body_text, raw_path, useBytes = TRUE)
      parsed <- parse_monthly_comtrade_json(
        out$body_text,
        request_meta = c(out$request_meta, list(request_id = rid))
      )
      state[request_id == rid, `:=`(
        status = if (isTRUE(parsed$empty)) "empty" else "succeeded",
        completed_at = utc_now(),
        http_status = status,
        result_row_count = nrow(parsed$rows),
        raw_file = row$raw_output_path,
        raw_checksum = safe_md5_file(raw_path),
        error_category = NA_character_,
        error_message = NA_character_
      )]
    } else {
      state[request_id == rid, `:=`(
        status = cls$status %||% "retryable_failed",
        completed_at = utc_now(),
        http_status = status,
        error_category = cls$category,
        error_message = substr(as.character(out$body_text %||% ""), 1, 200)
      )]
    }
    atomic_write_parquet_dt(state, state_path)
    n_done <- n_done + 1L
  }

  invisible(list(executed = n_done, quota_blocked = quota_hit, state = state))
}
