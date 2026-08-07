classify_http_failure <- function(http_status, body_text = NULL, error_message = NULL) {
  status <- suppressWarnings(as.integer(http_status %||% NA_integer_))
  snip <- paste(
    if (!is.null(body_text)) substr(as.character(body_text), 1, 500) else "",
    if (!is.null(error_message)) substr(as.character(error_message), 1, 500) else "",
    sep = " "
  )
  snip_l <- tolower(snip)

  quota_hit <- grepl(
    "quota|call volume|rate limit|usage limit|exceeded.*limit|subscription.*limit|over.*limit",
    snip_l
  )
  auth_hit <- grepl(
    "unauthor|unauthorized|forbidden|invalid.*(key|token|subscription)|access denied|not entitled|permission",
    snip_l
  )

  if (!is.na(status) && status == 403L) {
    if (quota_hit && !auth_hit) {
      return(list(category = "quota_exhausted", status = "quota_blocked", retryable = TRUE))
    }
    if (auth_hit && !quota_hit) {
      return(list(category = "auth_forbidden", status = "permanently_failed", retryable = FALSE))
    }
    if (quota_hit) {
      return(list(category = "quota_exhausted", status = "quota_blocked", retryable = TRUE))
    }

    return(list(category = "quota_exhausted", status = "quota_blocked", retryable = TRUE))
  }

  if (!is.na(status) && status == 429L) {
    return(list(category = "rate_limited", status = "retryable_failed", retryable = TRUE))
  }
  if (quota_hit) {
    return(list(category = "quota_exhausted", status = "quota_blocked", retryable = TRUE))
  }
  if (!is.na(status) && status >= 500L) {
    return(list(category = "http_server_error", status = "retryable_failed", retryable = TRUE))
  }
  if (grepl("timeout|temporar|timed out", snip_l)) {
    return(list(category = "timeout", status = "retryable_failed", retryable = TRUE))
  }
  list(category = "http_error", status = "retryable_failed", retryable = TRUE)
}

run_quota_preflight <- function(cfg = load_config()) {

  res <- tryCatch(
    comtrade_fetch_once(
      cfg,
      reporter_code = "842",
      partner_codes = "0",
      year = 2023,
      flow_code = "M",
      cmd_code = "85",
      classification = "HS",
      omit_partner = FALSE
    ),
    error = function(e) list(status = 0L, body_text = conditionMessage(e), error = e)
  )
  status <- as.integer(res$status %||% 0L)
  body <- res$body_text
  cls <- classify_http_failure(status, body_text = body)
  if (!is.na(status) && status >= 200L && status < 300L) {
    cat("QUOTA_AVAILABLE http_status=", status, "\n", sep = "")
    return(invisible(list(available = TRUE, http_status = status, category = "ok")))
  }
  if (identical(cls$category, "quota_exhausted")) {
    cat("QUOTA_UNAVAILABLE http_status=", status, " category=", cls$category, "\n", sep = "")
    return(invisible(list(available = FALSE, http_status = status, category = cls$category)))
  }
  cat("QUOTA_UNAVAILABLE http_status=", status, " category=", cls$category, "\n", sep = "")
  invisible(list(available = FALSE, http_status = status, category = cls$category))
}

execute_production_request <- function(cfg, req, request_delay_seconds = 1.0) {
  Sys.sleep(as.numeric(request_delay_seconds %||% 0))

  omit_partner <- isTRUE(as.logical(req$omit_partner %||% FALSE))
  classification <- req$classification %||% cfg$comtrade$classification %||% "HS"
  period <- req$period %||% req$year
  partner_codes <- if (omit_partner) {
    NULL
  } else {
    pcs <- req$partner_codes_api %||% req$partner_code
    if (is.character(pcs) && length(pcs) == 1L && grepl(",", pcs)) {
      strsplit(pcs, ",", fixed = TRUE)[[1]]
    } else {
      pcs
    }
  }

  res <- comtrade_fetch_once(
    cfg,
    reporter_code = req$reporter_codes_api %||% req$reporter_code,
    partner_codes = partner_codes %||% "0",
    year = period,
    flow_code = req$flow_code,
    cmd_code = req$cmd_code,
    classification = classification,
    omit_partner = omit_partner
  )
  status <- res$status
  body_text <- res$body_text
  parsed <- parse_comtrade_payload_production(body_text)
  dt <- comtrade_records_to_dt_production(parsed$records)
  list(status = status, body_text = body_text, parsed = parsed, dt = dt, meta = res$request_meta)
}

should_skip_cached <- function(state_dt, req_row, cfg = load_config()) {
  rid <- req_row$request_id
  st <- state_dt[state_dt$request_id == rid]
  if (!nrow(st)) return(FALSE)
  if (!(st$status %in% c("succeeded", "empty", "skipped_cached"))) return(FALSE)
  raw_file <- st$raw_file %||% req_row$raw_file
  if (is.null(raw_file) || !nzchar(raw_file) || !file.exists(raw_file)) return(FALSE)
  if (is.na(st$raw_checksum) || !nzchar(st$raw_checksum)) return(FALSE)
  md5 <- safe_md5_file(raw_file)
  identical(md5, st$raw_checksum)
}

run_production_fetch_stage <- function(cfg,
                                        plan_dt,
                                        stage_filter = NULL,
                                        request_delay_seconds = 1.0,
                                        dry_run = FALSE,
                                        max_requests = Inf,
                                        retry_failed_only = FALSE,
                                        refresh_raw = FALSE) {
  plan_dt <- data.table::as.data.table(plan_dt)
  if ("plan_status" %in% names(plan_dt)) {
    plan_dt <- plan_dt[is.na(plan_status) | plan_status == "active"]
  }
  if (!is.null(stage_filter)) {
    plan_dt <- plan_dt[dataset_type %in% stage_filter]
  }

  if (nrow(plan_dt)) {
    bad <- plan_dt$reporter_code == "0" | plan_dt$reporter_codes_api == "0"
    if (any(bad, na.rm = TRUE)) {
      stop("Active plan still contains reporterCode=0 final-data requests.", call. = FALSE)
    }
  }

  st <- load_state(cfg)
  st <- init_state_from_plan(plan_dt, existing_state = st)
  st <- recover_stale_running(st, stale_minutes = 30)

  st_scoped <- st[request_id %in% as.character(plan_dt$request_id)]

  skip_n <- 0L
  if (!refresh_raw) {
    already <- st_scoped[status %in% c("succeeded", "empty", "skipped_cached")]
    if (nrow(already)) {
      for (j in seq_len(nrow(already))) {
        row <- already[j]
        req_row <- plan_dt[request_id == row$request_id][1]
        if (nrow(req_row) && should_skip_cached(st_scoped, req_row, cfg = cfg)) {
          st <- mark_terminal(
            st, row$request_id,
            status = "skipped_cached",
            attempts = row$attempts,
            http_status = row$http_status,
            result_row_count = row$result_row_count,
            raw_file = req_row$raw_file,
            raw_checksum = row$raw_checksum
          )
          skip_n <- skip_n + 1L
        }
      }
      if (skip_n > 0L) save_state(st, cfg)
      st_scoped <- st[request_id %in% as.character(plan_dt$request_id)]
    }
  }

  to_run <- select_requests_to_run(
    st_scoped, retry_failed_only = retry_failed_only, max_requests = max_requests
  )
  if (!nrow(to_run)) return(list(state = st, executed = 0L, skipped = skip_n, stopped_quota = FALSE))

  exec_n <- 0L
  stopped_quota <- FALSE

  for (i in seq_len(nrow(to_run))) {
    req_id <- to_run$request_id[i]
    req <- plan_dt[request_id == req_id][1]

    if (!refresh_raw && should_skip_cached(st, req, cfg = cfg)) {
      st <- mark_terminal(
        st, req_id,
        status = "skipped_cached",
        attempts = st[request_id == req_id]$attempts,
        http_status = st[request_id == req_id]$http_status,
        result_row_count = st[request_id == req_id]$result_row_count,
        raw_file = req$raw_file,
        raw_checksum = st[request_id == req_id]$raw_checksum
      )
      save_state(st, cfg)
      skip_n <- skip_n + 1L
      next
    }
    if (dry_run) next

    st[request_id == req_id, `:=`(
      status = "running",
      started_at = utc_now(),
      attempts = st[request_id == req_id]$attempts + 1L,
      error_category = NA_character_,
      error_message = NA_character_
    )]
    save_state(st, cfg)

    out <- tryCatch(
      execute_production_request(cfg, req, request_delay_seconds = request_delay_seconds),
      error = function(e) list(status = 0L, body_text = NULL, parsed = NULL, dt = NULL, error = e)
    )

    raw_checksum <- NA_character_
    raw_ok <- FALSE
    if (!is.null(out$body_text)) {
      ensure_dir(dirname(req$raw_file))
      writeLines(out$body_text, req$raw_file, useBytes = TRUE)
      raw_checksum <- safe_md5_file(req$raw_file)
      raw_ok <- TRUE
    }

    if (!is.null(out$error)) {
      err_msg <- conditionMessage(out$error)
      is_invalid <- grepl("Invalid reporter_code|reporterCode=0", err_msg, ignore.case = TRUE)
      cls <- classify_http_failure(out$status, body_text = out$body_text, error_message = err_msg)
      st <- mark_terminal(
        st, req_id,
        status = if (is_invalid) {
          "invalid"
        } else {
          cls$status
        },
        http_status = as.integer(out$status %||% NA_integer_),
        attempts = st[request_id == req_id]$attempts,
        raw_file = req$raw_file,
        raw_checksum = if (raw_ok) raw_checksum else NA_character_,
        error_category = if (is_invalid) "invalid_request" else cls$category,
        error_message = substr(err_msg, 1, 400)
      )
      save_state(st, cfg)
      if (!is_invalid && identical(cls$category, "quota_exhausted")) {
        cat("QUOTA_EXHAUSTED stop_after_request=", req_id, " http_status=",
            as.integer(out$status %||% NA_integer_), "\n", sep = "")
        stopped_quota <- TRUE
        break
      }
      next
    }

    http_status <- as.integer(out$status %||% NA_integer_)
    if (is.na(http_status) || http_status < 200 || http_status >= 300) {
      cls <- classify_http_failure(http_status, body_text = out$body_text)
      st <- mark_terminal(
        st, req_id,
        status = cls$status,
        http_status = http_status,
        attempts = st[request_id == req_id]$attempts,
        result_row_count = 0L,
        raw_file = req$raw_file,
        raw_checksum = if (raw_ok) raw_checksum else NA_character_,
        error_category = cls$category,
        error_message = paste0("HTTP status ", http_status)
      )
      save_state(st, cfg)
      if (identical(cls$category, "quota_exhausted")) {
        cat("QUOTA_EXHAUSTED stop_after_request=", req_id, " http_status=", http_status, "\n", sep = "")
        stopped_quota <- TRUE
        break
      }
      next
    }

    if (isTRUE(out$parsed$is_truncated)) {
      st <- mark_terminal(
        st, req_id,
        status = "retryable_failed",
        http_status = http_status,
        attempts = st[request_id == req_id]$attempts,
        result_row_count = nrow(out$dt %||% data.table::data.table()),
        raw_file = req$raw_file,
        raw_checksum = if (raw_ok) raw_checksum else NA_character_,
        error_category = "truncated",
        error_message = sprintf(
          "Comtrade response truncated: returned=%d total=%s",
          out$parsed$returned_count, out$parsed$total_count
        )
      )
      save_state(st, cfg)
      next
    }

    row_n <- if (!is.null(out$dt) && nrow(out$dt)) nrow(out$dt) else 0L
    if (row_n == 0L) {
      st <- mark_terminal(
        st, req_id,
        status = "empty",
        http_status = http_status,
        attempts = st[request_id == req_id]$attempts,
        result_row_count = 0L,
        raw_file = req$raw_file,
        raw_checksum = if (raw_ok) raw_checksum else NA_character_
      )
      save_state(st, cfg)
      next
    }

    parsed_path <- file.path(dirname(req$raw_file), paste0(req_id, ".parquet"))
    out$dt[, `:=`(request_id = req_id, ingested_at = utc_now())]
    ensure_dir(dirname(parsed_path))
    arrow::write_parquet(out$dt, parsed_path)

    st <- mark_terminal(
      st, req_id,
      status = "succeeded",
      http_status = http_status,
      attempts = st[request_id == req_id]$attempts,
      result_row_count = row_n,
      raw_file = req$raw_file,
      raw_checksum = if (raw_ok) raw_checksum else NA_character_
    )
    save_state(st, cfg)
    exec_n <- exec_n + 1L
  }

  list(state = st, executed = exec_n, skipped = skip_n, stopped_quota = stopped_quota)
}

rebuild_partial_detailed_from_caches <- function(cfg, universe, universe_checksum,
                                                   years = 2019:2024) {
  top_reporters <- data.table::as.data.table(universe$top_reporters)
  top_partners <- data.table::as.data.table(universe$top_partners)
  top_hs4 <- data.table::as.data.table(universe$top_hs4)
  plan_path <- request_plan_file(cfg)
  plan <- if (file.exists(plan_path)) {
    data.table::as.data.table(arrow::read_parquet(plan_path))
  } else {
    data.table::data.table()
  }
  st <- load_state(cfg)
  if (is.null(st)) st <- state_schema()

  active <- if (nrow(plan)) {
    plan[dataset_type == "trade_detailed_top20" &
           (is.na(plan_status) | plan_status == "active")]
  } else {
    data.table::data.table()
  }

  rep_set <- as.character(top_reporters$reporter_code)
  par_set <- as.character(top_partners$partner_code)
  hs4_set <- as.character(top_hs4$hs_code)
  excluded_iso <- c("EUR", "WLD", "W00", "ASE")

  cache_dir <- file.path(cfg[['paths']]$raw, "comtrade", "production", "detailed")
  cache_parquets <- if (dir.exists(cache_dir)) {
    list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
  } else {
    character()
  }

  reused_reporters <- character()
  invalidated <- 0L
  parsed_rows <- list()

  for (parsed_path in cache_parquets) {
    rid <- sub("\\.parquet$", "", basename(parsed_path))
    dt <- tryCatch(
      data.table::as.data.table(arrow::read_parquet(parsed_path)),
      error = function(e) NULL
    )
    if (is.null(dt) || !nrow(dt)) next
    reps <- unique(as.character(dt$reporter_code))
    isos <- if ("reporter_iso3" %in% names(dt)) unique(as.character(dt$reporter_iso3)) else character()
    if (any(isos %in% excluded_iso) || any(reps == "97")) {
      invalidated <- invalidated + 1L
      next
    }
    if (!any(reps %in% rep_set)) {
      invalidated <- invalidated + 1L
      next
    }
    if (!"request_id" %in% names(dt)) dt[, request_id := rid]
    if (!"ingested_at" %in% names(dt)) dt[, ingested_at := utc_now()]
    parsed_rows[[length(parsed_rows) + 1L]] <- dt
    reused_reporters <- c(reused_reporters, reps[reps %in% rep_set])
  }

  if (nrow(active)) {
    for (i in seq_len(nrow(active))) {
      req <- active[i]
      rid <- as.character(req$request_id)
      if (!(as.character(req$reporter_code) %in% rep_set)) {
        invalidated <- invalidated + 1L
        next
      }
      parsed_path <- file.path(dirname(req$raw_file), paste0(rid, ".parquet"))
      if (file.exists(parsed_path)) next
      raw_file <- req$raw_file
      if (!is.na(raw_file) && file.exists(raw_file)) {
        body <- paste(readLines(raw_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        parsed <- tryCatch(parse_comtrade_payload_production(body), error = function(e) NULL)
        if (!is.null(parsed)) {
          dt <- comtrade_records_to_dt_production(parsed$records)
          if (nrow(dt)) {
            dt[, `:=`(request_id = rid, ingested_at = utc_now())]
            arrow::write_parquet(dt, parsed_path)
            parsed_rows[[length(parsed_rows) + 1L]] <- dt
            reused_reporters <- c(reused_reporters, as.character(req$reporter_code))
          }
        }
      }
    }
  }

  out_path <- file.path(cfg[['paths']]$processed, "trade_detailed_top20.parquet")
  empty_schema <- function() {
    data.table::data.table(
      year = integer(), period = character(), frequency = character(),
      reporter_code = character(), reporter_iso3 = character(), reporter_name = character(),
      partner_code = character(), partner_iso3 = character(), partner_name = character(),
      flow_code = character(), flow_name = character(),
      hs_revision = character(), hs_code = character(), hs_level = integer(),
      commodity_description = character(),
      trade_value_usd = numeric(), net_weight_kg = numeric(),
      quantity = numeric(), quantity_unit = character(),
      source_updated_at = character(), ingested_at = character(), request_id = character(),
      universe_checksum = character()
    )
  }

  if (!length(parsed_rows)) {
    atomic_write_parquet_dt(empty_schema(), out_path)
    return(list(
      path = out_path, rows = 0L, reused_reporters = character(),
      represented_reporters = 0L, invalidated = invalidated,
      universe_checksum = universe_checksum
    ))
  }

  all_dt <- data.table::rbindlist(parsed_rows, fill = TRUE)
  all_dt[, year := as.integer(ref_year %||% substr(period, 1, 4))]
  all_dt[, frequency := "A"]
  all_dt[, hs_code := as.character(cmd_code)]
  all_dt[, hs_revision := as.character(classification_code %||% classification_search_code)]
  all_dt[, commodity_description := as.character(cmd_desc)]
  all_dt[, trade_value_usd := as.numeric(primary_value)]
  all_dt[, net_weight_kg := as.numeric(net_wgt)]
  all_dt[, quantity := as.numeric(qty)]
  all_dt[, quantity_unit := as.character(qty_unit)]
  all_dt[, hs_level := ifelse(is.na(aggr_level), nchar(as.character(cmd_code)), as.integer(aggr_level))]
  all_dt[, source_updated_at := NA_character_]
  all_dt[, period := as.character(year)]
  all_dt[, universe_checksum := as.character(universe_checksum)]

  detailed_dt <- all_dt[
    year %in% as.integer(years) &
      flow_code %in% c("M", "X") &
      reporter_code %in% rep_set &
      partner_code %in% par_set &
      hs_code %in% hs4_set &
      partner_iso3 != "W00" &
      nchar(hs_code) == 4 &
      substr(hs_code, 1, 2) == "85"
  ]

  required_cols <- c(
    "year", "period", "frequency",
    "reporter_code", "reporter_iso3", "reporter_name",
    "partner_code", "partner_iso3", "partner_name",
    "flow_code", "flow_name",
    "hs_revision", "hs_code", "hs_level",
    "commodity_description",
    "trade_value_usd", "net_weight_kg", "quantity", "quantity_unit",
    "source_updated_at", "ingested_at", "request_id", "universe_checksum"
  )
  for (c in required_cols) if (!c %in% names(detailed_dt)) detailed_dt[, (c) := NA]
  detailed_dt <- detailed_dt[, ..required_cols]
  if (nrow(detailed_dt)) {
    detailed_dt <- unique(detailed_dt, by = c("year", "flow_code", "reporter_code", "partner_code", "hs_code"))
  }
  atomic_write_parquet_dt(detailed_dt, out_path)

  if (nrow(active) && nrow(st) && nrow(detailed_dt)) {
    for (rid in unique(as.character(detailed_dt$request_id))) {
      if (rid %in% active$request_id && rid %in% st$request_id) {
        raw_file <- active[request_id == rid]$raw_file[1]
        if (!is.na(raw_file) && file.exists(raw_file)) {
          st <- mark_terminal(
            st, rid,
            status = "succeeded",
            result_row_count = nrow(detailed_dt[request_id == rid]),
            raw_file = raw_file,
            raw_checksum = safe_md5_file(raw_file)
          )
        }
      }
    }
    save_state(st, cfg)
  }

  list(
    path = out_path,
    rows = nrow(detailed_dt),
    reused_reporters = sort(unique(reused_reporters)),
    represented_reporters = data.table::uniqueN(detailed_dt$reporter_code),
    invalidated = invalidated,
    universe_checksum = universe_checksum
  )
}
