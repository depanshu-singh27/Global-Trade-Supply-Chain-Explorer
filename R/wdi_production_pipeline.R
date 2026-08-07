wdi_state_schema <- function() {
  data.table::data.table(
    request_id = character(),
    indicator_code = character(),
    status = character(),
    attempts = integer(),
    started_at = character(),
    completed_at = character(),
    http_status = integer(),
    result_row_count = integer(),
    pages_total = integer(),
    raw_file = character(),
    raw_checksum = character(),
    error_category = character(),
    error_message = character()
  )
}

load_wdi_state <- function(cfg = load_config()) {
  p <- wdi_state_file(cfg)
  if (!file.exists(p)) return(wdi_state_schema())
  data.table::as.data.table(arrow::read_parquet(p))
}

save_wdi_state <- function(state_dt, cfg = load_config()) {
  atomic_write_parquet_dt(state_dt, wdi_state_file(cfg))
  invisible(wdi_state_file(cfg))
}

init_wdi_state_from_plan <- function(plan_dt, existing = NULL) {
  st <- if (!is.null(existing) && nrow(existing)) {
    data.table::as.data.table(existing)
  } else {
    wdi_state_schema()
  }
  plan_dt <- data.table::as.data.table(plan_dt)
  missing <- setdiff(as.character(plan_dt$request_id), as.character(st$request_id))
  if (length(missing)) {
    add <- unique(plan_dt[request_id %in% missing, .(request_id, indicator_code)], by = "request_id")
    add[, `:=`(
      status = "planned",
      attempts = 0L,
      started_at = NA_character_,
      completed_at = NA_character_,
      http_status = NA_integer_,
      result_row_count = NA_integer_,
      pages_total = NA_integer_,
      raw_file = NA_character_,
      raw_checksum = NA_character_,
      error_category = NA_character_,
      error_message = NA_character_
    )]
    st <- data.table::rbindlist(list(st, add), fill = TRUE)
  }
  st
}

wdi_select_requests <- function(state_dt, retry_failed_only = FALSE, max_requests = Inf) {
  if (is.null(state_dt) || !nrow(state_dt)) return(data.table::data.table())
  skip <- c("succeeded", "empty", "skipped_cached", "invalid")
  elig <- state_dt[!(status %in% skip)]
  if (retry_failed_only) {
    pending <- elig[status %in% c("retryable_failed")][order(request_id)]
  } else {
    pending <- elig[status %in% c("planned", "retryable_failed")][order(request_id)]
  }
  if (is.finite(max_requests)) {
    pending <- pending[seq_len(min(nrow(pending), as.integer(max_requests)))]
  }
  pending
}

wdi_should_skip_cached <- function(state_row, req_row) {
  if (!nrow(state_row)) return(FALSE)
  if (!(state_row$status %in% c("succeeded", "empty", "skipped_cached"))) return(FALSE)
  raw_file <- state_row$raw_file %||% req_row$raw_output_path
  if (is.null(raw_file) || !nzchar(raw_file) || !file.exists(raw_file)) return(FALSE)
  if (is.na(state_row$raw_checksum) || !nzchar(state_row$raw_checksum)) return(FALSE)
  identical(safe_md5_file(raw_file), state_row$raw_checksum)
}

validate_wdi_raw_cache <- function(path) {
  if (!file.exists(path)) return(list(ok = FALSE, reason = "missing"))
  body <- tryCatch(
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(e) NULL
  )
  if (is.null(body) || !nzchar(body)) return(list(ok = FALSE, reason = "empty"))
  parsed <- tryCatch(parse_wdi_payload(body), error = function(e) NULL)
  if (is.null(parsed)) return(list(ok = FALSE, reason = "corrupt_json"))
  list(ok = TRUE, body = body, parsed = parsed)
}

execute_wdi_request <- function(cfg, req, request_delay_seconds = 0.4) {
  Sys.sleep(as.numeric(request_delay_seconds %||% 0))
  countries <- strsplit(as.character(req$requested_country_codes), ";", fixed = TRUE)[[1]]
  date_range <- sprintf("%s:%s", req$start_year, req$end_year)
  url <- wdi_indicator_url(
    cfg$wdi$base_url, countries, req$indicator_code, date_range,
    page = as.integer(req$page), per_page = as.integer(req$per_page %||% 1000L)
  )
  res <- wdi_fetch_page(url, cfg)
  list(status = res$status, body_text = res$body_text)
}

run_wdi_production_fetch <- function(cfg,
                                       plan_dt,
                                       dry_run = FALSE,
                                       max_requests = Inf,
                                       retry_failed_only = FALSE,
                                       refresh_raw = FALSE,
                                       request_delay_seconds = 0.4) {
  plan_dt <- data.table::as.data.table(plan_dt)
  if ("plan_status" %in% names(plan_dt)) {
    plan_dt <- plan_dt[is.na(plan_status) | plan_status == "active"]
  }
  ensure_dir(file.path(cfg[['paths']]$raw, "wdi", "production"))

  st <- load_wdi_state(cfg)
  st <- init_wdi_state_from_plan(plan_dt, st)
  skip_n <- 0L
  exec_n <- 0L

  if (!refresh_raw) {
    for (rid in as.character(plan_dt$request_id)) {
      row <- st[request_id == rid]
      req <- plan_dt[request_id == rid][1]
      if (nrow(row) && wdi_should_skip_cached(row, req)) {
        chk <- validate_wdi_raw_cache(req$raw_output_path)
        if (isTRUE(chk$ok)) {
          st[request_id == rid, `:=`(
            status = "skipped_cached",
            completed_at = utc_now(),
            raw_file = req$raw_output_path
          )]
          skip_n <- skip_n + 1L
        } else {
          st[request_id == rid, `:=`(
            status = "planned",
            error_category = "corrupt_cache",
            error_message = chk$reason
          )]
        }
      }
    }
    save_wdi_state(st, cfg)
  }

  to_run <- wdi_select_requests(st[request_id %in% plan_dt$request_id],
                                retry_failed_only = retry_failed_only,
                                max_requests = max_requests)
  if (!nrow(to_run) || dry_run) {
    return(list(state = st, plan = plan_dt, executed = 0L, skipped = skip_n, dry_run = dry_run))
  }

  for (i in seq_len(nrow(to_run))) {
    rid <- to_run$request_id[i]
    req <- plan_dt[request_id == rid][1]
    if (!refresh_raw && wdi_should_skip_cached(st[request_id == rid], req)) {
      skip_n <- skip_n + 1L
      next
    }

    st[request_id == rid, `:=`(
      status = "running",
      started_at = utc_now(),
      attempts = attempts + 1L,
      error_category = NA_character_,
      error_message = NA_character_
    )]
    save_wdi_state(st, cfg)

    out <- tryCatch(
      execute_wdi_request(cfg, req, request_delay_seconds = request_delay_seconds),
      error = function(e) list(status = 0L, body_text = NULL, error = e)
    )

    if (!is.null(out$error)) {
      msg <- conditionMessage(out$error)
      is_invalid <- grepl("Invalid value|WDI API error", msg, ignore.case = TRUE)
      retryable <- !is_invalid && grepl("timeout|temporar|429|5\\d\\d|timed out", msg, ignore.case = TRUE)
      st[request_id == rid, `:=`(
        status = if (is_invalid) {
          "permanently_failed"
        } else if (retryable) {
          "retryable_failed"
        } else {
          "retryable_failed"
        },
        completed_at = utc_now(),
        http_status = as.integer(out$status %||% NA_integer_),
        error_category = if (is_invalid) "invalid_country_parameter" else if (retryable) "transient" else "fetch_error",
        error_message = substr(msg, 1, 400)
      )]
      save_wdi_state(st, cfg)
      next
    }

    http_status <- as.integer(out$status)
    if (is.na(http_status) || http_status < 200 || http_status >= 300) {
      retryable <- !is.na(http_status) && http_status %in% c(429L, 500L, 502L, 503L, 504L)
      st[request_id == rid, `:=`(
        status = if (retryable) "retryable_failed" else "permanently_failed",
        completed_at = utc_now(),
        http_status = http_status,
        error_category = "http_error",
        error_message = paste0("HTTP ", http_status)
      )]
      save_wdi_state(st, cfg)
      next
    }

    ensure_dir(dirname(req$raw_output_path))
    writeLines(out$body_text, req$raw_output_path, useBytes = TRUE)
    checksum <- safe_md5_file(req$raw_output_path)

    parsed <- tryCatch(parse_wdi_payload(out$body_text), error = function(e) e)
    if (inherits(parsed, "error")) {
      msg <- conditionMessage(parsed)
      is_invalid <- grepl("Invalid value|WDI API error", msg, ignore.case = TRUE)
      st[request_id == rid, `:=`(
        status = if (is_invalid) "permanently_failed" else "retryable_failed",
        completed_at = utc_now(),
        http_status = http_status,
        raw_file = req$raw_output_path,
        raw_checksum = checksum,
        error_category = if (is_invalid) "invalid_country_parameter" else "malformed_json",
        error_message = substr(msg, 1, 400)
      )]
      save_wdi_state(st, cfg)
      next
    }

    n_rec <- length(parsed$records)
    pages_total <- as.integer(parsed$pages %||% 1L)

    if (!is.na(pages_total) && pages_total > 1L && as.integer(req$page) == 1L) {
      plan_dt <- append_wdi_pagination_requests(plan_dt, req, pages_total, cfg)
      st <- init_wdi_state_from_plan(plan_dt, st)
      atomic_write_parquet_dt(plan_dt, wdi_plan_file(cfg))
    }

    st[request_id == rid, `:=`(
      status = if (n_rec == 0L) "empty" else "succeeded",
      completed_at = utc_now(),
      http_status = http_status,
      result_row_count = as.integer(n_rec),
      pages_total = pages_total,
      raw_file = req$raw_output_path,
      raw_checksum = checksum,
      error_category = NA_character_,
      error_message = NA_character_
    )]
    save_wdi_state(st, cfg)
    exec_n <- exec_n + 1L
  }

  list(state = st, plan = plan_dt, executed = exec_n, skipped = skip_n, dry_run = FALSE)
}

assemble_wdi_production_long <- function(cfg, plan_dt = NULL, allowed_iso3 = NULL) {
  if (is.null(plan_dt)) {
    if (!file.exists(wdi_plan_file(cfg))) stop("Missing WDI request plan.", call. = FALSE)
    plan_dt <- data.table::as.data.table(arrow::read_parquet(wdi_plan_file(cfg)))
  }
  st <- load_wdi_state(cfg)
  ok <- st[status %in% c("succeeded", "skipped_cached", "empty")]
  rows <- list()
  conflicts <- list()

  for (rid in as.character(ok$request_id)) {
    req <- plan_dt[request_id == rid]
    if (!nrow(req)) next
    req <- req[1]
    raw_path <- req$raw_output_path %||% ok[request_id == rid]$raw_file[1]
    chk <- validate_wdi_raw_cache(raw_path)
    if (!isTRUE(chk$ok)) next
    dt <- wdi_records_to_dt(chk$parsed$records, req$indicator_code, req$indicator_name)
    if (!nrow(dt)) next
    dt[, `:=`(
      world_bank_code = iso3,
      request_id = rid,
      ingested_at = utc_now(),
      source_updated_at = utc_now()
    )]

    if (!is.null(allowed_iso3)) {
      dt <- dt[iso3 %in% as.character(allowed_iso3)]
    }
    rows[[length(rows) + 1L]] <- dt
  }

  if (!length(rows)) {
    return(list(
      long = data.table::data.table(
        iso3 = character(), country_name = character(), world_bank_code = character(),
        year = integer(), indicator_code = character(), indicator_name = character(),
        value = numeric(), source_updated_at = character(), ingested_at = character(),
        request_id = character()
      ),
      conflicts = data.table::data.table(),
      duplicate_exact = 0L
    ))
  }

  long <- data.table::rbindlist(rows, fill = TRUE)
  long <- long[!is.na(iso3) & nzchar(iso3) & !is.na(year) & !is.na(indicator_code)]
  long[, year := as.integer(year)]
  long[, value := suppressWarnings(as.numeric(value))]

  before <- nrow(long)
  long <- unique(long, by = c("iso3", "year", "indicator_code", "value", "request_id"))
  exact_dups_removed <- before - nrow(long)

  key_n <- long[, .(n_val = data.table::uniqueN(value, na.rm = FALSE)),
                by = .(iso3, year, indicator_code)]
  conflict_keys <- key_n[n_val > 1L]
  if (nrow(conflict_keys)) {
    conflicts <- long[conflict_keys, on = .(iso3, year, indicator_code)]
  } else {
    conflicts <- data.table::data.table()
  }

  data.table::setorderv(long, c("iso3", "year", "indicator_code", "ingested_at"))
  long <- unique(long, by = c("iso3", "year", "indicator_code"), fromLast = TRUE)

  long <- long[, .(
    iso3, country_name, world_bank_code, year, indicator_code, indicator_name,
    value, source_updated_at, ingested_at, request_id
  )]
  list(long = long, conflicts = conflicts, duplicate_exact = exact_dups_removed)
}

wdi_long_to_wide_production <- function(long_dt) {
  dt <- data.table::as.data.table(long_dt)
  if (!nrow(dt)) {
    return(data.table::data.table(
      iso3 = character(), country_name = character(), year = integer(),
      gdp_current_usd = numeric(), population_total = numeric(),
      cpi_index = numeric(), inflation_annual_pct = numeric(),
      gdp_per_capita_usd = numeric(),
      source_updated_at = character(), ingested_at = character()
    ))
  }
  names_map <- c(
    "NY.GDP.MKTP.CD" = "gdp_current_usd",
    "SP.POP.TOTL" = "population_total",
    "FP.CPI.TOTL" = "cpi_index",
    "FP.CPI.TOTL.ZG" = "inflation_annual_pct"
  )
  dt[, field := names_map[indicator_code]]
  dt <- dt[!is.na(field)]
  wide <- data.table::dcast(
    dt,
    iso3 + country_name + year ~ field,
    value.var = "value"
  )
  meta <- dt[, .(
    source_updated_at = max(as.character(source_updated_at), na.rm = TRUE),
    ingested_at = max(as.character(ingested_at), na.rm = TRUE)
  ), by = .(iso3, year)]
  wide <- meta[wide, on = .(iso3, year)]

  for (f in c("gdp_current_usd", "population_total", "cpi_index", "inflation_annual_pct")) {
    if (!f %in% names(wide)) wide[, (f) := NA_real_]
  }
  wide[, gdp_per_capita_usd := data.table::fifelse(
    !is.na(gdp_current_usd) & !is.na(population_total) & population_total > 0,
    gdp_current_usd / population_total,
    NA_real_
  )]
  wide[, .(
    iso3, country_name, year,
    gdp_current_usd, population_total, cpi_index, inflation_annual_pct,
    gdp_per_capita_usd, source_updated_at, ingested_at
  )]
}
