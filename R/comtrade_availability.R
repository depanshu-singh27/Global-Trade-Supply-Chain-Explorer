comtrade_da_base_url <- function(cfg = load_config()) {
  sub("/get$", "/getDA", cfg$comtrade$base_url)
}

parse_availability_payload <- function(body_text) {
  parsed <- tryCatch(
    jsonlite::fromJSON(body_text, simplifyVector = FALSE),
    error = function(e) stop("Availability JSON parse failed: ", e$message, call. = FALSE)
  )
  recs <- parsed$data %||% parsed$results %||% list()

  if (is.list(recs) && length(recs) == 1L && is.null(names(recs[[1]])) &&
      length(recs[[1]]) > 0L && is.list(recs[[1]][[1]])) {
    recs <- recs[[1]]
  }

  if (length(recs) > 0L && is.list(recs[[1]]) && is.null(recs[[1]]$reporterCode) &&
      !is.null(recs[[1]][[1]]$reporterCode)) {
    recs <- unlist(recs, recursive = FALSE)
  }
  if (!length(recs)) {
    return(data.table::data.table(
      reporter_code = character(), reporter_iso3 = character(), reporter_name = character(),
      year = integer(), classification = character(), classification_search = character(),
      total_records = integer(), first_released = character(), last_released = character(),
      availability_status = character()
    ))
  }

  rows <- lapply(recs, function(r) {
    if (!is.list(r) || is.null(r$reporterCode)) return(NULL)
    period <- pluck_chr(r$period)
    list(
      reporter_code = as.character(r$reporterCode),
      reporter_iso3 = pluck_chr(r$reporterISO),
      reporter_name = pluck_chr(r$reporterDesc),
      year = suppressWarnings(as.integer(substr(period, 1, 4))),
      classification = pluck_chr(r$classificationCode),
      classification_search = pluck_chr(r$classificationSearchCode),
      total_records = suppressWarnings(as.integer(r$totalRecords %||% NA)),
      first_released = pluck_chr(r$firstReleased),
      last_released = pluck_chr(r$lastReleased),
      availability_status = "available"
    )
  })
  rows <- Filter(Negate(is.null), rows)
  data.table::rbindlist(rows, fill = TRUE)
}

fetch_comtrade_availability_year <- function(cfg, classification, year, refresh = FALSE) {
  ensure_data_dirs(cfg)
  raw_dir <- file.path(cfg[['paths']]$raw, "reference", "availability")
  ensure_dir(raw_dir)
  raw_path <- file.path(raw_dir, sprintf("da_%s_%s.json", classification, year))

  if (!refresh && file.exists(raw_path)) {
    body <- paste(readLines(raw_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    status <- 200L
  } else {
    key <- comtrade_subscription_key()
    url_path <- paste(comtrade_da_base_url(cfg), "C", "A", classification, sep = "/")
    req <- httr2::request(url_path) |>
      httr2::req_headers(`Ocp-Apim-Subscription-Key` = key) |>
      httr2::req_url_query(period = as.character(year), format = "JSON") |>
      httr2::req_timeout(cfg$api$timeout_seconds %||% 60) |>
      httr2::req_error(is_error = function(resp) FALSE)
    log_msg(sprintf("Comtrade availability classification=%s year=%s", classification, year))
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    body <- httr2::resp_body_string(resp)
    if (status < 200 || status >= 300) {
      stop(sprintf("Availability HTTP %s for classification=%s year=%s",
                   status, classification, year), call. = FALSE)
    }
    writeLines(body, raw_path, useBytes = TRUE)
    Sys.sleep(0.8)
  }

  dt <- parse_availability_payload(body)
  if (nrow(dt)) {
    dt[, `:=`(
      request_id = sprintf("da_%s_%s", classification, year),
      source_http_status = as.integer(status)
    )]
  }
  dt
}

fetch_comtrade_availability <- function(cfg = load_config(),
                                          classification = "HS",
                                          years = 2019:2024,
                                          refresh = FALSE) {
  parts <- lapply(as.integer(years), function(y) {
    fetch_comtrade_availability_year(cfg, classification, y, refresh = refresh)
  })
  dt <- data.table::rbindlist(parts, fill = TRUE)
  out_path <- file.path(cfg[['paths']]$interim, "comtrade_availability.parquet")
  ensure_dir(dirname(out_path))
  arrow::write_parquet(dt, out_path)
  dt
}

intersect_reporters_with_availability <- function(eligible_reporters, availability_dt,
                                                    years = 2019:2024) {
  elig <- data.table::as.data.table(eligible_reporters)
  avail <- data.table::as.data.table(availability_dt)
  years <- as.integer(years)

  elig_codes <- unique(as.character(elig$reporter_code))
  avail <- avail[year %in% years & reporter_code %in% elig_codes]

  reporter_years <- unique(avail[, .(reporter_code, year, reporter_iso3, reporter_name)])
  coverage <- avail[, .(n_years = data.table::uniqueN(year)), by = .(reporter_code, reporter_iso3)]
  coverage[, has_all_years := n_years == length(years)]

  list(
    reporter_years = reporter_years,
    coverage_by_reporter = coverage,
    coverage_by_year = avail[, .(n_reporters = data.table::uniqueN(reporter_code)), by = year][order(year)]
  )
}

choose_ranking_year <- function(coverage_by_year, prefer = 2024L, min_share_of_max = 0.85) {
  cov <- data.table::as.data.table(coverage_by_year)
  if (!nrow(cov)) stop("No availability coverage to choose ranking year.", call. = FALSE)
  max_n <- max(cov$n_reporters, na.rm = TRUE)
  preferred <- cov[year == as.integer(prefer)]
  if (nrow(preferred) && preferred$n_reporters[1] >= min_share_of_max * max_n) {
    return(list(year = as.integer(prefer), n_reporters = preferred$n_reporters[1],
                rationale = sprintf(
                  "%s preferred and coverage=%s is >= %.0f%% of max coverage=%s",
                  prefer, preferred$n_reporters[1], 100 * min_share_of_max, max_n
                )))
  }

  adequate <- cov[n_reporters >= min_share_of_max * max_n]
  if (nrow(adequate)) {
    best <- adequate[order(-year)][1]
    return(list(
      year = as.integer(best$year),
      n_reporters = as.integer(best$n_reporters),
      rationale = sprintf(
        "%s preferred coverage inadequate (n=%s); selected latest adequate year %s (n=%s, max=%s, threshold=%.0f%%)",
        prefer,
        if (nrow(preferred)) preferred$n_reporters[1] else 0L,
        best$year, best$n_reporters, max_n, 100 * min_share_of_max
      )
    ))
  }
  best <- cov[order(-n_reporters, -year)][1]
  list(
    year = as.integer(best$year),
    n_reporters = as.integer(best$n_reporters),
    rationale = sprintf(
      "No year met adequacy threshold; selected highest-coverage year %s (n=%s, max=%s)",
      best$year, best$n_reporters, max_n
    )
  )
}
