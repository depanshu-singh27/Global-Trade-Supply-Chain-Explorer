wdi_indicator_url <- function(base_url, countries, indicator, date_range,
                              page = 1L, per_page = 1000L) {
  sprintf(
    "%s/country/%s/indicator/%s?format=json&date=%s&page=%d&per_page=%d",
    sub("/$", "", base_url),
    paste(countries, collapse = ";"),
    indicator,
    date_range,
    page,
    per_page
  )
}

wdi_fetch_page <- function(url, cfg) {

  timeout_sec <- max(as.numeric(cfg$api$timeout_seconds), 120)
  req <- httr2::request(url) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(
      max_tries = max(cfg$api$max_retries + 2L, 5L),
      backoff = function(i) cfg$api$retry_backoff_seconds * (2^(i - 1)),
      retry_on_failure = TRUE,
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
      }
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  body_text <- httr2::resp_body_string(resp)
  list(status = status, body_text = body_text, url = url)
}

parse_wdi_payload <- function(body_text) {
  parsed <- tryCatch(
    jsonlite::fromJSON(body_text, simplifyVector = FALSE),
    error = function(e) stop("WDI JSON parse failed: ", e$message, call. = FALSE)
  )
  if (!is.list(parsed) || length(parsed) < 1L) {
    stop("Unexpected WDI response structure.", call. = FALSE)
  }

  if (length(parsed) == 1L && !is.null(parsed[[1]]$message)) {
    msg <- tryCatch(parsed[[1]]$message[[1]]$value %||% "WDI API message error",
                    error = function(e) "WDI API message error")
    stop("WDI API error: ", msg, call. = FALSE)
  }
  if (length(parsed) < 2L) {
    stop("Unexpected WDI response structure.", call. = FALSE)
  }
  meta <- parsed[[1]]
  data <- parsed[[2]] %||% list()
  list(meta = meta, records = data, pages = as.integer(meta$pages %||% 1L))
}

wdi_records_to_dt <- function(records, indicator_code, indicator_name) {
  if (!length(records)) return(data.table::data.table())
  rows <- lapply(records, function(r) {
    list(
      iso3 = pluck_chr(r$countryiso3code %||% list_get(r, "country", "id")),
      country_name = pluck_chr(list_get(r, "country", "value") %||% r$country),
      year = suppressWarnings(as.integer(pluck_chr(r$date))),
      indicator_code = indicator_code,
      indicator_name = indicator_name,
      value = pluck_num(r$value),
      source_updated_at = pluck_chr(list_get(r, "indicator", "id"))
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

fetch_wdi_pilot <- function(cfg = load_config()) {
  ensure_data_dirs(cfg)
  countries <- unique(c(cfg$pilot$reporters, cfg$pilot$partners))
  date_range <- sprintf("%s:%s", cfg$pilot$start_year, cfg$pilot$end_year)
  out_dir <- file.path(cfg[['paths']]$raw, "wdi")
  ensure_dir(out_dir)

  indicators <- cfg$wdi$indicators
  all_rows <- list()
  manifest <- list(requests = list(), updated_at = utc_now())

  for (ind in indicators) {
    code <- ind$code
    name <- ind$name
    page <- 1L
    total_pages <- 1L
    while (page <= total_pages) {
      req_id <- sprintf("wdi_%s_%s_p%d", code, gsub(":", "-", date_range), page)
      raw_path <- file.path(out_dir, paste0(req_id, ".json"))
      url <- wdi_indicator_url(cfg$wdi$base_url, countries, code, date_range, page)

      if (file.exists(raw_path)) {
        log_msg("Using cached WDI raw: ", basename(raw_path))
        body_text <- paste(readLines(raw_path, warn = FALSE, encoding = "UTF-8"),
                           collapse = "\n")
        status <- 200L
        from_cache <- TRUE
      } else {
        log_msg("WDI request indicator=", code, " page=", page)
        res <- wdi_fetch_page(url, cfg)
        status <- res$status
        body_text <- res$body_text
        from_cache <- FALSE
        if (status >= 200 && status < 300) {
          writeLines(body_text, raw_path, useBytes = TRUE)
        } else {
          stop(sprintf("WDI HTTP %s for indicator=%s page=%s", status, code, page),
               call. = FALSE)
        }
        Sys.sleep(0.3)
      }

      parsed <- parse_wdi_payload(body_text)
      total_pages <- max(1L, parsed$pages %||% 1L)
      dt <- wdi_records_to_dt(parsed$records, code, name)
      if (nrow(dt)) {
        dt[, `:=`(ingested_at = utc_now(), source_updated_at = utc_now())]

        dt <- dt[!is.na(iso3) & nzchar(iso3)]
        all_rows[[length(all_rows) + 1L]] <- dt
      }

      manifest$requests[[length(manifest$requests) + 1L]] <- list(
        request_id = req_id,
        endpoint = "api.worldbank.org/v2/country/.../indicator/...",
        indicator = code,
        page = page,
        pages = total_pages,
        http_status = status,
        from_cache = from_cache,
        record_count = length(parsed$records),
        raw_file = basename(raw_path),
        ingested_at = utc_now()
      )
      page <- page + 1L
    }
  }

  write_json_atomic(manifest, file.path(out_dir, "request_manifest.json"))

  long_dt <- if (length(all_rows)) {
    data.table::rbindlist(all_rows, fill = TRUE)
  } else {
    data.table::data.table()
  }

  interim_path <- file.path(cfg[['paths']]$interim, "wdi_pilot_raw_long.parquet")
  if (nrow(long_dt)) arrow::write_parquet(long_dt, interim_path)

  list(long = long_dt, n_rows = nrow(long_dt), interim_path = interim_path)
}

fetch_wdi_country_reference <- function(cfg = load_config(), refresh = FALSE) {
  ensure_data_dirs(cfg)
  path <- file.path(cfg[['paths']]$reference, "wdi_countries.parquet")
  raw_path <- file.path(cfg[['paths']]$raw, "wdi", "reference", "countries.json")
  if (!refresh && file.exists(path)) {
    return(data.table::as.data.table(arrow::read_parquet(path)))
  }
  ensure_dir(dirname(raw_path))
  url <- paste0(sub("/$", "", cfg$wdi$base_url), "/country?format=json&per_page=400")
  if (!refresh && file.exists(raw_path)) {
    body <- paste(readLines(raw_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    res <- wdi_fetch_page(url, cfg)
    if (res$status < 200 || res$status >= 300) {
      stop(sprintf("WDI country reference HTTP %s", res$status), call. = FALSE)
    }
    body <- res$body_text
    writeLines(body, raw_path, useBytes = TRUE)
  }
  parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  recs <- parsed[[2]] %||% list()
  dt <- data.table::rbindlist(lapply(recs, function(r) {
    list(
      world_bank_code = pluck_chr(r$id),
      iso2 = pluck_chr(r$iso2Code),
      country_name = pluck_chr(r$name),
      region = pluck_chr(list_get(r, "region", "value")),
      income_level = pluck_chr(list_get(r, "incomeLevel", "value")),
      capital_city = pluck_chr(r$capitalCity),
      is_aggregate = grepl("Aggregat", pluck_chr(list_get(r, "region", "value")),
                           ignore.case = TRUE)
    )
  }), fill = TRUE)
  arrow::write_parquet(dt, path)
  dt
}

filter_world_bank_iso3 <- function(iso3, wdi_countries) {
  codes <- unique(as.character(iso3))
  wb <- data.table::as.data.table(wdi_countries)
  valid <- wb[is_aggregate == FALSE & nchar(world_bank_code) == 3L]$world_bank_code
  list(
    matched = sort(intersect(codes, valid)),
    unmatched = sort(setdiff(codes, wb$world_bank_code)),
    unmatched_aggregate = sort(intersect(codes, wb[is_aggregate == TRUE]$world_bank_code))
  )
}

fetch_wdi_country_reference <- function(cfg = load_config(), refresh = FALSE) {
  ensure_data_dirs(cfg)
  path <- file.path(cfg[['paths']]$reference, "wdi_countries.parquet")
  raw_path <- file.path(cfg[['paths']]$raw, "wdi", "reference", "countries.json")
  if (!refresh && file.exists(path)) {
    return(data.table::as.data.table(arrow::read_parquet(path)))
  }
  ensure_dir(dirname(raw_path))
  url <- paste0(sub("/$", "", cfg$wdi$base_url), "/country?format=json&per_page=400")
  if (!refresh && file.exists(raw_path)) {
    body <- paste(readLines(raw_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    res <- wdi_fetch_page(url, cfg)
    if (res$status < 200 || res$status >= 300) {
      stop(sprintf("WDI country reference HTTP %s", res$status), call. = FALSE)
    }
    body <- res$body_text
    writeLines(body, raw_path, useBytes = TRUE)
  }
  parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  recs <- parsed[[2]] %||% list()
  dt <- data.table::rbindlist(lapply(recs, function(r) {
    list(
      world_bank_code = pluck_chr(r$id),
      iso2 = pluck_chr(r$iso2Code),
      country_name = pluck_chr(r$name),
      region = pluck_chr(list_get(r, "region", "value")),
      income_level = pluck_chr(list_get(r, "incomeLevel", "value")),
      capital_city = pluck_chr(r$capitalCity),
      is_aggregate = grepl("Aggregat", pluck_chr(list_get(r, "region", "value")),
                           ignore.case = TRUE)
    )
  }), fill = TRUE)
  arrow::write_parquet(dt, path)
  dt
}

filter_world_bank_iso3 <- function(iso3, wdi_countries) {
  codes <- unique(as.character(iso3))
  wb <- data.table::as.data.table(wdi_countries)
  valid <- wb[is_aggregate == FALSE & nchar(as.character(world_bank_code)) == 3L]$world_bank_code
  list(
    matched = sort(intersect(codes, valid)),
    unmatched = sort(setdiff(codes, as.character(wb$world_bank_code))),
    unmatched_aggregate = sort(intersect(codes, wb[is_aggregate == TRUE]$world_bank_code))
  )
}
