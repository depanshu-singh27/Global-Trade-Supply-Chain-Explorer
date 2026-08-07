comtrade_subscription_key <- function() {
  if (!comtrade_key_present()) {
    stop(
      "COMTRADE_PRIMARY is missing. Copy .Renviron.example to .Renviron and set your key.",
      call. = FALSE
    )
  }
  Sys.getenv("COMTRADE_PRIMARY")
}

comtrade_safe_request_id <- function(reporter_code, partner_codes, year,
                                     flow_code, cmd_code) {
  partners <- paste(sort(unique(as.character(partner_codes))), collapse = "-")
  sprintf(
    "comtrade_C_A_HS_r%s_p%s_y%s_f%s_c%s",
    reporter_code, partners, year, flow_code, cmd_code
  )
}

comtrade_fetch_once <- function(cfg, reporter_code, partner_codes, year,
                                flow_code, cmd_code = "85",
                                classification = NULL,
                                omit_partner = FALSE) {
  key <- comtrade_subscription_key()
  base <- cfg$comtrade$base_url
  type <- cfg$comtrade$type_code
  freq <- cfg$pilot$frequency %||% "A"
  cl <- classification %||% cfg$comtrade$classification %||% "HS"

  reporter_code <- as.character(reporter_code)
  if (identical(reporter_code, "0") || !nzchar(reporter_code)) {
    stop("Invalid reporter_code for final-data request (reporterCode=0 is not allowed).",
         call. = FALSE)
  }

  url_path <- paste(base, type, freq, cl, sep = "/")
  period <- if (length(year) > 1L) {
    paste(unique(as.character(year)), collapse = ",")
  } else {
    as.character(year)
  }
  flows <- if (length(flow_code) > 1L) {
    paste(unique(as.character(flow_code)), collapse = ",")
  } else {
    as.character(flow_code)
  }

  query <- list(
    reporterCode = reporter_code,
    period = period,
    cmdCode = as.character(cmd_code),
    flowCode = flows,
    partner2Code = "0",
    customsCode = "C00",
    motCode = "0",
    maxRecords = 50000,
    format = "JSON",
    includeDesc = "true"
  )
  partners <- NA_character_
  if (!isTRUE(omit_partner)) {
    partners <- paste(unique(as.character(partner_codes)), collapse = ",")
    query$partnerCode <- partners
  }

  timeout_sec <- max(as.numeric(cfg$api$timeout_seconds %||% 60), 180)

  req <- httr2::request(url_path) |>
    httr2::req_headers(`Ocp-Apim-Subscription-Key` = key)
  req <- do.call(httr2::req_url_query, c(list(req), query))
  req <- req |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(
      max_tries = cfg$api$max_retries + 1L,
      backoff = function(i) cfg$api$retry_backoff_seconds * (2^(i - 1)),
      is_transient = function(resp) {
        status <- httr2::resp_status(resp)
        status %in% c(429, 500, 502, 503, 504)
      }
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  log_msg(sprintf(
    "Comtrade request classification=%s reporter=%s period=%s flow=%s partners=%s cmd=%s",
    cl, reporter_code, period, flows,
    if (isTRUE(omit_partner)) "OMITTED" else partners,
    cmd_code
  ))

  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  body_text <- httr2::resp_body_string(resp)

  list(
    status = status,
    body_text = body_text,
    request_meta = list(
      endpoint = sprintf("comtradeapi.un.org/data/v1/get/C/A/%s", cl),
      classification = as.character(cl),
      reporter_code = reporter_code,
      partner_codes = if (isTRUE(omit_partner)) NA_character_ else as.character(partner_codes),
      period = period,
      flow_code = flows,
      cmd_code = as.character(cmd_code),
      http_status = status,
      requested_at = utc_now()
    )
  )
}

parse_comtrade_payload <- function(body_text) {
  parsed <- tryCatch(
    jsonlite::fromJSON(body_text, simplifyVector = FALSE),
    error = function(e) stop("Comtrade JSON parse failed: ", e$message, call. = FALSE)
  )
  if (!is.null(parsed$statusCode) && as.integer(parsed$statusCode) >= 400) {
    stop(
      sprintf(
        "Comtrade API statusCode=%s message=%s",
        parsed$statusCode,
        parsed$message %||% parsed$error %||% "unknown"
      ),
      call. = FALSE
    )
  }

  records <- parsed$data %||% parsed$dataset %||% list()
  if (is.data.frame(records)) {
    records <- jsonlite::fromJSON(
      jsonlite::toJSON(records, auto_unbox = TRUE),
      simplifyVector = FALSE
    )
  }
  list(raw = parsed, records = records, count = length(records))
}

comtrade_records_to_dt <- function(records) {
  if (!length(records)) {
    return(data.table::data.table())
  }
  rows <- lapply(records, function(r) {
    list(
      period = pluck_chr(r$period %||% r$Period),
      reporter_code = pluck_chr(r$reporterCode %||% r$ReporterCode),
      reporter_desc = pluck_chr(r$reporterDesc %||% r$ReporterDesc),
      partner_code = pluck_chr(r$partnerCode %||% r$PartnerCode),
      partner_desc = pluck_chr(r$partnerDesc %||% r$PartnerDesc),
      flow_code = pluck_chr(r$flowCode %||% r$FlowCode),
      flow_desc = pluck_chr(r$flowDesc %||% r$FlowDesc),
      cmd_code = pluck_chr(r$cmdCode %||% r$CmdCode),
      cmd_desc = pluck_chr(r$cmdDesc %||% r$CmdDesc),
      customs_code = pluck_chr(r$customsCode %||% r$CustomsCode),
      mot_code = pluck_chr(r$motCode %||% r$MotCode),
      primary_value = pluck_num(r$primaryValue %||% r$PrimaryValue),
      net_wgt = pluck_num(r$netWgt %||% r$NetWgt),
      qty = pluck_num(r$qty %||% r$Qty),
      qty_unit = pluck_chr(r$qtyUnitAbbr %||% r$QtyUnitAbbr %||% r$qtyUnitCode),
      ref_year = pluck_num(r$refYear %||% r$RefYear),
      ref_month = pluck_num(r$refMonth %||% r$RefMonth),
      is_leaf = pluck_chr(r$isLeaf %||% r$IsLeaf),
      aggr_level = pluck_num(r$aggrLevel %||% r$AggrLevel)
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

parse_comtrade_payload_production <- function(body_text) {
  parsed <- tryCatch(
    jsonlite::fromJSON(body_text, simplifyVector = FALSE),
    error = function(e) stop("Comtrade JSON parse failed: ", e$message, call. = FALSE)
  )
  if (!is.null(parsed$statusCode) && as.integer(parsed$statusCode) >= 400) {
    stop(
      sprintf(
        "Comtrade API statusCode=%s message=%s",
        parsed$statusCode,
        parsed$message %||% parsed$error %||% "unknown"
      ),
      call. = FALSE
    )
  }

  records <- parsed$data %||% parsed$dataset %||% list()
  if (is.data.frame(records)) {
    records <- jsonlite::fromJSON(
      jsonlite::toJSON(records, auto_unbox = TRUE),
      simplifyVector = FALSE
    )
  }

  returned_count <- length(records)
  total_count <- suppressWarnings(as.integer(parsed$count %||% returned_count))
  is_truncated <- !is.na(total_count) && total_count > returned_count

  list(
    raw = parsed,
    records = records,
    returned_count = returned_count,
    total_count = total_count,
    is_truncated = is_truncated
  )
}

comtrade_records_to_dt_production <- function(records) {
  if (!length(records)) return(data.table::data.table())
  rows <- lapply(records, function(r) {
    list(
      period = pluck_chr(r$period %||% r$Period),
      ref_year = pluck_num(r$refYear %||% r$RefYear),
      reporter_code = pluck_chr(r$reporterCode %||% r$ReporterCode),
      reporter_iso3 = pluck_chr(r$reporterISO %||% r$ReporterISO),
      reporter_name = pluck_chr(r$reporterDesc %||% r$ReporterDesc),
      flow_code = pluck_chr(r$flowCode %||% r$FlowCode),
      flow_name = pluck_chr(r$flowDesc %||% r$FlowDesc),
      partner_code = pluck_chr(r$partnerCode %||% r$PartnerCode),
      partner_iso3 = pluck_chr(r$partnerISO %||% r$PartnerISO),
      partner_name = pluck_chr(r$partnerDesc %||% r$PartnerDesc),
      partner2_code = pluck_chr(r$partner2Code %||% r$Partner2Code),
      partner2_iso3 = pluck_chr(r$partner2ISO %||% r$Partner2ISO),
      partner2_name = pluck_chr(r$partner2Desc %||% r$Partner2Desc),
      classification_search_code = pluck_chr(r$classificationSearchCode %||% r$ClassificationSearchCode),
      classification_code = pluck_chr(r$classificationCode %||% r$ClassificationCode),
      cmd_code = pluck_chr(r$cmdCode %||% r$CmdCode),
      cmd_desc = pluck_chr(r$cmdDesc %||% r$CmdDesc),
      aggr_level = pluck_num(r$aggrLevel %||% r$AggrLevel),
      is_leaf = pluck_chr(r$isLeaf %||% r$IsLeaf),
      primary_value = pluck_num(r$primaryValue %||% r$PrimaryValue),
      net_wgt = pluck_num(r$netWgt %||% r$NetWgt),
      qty = pluck_num(r$qty %||% r$Qty),
      qty_unit = pluck_chr(r$qtyUnitAbbr %||% r$QtyUnitAbbr %||% r$qtyUnitCode),
      customs_code = pluck_chr(r$customsCode %||% r$CustomsCode),
      mot_code = pluck_chr(r$motCode %||% r$MotCode),
      is_reported = pluck_chr(r$isReported %||% r$IsReported),
      is_aggregate = pluck_chr(r$isAggregate %||% r$IsAggregate)
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

fetch_comtrade_pilot <- function(cfg = load_config()) {
  ensure_data_dirs(cfg)
  countries <- build_country_reference()
  reporters <- countries[iso3 %in% cfg$pilot$reporters]
  partners <- countries[iso3 %in% cfg$pilot$partners]
  years <- seq.int(cfg$pilot$start_year, cfg$pilot$end_year)
  flows <- cfg$pilot$flows
  cmd <- as.character(cfg$pilot$hs_chapter)

  out_dir <- file.path(cfg[['paths']]$raw, "comtrade")
  ensure_dir(out_dir)
  manifest_path <- file.path(out_dir, "request_manifest.json")
  manifest <- if (file.exists(manifest_path)) {
    safe_read_json(manifest_path) %||% list(requests = list())
  } else {
    list(requests = list())
  }

  all_rows <- list()
  idx <- 0L

  for (ri in seq_len(nrow(reporters))) {
    rep_code <- reporters$comtrade_code[ri]
    for (year in years) {
      for (flow in flows) {
        req_id <- comtrade_safe_request_id(
          rep_code, partners$comtrade_code, year, flow, cmd
        )
        raw_path <- file.path(out_dir, paste0(req_id, ".json"))
        idx <- idx + 1L

        if (file.exists(raw_path)) {
          log_msg("Using cached Comtrade raw: ", basename(raw_path))
          body_text <- paste(readLines(raw_path, warn = FALSE, encoding = "UTF-8"),
                             collapse = "\n")
          status <- 200L
          from_cache <- TRUE
        } else {
          res <- comtrade_fetch_once(
            cfg, rep_code, partners$comtrade_code, year, flow, cmd
          )
          status <- res$status
          body_text <- res$body_text
          from_cache <- FALSE
          if (status >= 200 && status < 300) {
            writeLines(body_text, raw_path, useBytes = TRUE)
          } else {
            err_path <- file.path(out_dir, paste0(req_id, "_error.json"))
            writeLines(body_text, err_path, useBytes = TRUE)
            stop(sprintf(
              "Comtrade HTTP %s for reporter=%s year=%s flow=%s (see %s). Body snippet omitted for safety.",
              status, rep_code, year, flow, basename(err_path)
            ), call. = FALSE)
          }
          Sys.sleep(1.1)
        }

        parsed <- parse_comtrade_payload(body_text)
        dt <- comtrade_records_to_dt(parsed$records)
        if (nrow(dt)) {
          dt[, `:=`(
            request_id = req_id,
            ingested_at = utc_now(),
            from_cache = from_cache
          )]
          all_rows[[length(all_rows) + 1L]] <- dt
        }

        manifest$requests[[length(manifest$requests) + 1L]] <- list(
          request_id = req_id,
          endpoint = "comtradeapi.un.org/data/v1/get/C/A/HS",
          reporter_code = rep_code,
          partner_codes = partners$comtrade_code,
          year = year,
          flow_code = flow,
          cmd_code = cmd,
          http_status = status,
          from_cache = from_cache,
          record_count = parsed$count,
          raw_file = basename(raw_path),
          ingested_at = utc_now()
        )
      }
    }
  }

  manifest$updated_at <- utc_now()
  manifest$key_present <- comtrade_key_present()
  write_json_atomic(manifest, manifest_path)

  trade <- if (length(all_rows)) {
    data.table::rbindlist(all_rows, fill = TRUE)
  } else {
    data.table::data.table()
  }

  interim_path <- file.path(cfg[['paths']]$interim, "comtrade_pilot_raw_rows.parquet")
  if (nrow(trade)) {
    arrow::write_parquet(trade, interim_path)
  }

  list(
    trade = trade,
    n_requests = length(manifest$requests),
    n_rows = nrow(trade),
    interim_path = interim_path,
    manifest_path = manifest_path
  )
}
