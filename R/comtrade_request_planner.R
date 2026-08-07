safe_code_list_string <- function(codes) {
  if (is.null(codes) || length(codes) == 0L) return("EMPTY")
  codes <- unique(as.character(codes))
  codes <- sort(codes)
  paste0(codes, collapse = "-")
}

sha256_short <- function(x, n = 16L) {
  if (requireNamespace("openssl", quietly = TRUE)) {
    h <- openssl::sha256(charToRaw(as.character(x)[1]))
    return(substr(paste(as.character(h), collapse = ""), 1L, n))
  }
  h <- tools::md5sum(charToRaw(as.character(x)[1]))
  substr(as.character(h)[1], 1L, n)
}

make_request_id <- function(dataset_type, frequency, classification,
                             period, flow_code,
                             reporter_codes, partner_codes,
                             cmd_code) {
  key <- paste(
    dataset_type, frequency, classification,
    as.character(period), as.character(flow_code), as.character(cmd_code),
    safe_code_list_string(reporter_codes),
    safe_code_list_string(partner_codes),
    sep = "|"
  )
  paste0("req_", dataset_type, "_", sha256_short(key))
}

build_global_hs85_plan <- function(cfg,
                                    reporter_codes,
                                    years = 2019:2024,
                                    frequency = "A",
                                    classification = "HS",
                                    cmd_code = "85",
                                    flow_codes = c("M", "X"),
                                    partner_code = "0",
                                    dataset_type = "trade_global_hs85_annual") {
  reporters <- sort(unique(as.character(reporter_codes)))
  reporters <- reporters[nzchar(reporters) & reporters != "0"]
  if (!length(reporters)) stop("No valid reporter codes for global plan.", call. = FALSE)

  period <- paste(as.integer(sort(unique(years))), collapse = ",")
  flows <- paste(unique(as.character(flow_codes)), collapse = ",")

  rows <- lapply(reporters, function(rc) {
    rid <- make_request_id(
      dataset_type = dataset_type,
      frequency = frequency,
      classification = classification,
      period = period,
      flow_code = flows,
      reporter_codes = rc,
      partner_codes = partner_code,
      cmd_code = cmd_code
    )
    data.table::data.table(
      request_id = rid,
      dataset_type = dataset_type,
      frequency = frequency,
      classification = classification,
      year = period,
      period = period,
      flow_code = flows,
      reporter_code = rc,
      reporter_codes_api = rc,
      partner_code = as.character(partner_code),
      partner_codes_api = as.character(partner_code),
      cmd_code = as.character(cmd_code),
      omit_partner = FALSE,
      expected_purpose = "Global reporter totals HS-85 World partner; multi-year + multi-flow batch",
      planned_at = utc_now(),
      plan_status = "active",
      raw_file = file.path(
        cfg[['paths']]$raw, "comtrade", "production", "global",
        paste0(rid, ".json")
      ),
      output_filename = file.path(
        cfg[['paths']]$processed, "trade_global_hs85_annual.parquet"
      )
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

build_ranking_year_global_plan <- function(cfg, reporter_codes, ranking_year,
                                            classification = "HS",
                                            cmd_code = "85",
                                            flow_codes = c("M", "X")) {
  build_global_hs85_plan(
    cfg = cfg,
    reporter_codes = reporter_codes,
    years = as.integer(ranking_year),
    classification = classification,
    cmd_code = cmd_code,
    flow_codes = flow_codes,
    dataset_type = "trade_global_hs85_ranking_year"
  )
}

build_detailed_top20_plan <- function(cfg,
                                       universe,
                                       years = 2019:2024,
                                       frequency = "A",
                                       classification = "HS",
                                       cmd_code = "85*",
                                       flow_codes = c("M", "X"),
                                       universe_checksum = NULL) {
  reporters <- sort(unique(as.character(universe$top_reporters$reporter_code)))
  partners <- sort(unique(as.character(universe$top_partners$partner_code)))
  hs_scope <- sort(unique(as.character(universe$top_hs4$hs_code)))
  years <- as.integer(sort(unique(years)))
  flows <- paste(unique(as.character(flow_codes)), collapse = ",")
  partner_api <- paste(partners, collapse = ",")
  if (is.null(universe_checksum) && !is.null(universe$universe_checksum)) {
    universe_checksum <- universe$universe_checksum
  }
  if (is.null(universe_checksum)) universe_checksum <- NA_character_

  grid <- data.table::CJ(reporter_code = reporters, year = years, sorted = TRUE)
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    rc <- grid$reporter_code[i]
    yr <- grid$year[i]
    rid <- make_request_id(
      dataset_type = "trade_detailed_top20",
      frequency = frequency,
      classification = classification,
      period = as.character(yr),
      flow_code = flows,
      reporter_codes = rc,
      partner_codes = partners,
      cmd_code = cmd_code
    )
    data.table::data.table(
      request_id = rid,
      dataset_type = "trade_detailed_top20",
      frequency = frequency,
      classification = classification,
      year = as.character(yr),
      period = as.character(yr),
      flow_code = flows,
      reporter_code = rc,
      reporter_codes_api = rc,
      partner_code = safe_code_list_string(partners),
      partner_codes_api = partner_api,
      cmd_code = as.character(cmd_code),
      hs4_scope = safe_code_list_string(hs_scope),
      universe_checksum = as.character(universe_checksum),
      omit_partner = FALSE,
      expected_purpose = "Detailed observed reporter-partner-HS4 via cmdCode=85* (one year per request)",
      planned_at = utc_now(),
      plan_status = "active",
      raw_file = file.path(
        cfg[['paths']]$raw, "comtrade", "production", "detailed",
        paste0(rid, ".json")
      ),
      output_filename = file.path(
        cfg[['paths']]$processed, "trade_detailed_top20.parquet"
      )
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

build_detailed_ranking_discovery_plan <- function(cfg,
                                                   top_reporter_codes,
                                                   ranking_year,
                                                   classification = "HS",
                                                   cmd_code = "85*",
                                                   flow_codes = c("M", "X")) {
  reporters <- sort(unique(as.character(top_reporter_codes)))
  period <- as.character(as.integer(ranking_year))
  flows <- paste(unique(as.character(flow_codes)), collapse = ",")

  rows <- lapply(reporters, function(rc) {
    rid <- make_request_id(
      dataset_type = "universe_partner_and_hs4_rank",
      frequency = "A",
      classification = classification,
      period = period,
      flow_code = flows,
      reporter_codes = rc,
      partner_codes = "OMIT",
      cmd_code = cmd_code
    )
    data.table::data.table(
      request_id = rid,
      dataset_type = "universe_partner_and_hs4_rank",
      frequency = "A",
      classification = classification,
      year = period,
      period = period,
      flow_code = flows,
      reporter_code = rc,
      reporter_codes_api = rc,
      partner_code = "OMIT",
      partner_codes_api = NA_character_,
      cmd_code = as.character(cmd_code),
      omit_partner = TRUE,
      expected_purpose = "Universe selection: bilateral HS4 discovery with partner omitted",
      planned_at = utc_now(),
      plan_status = "active",
      raw_file = file.path(
        cfg[['paths']]$raw, "comtrade", "production", "universe_rank",
        paste0(rid, ".json")
      ),
      output_filename = file.path(cfg[['paths']]$processed, "analytical_universe.json")
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

summarise_request_plan <- function(plan_dt, request_delay_seconds = 1.1) {
  plan_dt <- data.table::as.data.table(plan_dt)
  active <- if ("plan_status" %in% names(plan_dt)) plan_dt[plan_status == "active"] else plan_dt
  n <- nrow(active)
  list(
    planned_at = utc_now(),
    active_request_count = n,
    superseded_request_count = if ("plan_status" %in% names(plan_dt)) {
      nrow(plan_dt[plan_status == "superseded"])
    } else {
      0L
    },
    dataset_counts = as.list(table(active$dataset_type)),
    requests_by_flow = as.list(table(active$flow_code)),
    batching_strategy = "one reporter per request; multi-year periods and multi-flow M,X where applicable",
    configured_delay_seconds = as.numeric(request_delay_seconds),
    estimated_minimum_runtime_seconds = round(n * as.numeric(request_delay_seconds), 1),
    record_limit_strategy = "maxRecords=50000; truncate => retryable_failed; split if needed",
    note = "reporterCode=0 final-data requests are invalid and must not appear as active"
  )
}
