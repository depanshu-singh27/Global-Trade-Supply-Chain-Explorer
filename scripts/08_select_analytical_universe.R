if (file.exists("renv/activate.R")) source("renv/activate.R")

source("R/utilities.R")
source("R/config.R")
source("R/comtrade_client.R")
source("R/comtrade_request_planner.R")
source("R/pipeline_state.R")
source("R/production_trade_pipeline.R")
source("R/universe_selection.R")
source("R/comtrade_reporters.R")
source("R/plan_migration.R")

cfg <- load_config()
setwd(cfg[['project_root']])

dry_run <- tolower(Sys.getenv("GTSC_DRY_RUN", "false")) %in% c("1", "true", "yes")
max_requests <- suppressWarnings(as.integer(Sys.getenv("GTSC_MAX_REQUESTS", "")))
if (is.na(max_requests)) max_requests <- Inf
retry_failed_only <- tolower(Sys.getenv("GTSC_RETRY_FAILED_ONLY", "false")) %in% c("1", "true", "yes")
refresh_universe <- tolower(Sys.getenv("GTSC_REFRESH_UNIVERSE", "false")) %in% c("1", "true", "yes")
refresh_raw <- tolower(Sys.getenv("GTSC_REFRESH_RAW", "false")) %in% c("1", "true", "yes")
offline_repair <- tolower(Sys.getenv("GTSC_OFFLINE_REPAIR", "true")) %in% c("1", "true", "yes")
request_delay <- suppressWarnings(as.numeric(Sys.getenv("GTSC_REQUEST_DELAY_SECONDS", "1.1")))
if (is.na(request_delay)) request_delay <- 1.1
top_n <- suppressWarnings(as.integer(Sys.getenv("GTSC_TOP_N", "20")))
if (is.na(top_n)) top_n <- 20L

excluded_reporter_iso3 <- c("EUR", "WLD", "W00", "ASE")

raw_path <- file.path(cfg[['paths']]$raw, "reference", "Reporters.json")
if (file.exists(raw_path) || file.exists(file.path(cfg[['paths']]$reference, "comtrade_reporters_raw.parquet"))) {
  reporters_raw <- if (file.exists(file.path(cfg[['paths']]$reference, "comtrade_reporters_raw.parquet"))) {
    data.table::as.data.table(arrow::read_parquet(file.path(cfg[['paths']]$reference, "comtrade_reporters_raw.parquet")))
  } else {
    fetch_comtrade_reporters_reference(cfg, refresh = FALSE)
  }

  if (!"is_group" %in% names(reporters_raw) || !"is_aggregate_flag" %in% names(reporters_raw)) {
    reporters_raw <- fetch_comtrade_reporters_reference(cfg, refresh = FALSE)
  }
  filt <- filter_eligible_reporters(reporters_raw, years = 2019:2024)
  atomic_write_parquet_dt(filt$eligible, file.path(cfg[['paths']]$reference, "comtrade_reporters_eligible.parquet"))
  write_reporter_exclusion_diagnostics(filt$excluded, cfg)
  cat(sprintf("REPORTERS_ELIGIBLE n=%d excluded=%d\n", nrow(filt$eligible), nrow(filt$excluded)))
} else {
  filt <- NULL
}

universe_path <- file.path(cfg[['paths']]$processed, "analytical_universe.json")
top_rep_path <- file.path(cfg[['paths']]$processed, "top_reporters.parquet")
top_par_path <- file.path(cfg[['paths']]$processed, "top_partners.parquet")
top_hs4_path <- file.path(cfg[['paths']]$processed, "top_hs4.parquet")

need_rebuild <- isTRUE(refresh_universe)
old_reporters <- NULL
old_partners <- NULL
old_hs4 <- NULL
if (file.exists(top_rep_path)) {
  old_reporters <- data.table::as.data.table(arrow::read_parquet(top_rep_path))
  if (any(old_reporters$reporter_iso3 %in% excluded_reporter_iso3, na.rm = TRUE)) {
    need_rebuild <- TRUE
    cat("UNIVERSE_EXISTS but excluded reporter present -> rebuilding.\n")
  }
  if ("reporter_entity_type" %in% names(old_reporters) &&
      any(old_reporters$reporter_entity_type != "country_or_economy", na.rm = TRUE)) {
    need_rebuild <- TRUE
  }
}
if (!need_rebuild && file.exists(universe_path) && file.exists(top_par_path) && file.exists(top_hs4_path)) {
  cat("UNIVERSE_EXISTS; refresh_universe=false -> skipping rebuild (will still migrate plan if needed).\n")

  top_reporters <- old_reporters
  top_partners <- data.table::as.data.table(arrow::read_parquet(top_par_path))
  top_hs4 <- data.table::as.data.table(arrow::read_parquet(top_hs4_path))
  u_existing <- safe_read_json(universe_path)
  ranking_year <- as.integer(u_existing$selection_methodology$ranking_year %||% {
    rank_choice_path <- file.path(cfg[['paths']]$interim, "ranking_year_choice.json")
    if (file.exists(rank_choice_path)) as.integer(safe_read_json(rank_choice_path)$year) else 2023L
  })
} else {
  trade_global_path <- file.path(cfg[['paths']]$processed, "trade_global_hs85_annual.parquet")
  if (!file.exists(trade_global_path)) stop("Missing global dataset. Run scripts/07_fetch_global_hs85.R first.", call. = FALSE)
  trade_global <- data.table::as.data.table(arrow::read_parquet(trade_global_path))

  rank_choice_path <- file.path(cfg[['paths']]$interim, "ranking_year_choice.json")
  ranking_year <- if (file.exists(rank_choice_path)) {
    as.integer(safe_read_json(rank_choice_path)$year)
  } else {
    max(trade_global$year, na.rm = TRUE)
  }

  cat(sprintf("=== Phase 2: select analytical universe (ranking_year=%d) ===\n", ranking_year))

  elig_path <- file.path(cfg[['paths']]$reference, "comtrade_reporters_eligible.parquet")
  country_iso3_set <- if (file.exists(elig_path)) {
    unique(data.table::as.data.table(arrow::read_parquet(elig_path))$iso3)
  } else {
    NULL
  }
  eligible_codes <- if (file.exists(elig_path)) {
    as.character(data.table::as.data.table(arrow::read_parquet(elig_path))$reporter_code)
  } else {
    NULL
  }

  if (!is.null(old_reporters)) old_partners <- if (file.exists(top_par_path)) data.table::as.data.table(arrow::read_parquet(top_par_path)) else NULL
  if (!is.null(old_reporters)) old_hs4 <- if (file.exists(top_hs4_path)) data.table::as.data.table(arrow::read_parquet(top_hs4_path)) else NULL

  top_reporters <- select_top_reporters_from_global(
    trade_global_dt = trade_global,
    ranking_year = ranking_year,
    top_n = top_n,
    world_partner_iso3 = "W00",
    country_iso3_set = country_iso3_set,
    excluded_reporter_iso3 = excluded_reporter_iso3,
    eligible_reporter_codes = eligible_codes
  )
  top_reporters[, reporter_entity_type := "country_or_economy"]
  atomic_write_parquet_dt(top_reporters, top_rep_path)
  cat("TOP_REPORTERS_OK n=", nrow(top_reporters), "\n", sep = "")

  if (any(top_reporters$reporter_iso3 %in% excluded_reporter_iso3, na.rm = TRUE)) {
    stop("Reporter universe validation failed: excluded reporter_iso3 found in top_reporters.", call. = FALSE)
  }
  if (nrow(top_reporters) != as.integer(top_n)) {
    stop(sprintf("Expected %d reporters, got %d.", top_n, nrow(top_reporters)), call. = FALSE)
  }

  if (dry_run) {
    cat("DRY_RUN: stopping after top reporters.\n")
    quit(status = 0)
  }

  discovery_plan <- build_detailed_ranking_discovery_plan(
    cfg = cfg,
    top_reporter_codes = top_reporters$reporter_code,
    ranking_year = ranking_year,
    classification = "HS",
    cmd_code = "85*"
  )
  plan_path <- request_plan_file(cfg)
  if (file.exists(plan_path)) {
    existing <- data.table::as.data.table(arrow::read_parquet(plan_path))
    plan_dt <- data.table::rbindlist(list(existing, discovery_plan), fill = TRUE)
    plan_dt[, request_id := as.character(request_id)]
    plan_dt <- unique(plan_dt, by = "request_id")
  } else {
    plan_dt <- discovery_plan
  }
  atomic_write_parquet_dt(plan_dt, plan_path)

  if (!offline_repair && is.finite(max_requests) && max_requests > 0) {
    res <- run_production_fetch_stage(
      cfg = cfg,
      plan_dt = discovery_plan,
      stage_filter = "universe_partner_and_hs4_rank",
      request_delay_seconds = request_delay,
      dry_run = FALSE,
      max_requests = max_requests,
      retry_failed_only = retry_failed_only,
      refresh_raw = refresh_raw
    )
    cat(sprintf("DISCOVERY_FETCH_DONE executed=%d skipped=%d\n", res$executed %||% 0L, res$skipped %||% 0L))
  } else {
    cat("OFFLINE_REPAIR: using cached discovery responses only (no new API calls).\n")
  }

  st <- load_state(cfg)
  ok_ids <- character()
  if (!is.null(st) && nrow(st)) {
    ok_ids <- st[dataset_type == "universe_partner_and_hs4_rank" &
                   status %in% c("succeeded", "skipped_cached")]$request_id
  }

  cache_ids <- as.character(discovery_plan$request_id)
  parsed_rows <- list()
  for (rid in unique(c(as.character(ok_ids), cache_ids))) {
    req <- discovery_plan[request_id == rid]
    if (!nrow(req)) next
    req <- req[1]
    if (!(as.character(req$reporter_code) %in% as.character(top_reporters$reporter_code))) next
    parsed_path <- file.path(dirname(req$raw_file), paste0(rid, ".parquet"))
    if (!file.exists(parsed_path) && !is.na(req$raw_file) && file.exists(req$raw_file)) {
      body <- paste(readLines(req$raw_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      parsed <- tryCatch(parse_comtrade_payload_production(body), error = function(e) NULL)
      if (!is.null(parsed)) {
        dt <- comtrade_records_to_dt_production(parsed$records)
        if (nrow(dt)) {
          dt[, `:=`(request_id = rid, ingested_at = utc_now())]
          arrow::write_parquet(dt, parsed_path)
        }
      }
    }
    if (file.exists(parsed_path)) {
      dt <- data.table::as.data.table(arrow::read_parquet(parsed_path))
      if (nrow(dt)) parsed_rows[[length(parsed_rows) + 1L]] <- dt
    }
  }
  if (!length(parsed_rows)) stop("No parsed discovery data for refreshed reporters.", call. = FALSE)

  bilateral_dt <- data.table::rbindlist(parsed_rows, fill = TRUE)
  bilateral_dt[, year := suppressWarnings(as.integer(ref_year %||% substr(period, 1, 4)))]
  bilateral_dt[, hs_code := as.character(cmd_code)]
  bilateral_dt[, hs_level := ifelse(is.na(aggr_level), nchar(hs_code), as.integer(aggr_level))]
  bilateral_dt[, commodity_description := as.character(cmd_desc)]
  bilateral_dt[, trade_value_usd := as.numeric(primary_value)]
  bilateral_dt[, flow_code := as.character(flow_code)]

  bilateral_dt[, is_world := partner_iso3 == "W00" | partner_code == "0"]
  bilateral_dt[, is_unspecified := grepl("unspecified|not specified|nes", partner_name, ignore.case = TRUE)]
  bilateral_dt[, is_country := !is_world & !is_unspecified & grepl("^[A-Z]{3}$", partner_iso3) &
                 (is.null(country_iso3_set) | partner_iso3 %in% country_iso3_set)]
  bilateral_dt[, is_aggregate := !is_country & !is_world]

  excluded <- bilateral_dt[is_country == FALSE]
  if (nrow(excluded)) {
    excl_sum <- excluded[, .N, by = .(partner_code, partner_iso3, partner_name, is_world, is_aggregate, is_unspecified)]
    atomic_write_parquet_dt(excl_sum, file.path(cfg[['paths']]$interim, "production_excluded_rows_summary.parquet"))
  }

  sel <- select_top_partners_and_hs4_from_bilateral(
    bilateral_dt = bilateral_dt[is_country == TRUE],
    top_reporter_codes = top_reporters$reporter_code,
    ranking_year = ranking_year,
    top_partners_n = top_n,
    top_hs4_n = top_n,
    world_partner_iso3 = "W00",
    country_iso3_set = country_iso3_set
  )

  top_partners <- sel$top_partners
  top_hs4 <- sel$top_hs4
  data.table::setnames(top_partners, "partner_rank", "rank", skip_absent = TRUE)
  if (!"rank" %in% names(top_partners) && "partner_rank" %in% names(top_partners)) {
    top_partners[, rank := partner_rank]
  }
  if (!"rank" %in% names(top_hs4) && "hs_rank" %in% names(top_hs4)) {
    top_hs4[, rank := hs_rank]
  }
}

uv_checksum <- compute_universe_checksum(
  top_reporters = top_reporters,
  top_partners = top_partners,
  top_hs4 = top_hs4,
  ranking_year = ranking_year,
  classification = "HS"
)
top_reporters[, universe_checksum := uv_checksum]
top_partners[, universe_checksum := uv_checksum]
top_hs4[, universe_checksum := uv_checksum]
atomic_write_parquet_dt(top_reporters, top_rep_path)
atomic_write_parquet_dt(top_partners, top_par_path)
atomic_write_parquet_dt(top_hs4, top_hs4_path)

universe <- list(
  top_reporters = top_reporters,
  top_partners = top_partners,
  top_hs4 = top_hs4,
  universe_checksum = uv_checksum,
  universe_version = uv_checksum,
  selection_methodology = list(
    ranking_year = ranking_year,
    classification_path = "HS",
    reporter_score = "imports+exports with World partner; metadata-eligible country_or_economy only; tie-break ISO3 then code",
    partner_score = "bilateral trade with top reporters; exclude World/aggregates/unspecified; HS4 discovery via cmdCode=85*",
    hs4_score = "sum of trade on HS4 codes beginning with 85 among selected reporter-partner pairs; tie-break by hs_code",
    reporter_eligibility = "source isGroup/aggregate metadata primary; defensive denylist EUR,WLD,W00,ASE"
  ),
  generated_at = utc_now()
)
persist_analytical_universe(universe, cfg)

if (!is.null(old_reporters)) {
  removed <- setdiff(as.character(old_reporters$reporter_iso3), as.character(top_reporters$reporter_iso3))
  added <- setdiff(as.character(top_reporters$reporter_iso3), as.character(old_reporters$reporter_iso3))
  cat("REPORTER_REMOVED=", paste(removed, collapse = ","), "\n", sep = "")
  cat("REPORTER_ADDED=", paste(added, collapse = ","), "\n", sep = "")
}
if (!is.null(old_partners)) {
  cat("PARTNERS_CHANGED=", !identical(sort(as.character(old_partners$partner_code)), sort(as.character(top_partners$partner_code))), "\n", sep = "")
}
if (!is.null(old_hs4)) {
  cat("HS4_CHANGED=", !identical(sort(as.character(old_hs4$hs_code)), sort(as.character(top_hs4$hs_code))), "\n", sep = "")
}

cat("FINAL_TOP20\n")
print(top_reporters[, .(rank, reporter_code, reporter_iso3, reporter_name, score_value_usd, reporter_entity_type)])
cat("UNIVERSE_CHECKSUM=", uv_checksum, "\n", sep = "")
cat("RANKING_YEAR=", ranking_year, "\n", sep = "")

mig <- migrate_detailed_plan_for_universe_refresh(
  cfg = cfg,
  new_universe = list(top_reporters = top_reporters, top_partners = top_partners, top_hs4 = top_hs4),
  universe_checksum = uv_checksum,
  years = 2019:2024,
  classification = "HS"
)
cat(sprintf(
  "DETAILED_MIGRATE active=%d superseded=%d reused=%d invalidated_cache=%d reporters=%d years=%d\n",
  nrow(mig$active_detailed),
  length(mig$superseded_ids),
  length(mig$reused_ids),
  length(mig$invalidated_cache_ids),
  data.table::uniqueN(mig$active_detailed$reporter_code),
  data.table::uniqueN(mig$active_detailed$year)
))

partial <- rebuild_partial_detailed_from_caches(
  cfg = cfg,
  universe = list(top_reporters = top_reporters, top_partners = top_partners, top_hs4 = top_hs4),
  universe_checksum = uv_checksum,
  years = 2019:2024
)
cat(sprintf(
  "PARTIAL_DETAILED rows=%d represented=%d reused_reporters=%s invalidated=%d checksum=%s\n",
  partial$rows, partial$represented_reporters,
  paste(partial$reused_reporters, collapse = ","),
  partial$invalidated, partial$universe_checksum
))

summary <- summarise_request_plan(mig$plan, request_delay_seconds = request_delay)
summary$universe_checksum <- uv_checksum
summary$active_detailed_request_count <- nrow(mig$active_detailed)
summary$active_detailed_reporters <- data.table::uniqueN(mig$active_detailed$reporter_code)
summary$active_detailed_years <- data.table::uniqueN(mig$active_detailed$year)
write_json_atomic(summary, request_plan_summary_file(cfg), pretty = TRUE)

cat("UNIVERSE_OK reporters=", nrow(top_reporters),
    " partners=", nrow(top_partners),
    " hs4=", nrow(top_hs4), "\n", sep = "")
quit(status = 0)
