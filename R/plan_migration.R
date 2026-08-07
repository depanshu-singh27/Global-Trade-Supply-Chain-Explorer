is_invalid_reporter_zero_request <- function(plan_row) {
  rc <- as.character(plan_row$reporter_code %||% plan_row$reporter_codes_api %||% "")

  identical(rc, "0") || identical(gsub("\\s", "", rc), "0")
}

migrate_invalid_reporter_zero_state <- function(cfg = load_config()) {
  ensure_data_dirs(cfg)
  plan_path <- request_plan_file(cfg)
  state_path <- pipeline_state_file(cfg)
  audit_path <- file.path(cfg[['paths']]$interim, "production_plan_migration_audit.json")

  plan <- if (file.exists(plan_path)) {
    data.table::as.data.table(arrow::read_parquet(plan_path))
  } else {
    data.table::data.table()
  }
  st <- load_state(cfg)
  if (is.null(st)) st <- state_schema()

  st <- recover_stale_running(st, stale_minutes = 0)

  if (nrow(st)) {
    running_idx <- which(st$status == "running")
    if (length(running_idx)) {
      st[running_idx, `:=`(
        status = "planned",
        error_category = "stale_running",
        error_message = "Recovered interrupted running record before migration"
      )]
    }
  }

  invalid_ids <- character()
  if (nrow(plan) && "reporter_code" %in% names(plan)) {
    invalid_ids <- plan[reporter_code == "0"]$request_id
  }

  migrated_at <- utc_now()
  if (length(invalid_ids)) {

    if (!"plan_status" %in% names(plan)) plan[, plan_status := "active"]
    plan[request_id %in% invalid_ids, plan_status := "superseded"]

    for (rid in invalid_ids) {
      if (!rid %in% st$request_id) {
        st <- data.table::rbindlist(list(st, data.table::data.table(
          request_id = rid,
          dataset_type = plan[request_id == rid]$dataset_type[1] %||% "trade_global_hs85_annual",
          status = "invalid",
          attempts = 0L,
          started_at = NA_character_,
          completed_at = migrated_at,
          http_status = NA_integer_,
          result_row_count = NA_integer_,
          raw_file = NA_character_,
          raw_checksum = NA_character_,
          error_category = "invalid_reporter_strategy_replanned",
          error_message = "reporterCode=0 is invalid for final-data ingestion; superseded by reporter-code plan"
        )), fill = TRUE)
      } else {
        st[request_id == rid, `:=`(
          status = "invalid",
          completed_at = migrated_at,
          error_category = "invalid_reporter_strategy_replanned",
          error_message = "reporterCode=0 is invalid for final-data ingestion; superseded by reporter-code plan"
        )]
      }
    }
  }

  if (nrow(plan)) {

    tmp <- paste0(plan_path, ".tmp")
    if (file.exists(plan_path)) file.remove(plan_path)
    arrow::write_parquet(plan, tmp)
    file.rename(tmp, plan_path)
  }
  save_state(st, cfg)

  audit <- list(
    migrated_at = migrated_at,
    reason = "invalid_reporter_strategy_replanned",
    invalid_request_count = length(invalid_ids),
    invalid_request_ids = as.list(invalid_ids),
    note = "Old reporterCode=0 final-data requests marked invalid/superseded; not deleted."
  )
  write_json_atomic(audit, audit_path, pretty = TRUE)

  list(plan = plan, state = st, audit = audit, invalid_ids = invalid_ids)
}

migrate_detailed_plan_for_universe_refresh <- function(cfg,
                                                        new_universe,
                                                        universe_checksum,
                                                        years = 2019:2024,
                                                        classification = "HS") {
  ensure_data_dirs(cfg)
  plan_path <- request_plan_file(cfg)
  audit_path <- file.path(cfg[['paths']]$interim, "production_universe_migration_audit.json")
  migrated_at <- utc_now()
  new_uv <- as.character(universe_checksum)

  plan <- if (file.exists(plan_path)) {
    data.table::as.data.table(arrow::read_parquet(plan_path))
  } else {
    data.table::data.table()
  }
  if (nrow(plan)) plan[, request_id := as.character(request_id)]

  st <- load_state(cfg)
  if (is.null(st)) st <- state_schema()
  st <- recover_stale_running(st, stale_minutes = 0)
  if (nrow(st) && any(st$status == "running")) {
    st[status == "running", `:=`(
      status = "planned",
      error_category = "stale_running",
      error_message = "Recovered interrupted running record before universe migration"
    )]
  }

  new_plan <- build_detailed_top20_plan(
    cfg = cfg,
    universe = new_universe,
    years = years,
    classification = classification,
    cmd_code = "85*"
  )
  new_plan[, `:=`(
    universe_checksum = new_uv,
    plan_status = "active"
  )]

  new_ids <- as.character(new_plan$request_id)
  new_reps <- sort(unique(as.character(new_universe$top_reporters$reporter_code)))
  excluded_iso <- c("EUR", "WLD", "W00", "ASE")

  det <- if (nrow(plan) && "dataset_type" %in% names(plan)) {
    plan[dataset_type == "trade_detailed_top20"]
  } else {
    data.table::data.table()
  }

  superseded_ids <- character()
  reused_ids <- character()
  invalidated_cache_ids <- character()

  if (nrow(det)) {
    if (!"plan_status" %in% names(plan)) plan[, plan_status := "active"]
    if (!"universe_checksum" %in% names(plan)) plan[, universe_checksum := NA_character_]
    if (!"supersession_reason" %in% names(plan)) plan[, supersession_reason := NA_character_]

    active_old <- det[is.na(plan_status) | plan_status == "active"]
    for (rid in as.character(active_old$request_id)) {
      row <- active_old[request_id == rid][1]
      rc <- as.character(row$reporter_code %||% "")
      keep <- rid %in% new_ids && rc %in% new_reps
      iso <- if ("reporter_iso3" %in% names(row)) as.character(row$reporter_iso3) else NA_character_
      if (!keep || identical(rc, "97") || (!is.na(iso) && iso %in% excluded_iso)) {
        plan[request_id == rid, `:=`(
          plan_status = "superseded",
          supersession_reason = "universe_refresh_superseded"
        )]
        superseded_ids <- c(superseded_ids, rid)
        if (rid %in% st$request_id) {
          prev_status <- st[request_id == rid]$status[1]
          if (prev_status %in% c("succeeded", "skipped_cached", "empty")) {
            invalidated_cache_ids <- c(invalidated_cache_ids, rid)
          }
          st[request_id == rid, `:=`(
            status = "superseded",
            completed_at = migrated_at,
            error_category = "universe_refresh_superseded",
            error_message = "Superseded by refreshed analytical universe"
          )]
        }
      } else {
        plan[request_id == rid, `:=`(
          plan_status = "active",
          universe_checksum = new_uv
        )]
        reused_ids <- c(reused_ids, rid)
      }
    }
  }

  other <- if (nrow(plan)) {
    plan[is.na(dataset_type) | dataset_type != "trade_detailed_top20"]
  } else {
    data.table::data.table()
  }
  det_keep <- if (nrow(plan) && "dataset_type" %in% names(plan)) {
    plan[dataset_type == "trade_detailed_top20"]
  } else {
    data.table::data.table()
  }

  missing_new <- new_plan[!request_id %in% as.character(det_keep$request_id)]
  plan_out <- data.table::rbindlist(list(other, det_keep, missing_new), fill = TRUE)
  plan_out[, request_id := as.character(request_id)]
  plan_out <- unique(plan_out, by = "request_id")

  if (nrow(plan_out)) {
    if (!"universe_checksum" %in% names(plan_out)) plan_out[, universe_checksum := NA_character_]
    plan_out[dataset_type == "trade_detailed_top20" &
               (is.na(plan_status) | plan_status == "active"),
             universe_checksum := new_uv]
  }

  st <- init_state_from_plan(
    plan_out[dataset_type == "trade_detailed_top20" &
               (is.na(plan_status) | plan_status == "active")],
    existing_state = st
  )

  if (nrow(st) && length(superseded_ids)) {
    st[request_id %in% superseded_ids & !(status %in% c("superseded", "invalid")),
       `:=`(status = "superseded",
            error_category = "universe_refresh_superseded",
            error_message = "Superseded by refreshed analytical universe",
            completed_at = migrated_at)]
  }

  atomic_write_parquet_dt(plan_out, plan_path)
  save_state(st, cfg)

  active_det <- plan_out[dataset_type == "trade_detailed_top20" &
                           (is.na(plan_status) | plan_status == "active")]
  audit <- list(
    migrated_at = migrated_at,
    reason = "universe_refresh_superseded",
    universe_checksum = new_uv,
    superseded_request_count = length(unique(superseded_ids)),
    reused_request_ids = as.list(unique(reused_ids)),
    invalidated_cache_ids = as.list(unique(invalidated_cache_ids)),
    new_active_request_count = nrow(active_det),
    new_active_reporters = length(unique(active_det$reporter_code)),
    new_active_years = length(unique(active_det$year)),
    note = "Obsolete detailed requests superseded; valid semantic caches retained where IDs match."
  )
  write_json_atomic(audit, audit_path, pretty = TRUE)

  list(
    plan = plan_out,
    state = st,
    audit = audit,
    new_plan = new_plan,
    active_detailed = active_det,
    superseded_ids = unique(superseded_ids),
    reused_ids = unique(reused_ids),
    invalidated_cache_ids = unique(invalidated_cache_ids)
  )
}
