pipeline_state_file <- function(cfg = load_config()) {
  file.path(cfg[['paths']]$interim, "production_pipeline_state.parquet")
}

request_plan_file <- function(cfg = load_config()) {
  file.path(cfg[['paths']]$interim, "production_request_plan.parquet")
}

request_plan_summary_file <- function(cfg = load_config()) {
  file.path(cfg[['paths']]$interim, "production_request_plan_summary.json")
}

production_manifest_file <- function(cfg = load_config()) {
  file.path(cfg[['paths']]$processed, "production_pipeline_manifest.json")
}

safe_md5_file <- function(path) {

  out <- tools::md5sum(path)
  if (is.matrix(out) || is.data.frame(out)) {
    return(as.character(out[1, 1]))
  }
  as.character(out)[1]
}

atomic_write_parquet_dt <- function(dt, path) {
  ensure_dir(dirname(path))
  tmp <- paste0(path, ".tmp")
  if (file.exists(tmp)) file.remove(tmp)
  arrow::write_parquet(dt, tmp)
  if (file.exists(path)) {

    tryCatch(file.remove(path), error = function(e) NULL)
  }
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok) && file.exists(tmp)) {

    file.copy(tmp, path, overwrite = TRUE)
    file.remove(tmp)
  }
  invisible(path)
}

state_schema <- function() {
  data.table::data.table(
    request_id = character(),
    dataset_type = character(),
    status = character(),
    attempts = integer(),
    started_at = character(),
    completed_at = character(),
    http_status = integer(),
    result_row_count = integer(),
    raw_file = character(),
    raw_checksum = character(),
    error_category = character(),
    error_message = character()
  )
}

init_state_from_plan <- function(plan_dt, existing_state = NULL) {
  st <- if (!is.null(existing_state) && nrow(existing_state)) {
    data.table::as.data.table(existing_state)
  } else {
    state_schema()
  }
  plan_dt <- data.table::as.data.table(plan_dt)
  plan_dt[, request_id := as.character(request_id)]
  if ("dataset_type" %in% names(plan_dt)) {
    plan_dt[, dataset_type := as.character(dataset_type)]
  }
  st[, request_id := as.character(request_id)]

  missing <- setdiff(unique(plan_dt$request_id), unique(st$request_id))
  if (length(missing)) {
    to_add <- unique(plan_dt[request_id %in% missing, .(request_id, dataset_type)], by = "request_id")
    to_add[, `:=`(
      status = "planned",
      attempts = 0L,
      started_at = NA_character_,
      completed_at = NA_character_,
      http_status = NA_integer_,
      result_row_count = NA_integer_,
      raw_file = NA_character_,
      raw_checksum = NA_character_,
      error_category = NA_character_,
      error_message = NA_character_
    )]
    st <- data.table::rbindlist(list(st, to_add), fill = TRUE)
  }
  data.table::setkey(st, request_id)
  st
}

load_state <- function(cfg = load_config()) {
  p <- pipeline_state_file(cfg)
  if (!file.exists(p)) return(NULL)
  dt <- tryCatch(
    data.table::as.data.table(arrow::read_parquet(p)),
    error = function(e) NULL
  )
  dt
}

save_state <- function(state_dt, cfg = load_config()) {
  p <- pipeline_state_file(cfg)
  ensure_dir(dirname(p))
  atomic_write_parquet_dt(state_dt, p)
  invisible(p)
}

recover_stale_running <- function(state_dt, stale_minutes = 120) {
  if (is.null(state_dt) || !nrow(state_dt)) return(state_dt)

  cutoff <- as.POSIXct(Sys.time(), tz = "UTC") - (stale_minutes * 60)
  started <- suppressWarnings(as.POSIXct(state_dt$started_at, tz = "UTC"))
  stale_idx <- which(state_dt$status == "running" & !is.na(started) & started < cutoff)
  if (length(stale_idx)) {
    state_dt[stale_idx, `:=`(status = "planned", error_category = "stale_running", error_message = "Recovered stale running record")]
  }
  state_dt
}

mark_terminal <- function(state_dt, request_id, status, http_status = NA_integer_,
                            attempts = NULL, result_row_count = NA_integer_,
                            raw_file = NULL, raw_checksum = NULL,
                            error_category = NULL, error_message = NULL) {
  i <- which(state_dt$request_id == request_id)
  if (!length(i)) return(state_dt)
  now <- utc_now()
  new_status <- as.character(status)
  new_http <- if (is.null(http_status) || length(http_status) == 0L) {
    state_dt$http_status[i]
  } else {
    as.integer(http_status)
  }
  new_attempts <- if (is.null(attempts)) state_dt$attempts[i] else as.integer(attempts)
  new_rows <- if (is.null(result_row_count) || length(result_row_count) == 0L ||
                  (length(result_row_count) == 1L && is.na(result_row_count))) {
    state_dt$result_row_count[i]
  } else {
    as.integer(result_row_count)
  }
  new_raw <- if (is.null(raw_file)) state_dt$raw_file[i] else as.character(raw_file)
  new_sum <- if (is.null(raw_checksum)) state_dt$raw_checksum[i] else as.character(raw_checksum)
  new_cat <- if (is.null(error_category)) state_dt$error_category[i] else as.character(error_category)
  new_msg <- if (is.null(error_message)) state_dt$error_message[i] else as.character(error_message)

  state_dt[i, `:=`(
    status = new_status,
    attempts = new_attempts,
    completed_at = now,
    http_status = new_http,
    result_row_count = new_rows,
    raw_file = new_raw,
    raw_checksum = new_sum,
    error_category = new_cat,
    error_message = new_msg
  )]
  state_dt
}

select_requests_to_run <- function(state_dt,
                                     retry_failed_only = FALSE,
                                     max_requests = Inf,
                                     force_permanent = FALSE) {
  if (is.null(state_dt) || !nrow(state_dt)) return(data.table::data.table())

  terminal_skip <- c("invalid", "superseded", "succeeded", "empty", "skipped_cached")
  if (!isTRUE(force_permanent)) {
    terminal_skip <- c(terminal_skip, "permanently_failed")
  }
  eligible <- state_dt[!(status %in% terminal_skip)]

  retryable_statuses <- c("retryable_failed", "quota_blocked")
  if (retry_failed_only) {

    pending <- eligible[status %in% retryable_statuses][order(request_id)]
  } else {
    pending <- eligible[status %in% c("planned", retryable_statuses)][order(request_id)]
  }
  if (is.finite(max_requests)) {
    pending <- pending[seq_len(min(nrow(pending), as.integer(max_requests)))]
  }
  pending
}
