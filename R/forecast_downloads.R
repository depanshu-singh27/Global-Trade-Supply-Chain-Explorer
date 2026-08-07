forecast_download_provenance_meta <- function(profile = list()) {
  prof <- normalize_forecast_profile(profile)
  list(
    data_mode = prof$data_mode %||% FORECAST_DATA_MODE_FIXTURE,
    data_source = prof$data_source %||% FORECAST_DATA_SOURCE_FIXTURE,
    production_forecast_available = isTRUE(prof$production_forecast_available),
    live_monthly_successful_requests = as.integer(prof$live_monthly_successful_requests %||% 0L),
    mape_claim_below_15 = FALSE,
    accuracy_scope = if (isTRUE(prof$is_fixture) || identical(prof$data_mode, FORECAST_DATA_MODE_FIXTURE)) {
      "fixture_diagnostic_only_not_production_accuracy"
    } else {
      "live_backtest_metrics"
    }
  )
}

forecast_download_table <- function(dt, meta = list()) {
  out <- data.table::as.data.table(dt)
  drop <- grep(
    "path|url|header|secret|token|key|raw_file|cache|COMTRADE|absolute",
    names(out), ignore.case = TRUE, value = TRUE
  )
  if (length(drop)) out[, (drop) := NULL]
  char_cols <- names(out)[vapply(out, is.character, logical(1))]
  for (cc in char_cols) {
    if (any(grepl("^(/|[A-Za-z]:\\\\|file://)", out[[cc]] %||% ""), na.rm = TRUE)) {
      out[, (cc) := NULL]
    }
  }

  out[, data_mode := meta$data_mode %||% FORECAST_DATA_MODE_FIXTURE]
  out[, data_source := meta$data_source %||% FORECAST_DATA_SOURCE_FIXTURE]
  if (length(meta)) {
    for (nm in names(meta)) {
      if (nm %in% c("data_mode", "data_source")) next
      out[, (nm) := meta[[nm]]]
    }
  }
  out
}

forecast_download_filename <- function(prefix, ..., ext = "csv") {
  parts <- c(
    sanitize_download_token(prefix, "forecast"),
    vapply(list(...), function(z) sanitize_download_token(z, "x"), character(1))
  )
  paste0(paste(parts, collapse = "_"), ".", ext)
}

forecast_contains_forbidden_content <- function(x) {
  txt <- if (is.data.frame(x)) paste(unlist(x), collapse = " ") else as.character(x)
  secret <- paste0("COMTRADE", "_", "PRIMARY")
  grepl(paste0(secret, "|file://|[A-Za-z]:\\\\"), txt)
}
