FORECAST_ENGINE_VERSION <- "1.0.0-phase12"
FORECAST_CANDIDATE_VERSION <- "cand_v1_phase12"
FORECAST_HORIZONS <- c(1L, 3L, 6L, 12L)
FORECAST_DATA_MODE_FIXTURE <- "fixture_synthetic"
FORECAST_DATA_MODE_LIVE <- "live_comtrade"
FORECAST_DATA_SOURCE_FIXTURE <- "synthetic_offline_fixtures"
FORECAST_DATA_SOURCE_LIVE <- "un_comtrade_monthly_final"

forecast_methodology_notice <- function() {
  paste(
    "Forecasts are statistical extrapolations of historical reported trade values.",
    "They are not predictions of policy decisions, supply disruptions, prices,",
    "macroeconomic conditions or realised commercial outcomes."
  )
}

forecast_fixture_notice <- function() {
  paste(
    "Synthetic fixture results.",
    "Displayed series, backtests and metrics were generated from offline synthetic monthly data",
    "and are not production accuracy claims based on live Comtrade monthly ingestion."
  )
}

forecast_data_provenance_file <- function(cfg = load_config()) {
  file.path(cfg$paths$processed, "forecast_data_provenance.json")
}

count_monthly_live_successes <- function(monthly_state) {
  st <- data.table::as.data.table(monthly_state)
  if (!nrow(st) || !"status" %in% names(st)) return(0L)
  as.integer(sum(st$status %in% c("succeeded", "skipped_cached"), na.rm = TRUE))
}

detect_forecast_fixture_markers <- function(monthly_long = NULL, candidates = NULL) {
  ml <- data.table::as.data.table(monthly_long)
  if (nrow(ml)) {
    if ("data_mode" %in% names(ml) && any(ml$data_mode == FORECAST_DATA_MODE_FIXTURE, na.rm = TRUE)) {
      return(TRUE)
    }
    if ("request_id" %in% names(ml) && any(startsWith(as.character(ml$request_id), "fixture_"), na.rm = TRUE)) {
      return(TRUE)
    }
  }
  cand <- data.table::as.data.table(candidates)
  if (nrow(cand) && "production_status" %in% names(cand) &&
      any(cand$production_status == "fixture", na.rm = TRUE)) {
    return(TRUE)
  }
  FALSE
}

resolve_forecast_provenance <- function(data_mode = NULL,
                                          data_source = NULL,
                                          monthly_state = NULL,
                                          monthly_long = NULL,
                                          candidates = NULL,
                                          force_fixture = FALSE) {
  live_ok <- count_monthly_live_successes(monthly_state)
  looks_fixture <- isTRUE(force_fixture) || detect_forecast_fixture_markers(monthly_long, candidates)
  mode <- as.character(data_mode %||% NA_character_)[1]
  src <- as.character(data_source %||% NA_character_)[1]

  if (is.na(mode) || !nzchar(mode)) {
    mode <- if (looks_fixture || live_ok == 0L) FORECAST_DATA_MODE_FIXTURE else FORECAST_DATA_MODE_LIVE
  }
  if (identical(mode, FORECAST_DATA_MODE_LIVE) && looks_fixture && live_ok == 0L) {

    mode <- FORECAST_DATA_MODE_FIXTURE
  }
  if (is.na(src) || !nzchar(src)) {
    src <- if (identical(mode, FORECAST_DATA_MODE_FIXTURE)) {
      FORECAST_DATA_SOURCE_FIXTURE
    } else {
      FORECAST_DATA_SOURCE_LIVE
    }
  }
  production_available <- identical(mode, FORECAST_DATA_MODE_LIVE) && live_ok > 0L && !looks_fixture
  list(
    data_mode = mode,
    data_source = src,
    is_fixture = identical(mode, FORECAST_DATA_MODE_FIXTURE),
    production_forecast_available = isTRUE(production_available),
    live_monthly_successful_requests = as.integer(live_ok),
    fixture_accuracy_disclaimer = if (identical(mode, FORECAST_DATA_MODE_FIXTURE)) {
      "Fixture metrics are offline synthetic diagnostics only and must not be described as production accuracy."
    } else {
      NA_character_
    }
  )
}

normalize_forecast_profile <- function(profile,
                                         monthly_long = NULL,
                                         candidates = NULL,
                                         monthly_state = NULL) {
  prof <- if (is.null(profile)) list() else as.list(profile)
  force_fixture <- identical(prof$data_mode, FORECAST_DATA_MODE_FIXTURE) ||
    isTRUE(prof$is_fixture)

  unlabeled <- is.null(prof$data_mode) || !nzchar(as.character(prof$data_mode %||% ""))
  if (isTRUE(unlabeled) && detect_forecast_fixture_markers(monthly_long, candidates)) {
    force_fixture <- TRUE
  }
  if (isTRUE(unlabeled) && isTRUE(prof$production_forecast_available)) {

    live_ok <- count_monthly_live_successes(monthly_state)
    if (live_ok == 0L) force_fixture <- TRUE
  }
  prov <- resolve_forecast_provenance(
    data_mode = prof$data_mode,
    data_source = prof$data_source,
    monthly_state = monthly_state,
    monthly_long = monthly_long,
    candidates = candidates,
    force_fixture = force_fixture
  )
  prof$data_mode <- prov$data_mode
  prof$data_source <- prov$data_source
  prof$is_fixture <- prov$is_fixture
  prof$production_forecast_available <- prov$production_forecast_available
  prof$live_monthly_successful_requests <- prov$live_monthly_successful_requests
  prof$fixture_accuracy_disclaimer <- prov$fixture_accuracy_disclaimer
  if (is.null(prof$mape_claim_below_15) || !isFALSE(prof$mape_claim_below_15)) {

    if (isTRUE(prov$is_fixture)) prof$mape_claim_below_15 <- FALSE
  }
  if (is.null(prof$mape_claim_below_15)) prof$mape_claim_below_15 <- FALSE
  prof
}

write_forecast_data_provenance <- function(prov, cfg = load_config()) {
  ensure_dir(cfg$paths$processed)
  payload <- list(
    data_mode = prov$data_mode,
    data_source = prov$data_source,
    is_fixture = prov$is_fixture,
    production_forecast_available = prov$production_forecast_available,
    live_monthly_successful_requests = prov$live_monthly_successful_requests,
    written_at = utc_now(),
    replaces_prior_mode = TRUE
  )
  write_json_atomic(payload, forecast_data_provenance_file(cfg))
  invisible(payload)
}

read_forecast_data_provenance <- function(cfg = load_config()) {
  safe_read_json(forecast_data_provenance_file(cfg))
}

forecast_partial_notice <- function(represented, selected, coverage = NULL) {
  if (!is.null(coverage) && length(coverage)) {
    return(detailed_coverage_notice(coverage, context = "forecast"))
  }
  cov <- list(
    represented_reporter_count = as.integer(represented %||% 0L),
    selected_reporter_count = as.integer(selected %||% 0L),
    missing_reporter_count = max(
      0L,
      as.integer(selected %||% 0L) - as.integer(represented %||% 0L)
    ),
    production_status = if (
      as.integer(selected %||% 0L) > 0L &&
      identical(as.integer(represented %||% 0L), as.integer(selected %||% 0L))
    ) "complete" else "partial",
    request_summary = list(planned = 0L, active = 0L, quota_blocked = 0L)
  )
  detailed_coverage_notice(cov, context = "forecast")
}

format_forecast_usd <- function(x, scale = "auto") {
  format_trade_value_scaled(sanitize_chart_numeric(x), scale = scale)
}

format_forecast_pct <- function(x, digits = 1L) {
  x <- sanitize_chart_numeric(x)
  if (length(x) != 1L) {
    return(vapply(x, format_forecast_pct, character(1), digits = digits))
  }
  if (is.na(x) || !is.finite(x)) return("Unavailable")
  sprintf("%.*f%%", digits, x)
}

format_forecast_metric <- function(x, digits = 3L) {
  x <- sanitize_chart_numeric(x)
  if (!is.finite(x)) return("Unavailable")
  sprintf("%.*f", digits, x)
}

mape_unavailable_message <- function() {
  "Unavailable due to zero or missing actual values."
}

make_series_id <- function(reporter_iso3, partner_iso3, hs_code, flow_code) {
  sprintf(
    "%s__%s__%s__%s",
    as.character(reporter_iso3),
    as.character(partner_iso3),
    as.character(hs_code),
    as.character(flow_code)
  )
}

parse_series_id <- function(series_id) {
  parts <- strsplit(as.character(series_id), "__", fixed = TRUE)
  data.table::data.table(
    series_id = as.character(series_id),
    reporter_iso3 = vapply(parts, function(p) p[1] %||% NA_character_, character(1)),
    partner_iso3 = vapply(parts, function(p) p[2] %||% NA_character_, character(1)),
    hs_code = vapply(parts, function(p) p[3] %||% NA_character_, character(1)),
    flow_code = vapply(parts, function(p) p[4] %||% NA_character_, character(1))
  )
}

forecast_model_ids <- function() {
  c("seasonal_naive", "naive", "drift", "ets", "arima", "prophet")
}

forecast_model_labels <- function() {
  c(
    seasonal_naive = "Seasonal naïve",
    naive = "Naïve",
    drift = "Drift",
    ets = "ETS",
    arima = "ARIMA",
    prophet = "Prophet"
  )
}

forecast_model_choices <- function() {
  labs <- forecast_model_labels()
  stats::setNames(names(labs), unname(labs))
}

normalise_forecast_model_id <- function(x) {
  raw <- tolower(trimws(as.character(x %||% "")[1]))
  if (!nzchar(raw)) return(NA_character_)
  raw <- gsub("[[:space:]-]+", "_", raw)
  raw <- gsub("ï", "i", raw, fixed = TRUE)
  aliases <- c(
    "seasonal_naive" = "seasonal_naive",
    "seasonal_naïve" = "seasonal_naive",
    "seasonalnaïve" = "seasonal_naive",
    "seasonalnaive" = "seasonal_naive",
    "naive" = "naive",
    "naïve" = "naive",
    "drift" = "drift",
    "ets" = "ets",
    "arima" = "arima",
    "prophet" = "prophet"
  )
  if (raw %in% names(aliases)) return(unname(aliases[[raw]]))
  if (raw %in% forecast_model_ids()) return(raw)

  labs <- tolower(unname(forecast_model_labels()))
  hit <- match(tolower(as.character(x %||% "")[1]), labs)
  if (!is.na(hit)) return(names(forecast_model_labels())[hit])
  NA_character_
}

safe_forecast_total <- function(values) {
  v <- sanitize_chart_numeric(values)
  v <- v[is.finite(v)]
  if (!length(v)) return(NA_real_)
  sum(v)
}

prophet_availability <- function() {
  if (!requireNamespace("prophet", quietly = TRUE)) {
    return(list(
      available = FALSE,
      reason = "prophet package is not installed or not loadable in this environment."
    ))
  }
  ok <- tryCatch({

    inherits(getNamespace("prophet"), "environment")
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    return(list(available = FALSE, reason = "prophet namespace failed to initialise."))
  }
  list(available = TRUE, reason = NA_character_)
}

forecast_package_versions <- function() {
  pkgs <- c("forecast", "prophet", "zoo", "lubridate")
  vers <- vapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) {
      as.character(utils::packageVersion(p))
    } else {
      NA_character_
    }
  }, character(1))
  as.list(stats::setNames(vers, pkgs))
}
