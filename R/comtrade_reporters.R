unwrap_comtrade_results <- function(parsed) {
  recs <- parsed$results %||% parsed$data %||% parsed$dataset %||% list()

  if (is.list(recs) && length(recs) == 1L && is.null(names(recs[[1]])) &&
      length(recs[[1]]) > 0L && is.list(recs[[1]][[1]])) {
    recs <- recs[[1]]
  }
  if (is.data.frame(recs)) {
    recs <- jsonlite::fromJSON(
      jsonlite::toJSON(recs, auto_unbox = TRUE),
      simplifyVector = FALSE
    )
  }
  recs
}

parse_reporters_reference <- function(body_text) {
  parsed <- tryCatch(
    jsonlite::fromJSON(body_text, simplifyVector = FALSE),
    error = function(e) stop("Reporters JSON parse failed: ", e$message, call. = FALSE)
  )
  recs <- unwrap_comtrade_results(parsed)
  if (!length(recs)) {
    return(data.table::data.table(
      reporter_code = character(),
      reporter_name = character(),
      iso2 = character(),
      iso3 = character(),
      entry_effective_date = as.Date(character()),
      expiry_date = as.Date(character()),
      is_group = logical(),
      is_aggregate_flag = logical(),
      entity_type_raw = character()
    ))
  }

  rows <- lapply(recs, function(r) {

    is_group_raw <- r$isGroup %||% r$is_group %||% r$group %||% FALSE
    is_agg_raw <- r$isAggregate %||% r$is_aggregate %||% r$aggregate %||%
      r$isRegion %||% r$is_region %||% FALSE
    entity_raw <- pluck_chr(
      r$entityType %||% r$entity_type %||% r$type %||% r$reporterType %||% NA_character_
    )
    list(
      reporter_code = as.character(r$reporterCode %||% r$id %||% NA),
      reporter_name = pluck_chr(r$reporterDesc %||% r$text %||% r$reporterNote),
      iso2 = pluck_chr(r$reporterCodeIsoAlpha2 %||% r$iso2),
      iso3 = pluck_chr(r$reporterCodeIsoAlpha3 %||% r$iso3),
      entry_effective_date = suppressWarnings(
        as.Date(substr(pluck_chr(r$entryEffectiveDate %||% r$entry_effective_date), 1, 10))
      ),
      expiry_date = {
        x <- pluck_chr(r$expiryDate %||% r$exitEffectiveDate %||% r$expiry_date)
        if (is.na(x) || !nzchar(x)) as.Date(NA) else suppressWarnings(as.Date(substr(x, 1, 10)))
      },
      is_group = isTRUE(as.logical(is_group_raw)),
      is_aggregate_flag = isTRUE(as.logical(is_agg_raw)),
      entity_type_raw = entity_raw
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

fetch_comtrade_reporters_reference <- function(cfg = load_config(), refresh = FALSE) {
  ensure_data_dirs(cfg)
  raw_dir <- file.path(cfg[['paths']]$raw, "reference")
  ensure_dir(raw_dir)
  raw_path <- file.path(raw_dir, "Reporters.json")

  if (!refresh && file.exists(raw_path)) {
    body <- paste(readLines(raw_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    url <- "https://comtradeapi.un.org/files/v1/app/reference/Reporters.json"
    resp <- httr2::request(url) |>
      httr2::req_timeout(cfg$api$timeout_seconds %||% 60) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    status <- httr2::resp_status(resp)
    body <- httr2::resp_body_string(resp)
    if (status < 200 || status >= 300) {
      stop(sprintf("Reporters reference HTTP %s", status), call. = FALSE)
    }
    writeLines(body, raw_path, useBytes = TRUE)
  }

  dt <- parse_reporters_reference(body)
  arrow::write_parquet(dt, file.path(cfg[['paths']]$reference, "comtrade_reporters_raw.parquet"))
  dt
}

reporter_active_in_range <- function(entry_date, expiry_date, years) {
  years <- as.integer(years)
  start <- as.Date(sprintf("%04d-01-01", min(years)))
  end <- as.Date(sprintf("%04d-12-31", max(years)))
  entry_ok <- is.na(entry_date) | entry_date <= end
  expiry_ok <- is.na(expiry_date) | expiry_date >= start
  entry_ok & expiry_ok
}

classify_reporter_entity <- function(reporter_code,
                                     iso3,
                                     is_group = FALSE,
                                     is_aggregate_flag = FALSE,
                                     entity_type_raw = NA_character_,
                                     defensive_special_iso3 = c("EUR", "WLD", "W00", "ASE")) {
  n <- max(length(reporter_code), length(iso3), 1L)
  reporter_code <- as.character(rep(reporter_code, length.out = n))
  iso3 <- as.character(rep(iso3, length.out = n))
  is_group <- as.logical(rep(is_group, length.out = n))
  is_aggregate_flag <- as.logical(rep(is_aggregate_flag, length.out = n))
  entity_type_raw <- as.character(rep(entity_type_raw, length.out = n))
  is_group[is.na(is_group)] <- FALSE
  is_aggregate_flag[is.na(is_aggregate_flag)] <- FALSE

  out <- rep("unknown", n)

  out[reporter_code == "0" | iso3 %in% c("W00", "WLD")] <- "special"

  out[out == "unknown" & is_group] <- "group"

  raw_l <- tolower(entity_type_raw)
  raw_agg <- grepl("aggregat|group|region|union", raw_l) & !is.na(entity_type_raw) & nzchar(entity_type_raw)
  out[out == "unknown" & (is_aggregate_flag | raw_agg)] <- "aggregate"

  out[out == "unknown" & iso3 %in% defensive_special_iso3] <- "special"

  valid_iso <- !is.na(iso3) & grepl("^[A-Z]{3}$", iso3)
  valid_code <- !is.na(reporter_code) & nzchar(reporter_code) & reporter_code != "0"
  out[out == "unknown" & valid_iso & valid_code] <- "country_or_economy"

  out
}

filter_eligible_reporters <- function(reporters_dt, years = 2019:2024,
                                        defensive_special_iso3 = c("EUR", "WLD", "W00", "ASE")) {
  dt <- data.table::as.data.table(reporters_dt)
  if (!"is_aggregate_flag" %in% names(dt)) dt[, is_aggregate_flag := FALSE]
  if (!"entity_type_raw" %in% names(dt)) dt[, entity_type_raw := NA_character_]
  if (!"is_group" %in% names(dt)) dt[, is_group := FALSE]

  dt[, reporter_entity_type := classify_reporter_entity(
    reporter_code = reporter_code,
    iso3 = iso3,
    is_group = is_group,
    is_aggregate_flag = is_aggregate_flag,
    entity_type_raw = entity_type_raw,
    defensive_special_iso3 = defensive_special_iso3
  )]
  dt[, source_is_group := is_group == TRUE]
  dt[, exclusion_reason := NA_character_]

  dt[is.na(reporter_code) | !nzchar(as.character(reporter_code)) | as.character(reporter_code) == "0",
     exclusion_reason := "invalid_or_zero_reporter_code"]
  dt[is.na(exclusion_reason) & (is.na(iso3) | !grepl("^[A-Z]{3}$", iso3)),
     exclusion_reason := "missing_or_invalid_iso3"]
  dt[is.na(exclusion_reason) & reporter_entity_type == "group",
     exclusion_reason := "source_metadata_group"]
  dt[is.na(exclusion_reason) & reporter_entity_type == "aggregate",
     exclusion_reason := "source_metadata_aggregate"]
  dt[is.na(exclusion_reason) & reporter_entity_type == "special",
     exclusion_reason := "special_or_defensive_denylist"]
  dt[is.na(exclusion_reason) & reporter_entity_type == "unknown",
     exclusion_reason := "unknown_entity_type"]
  dt[is.na(exclusion_reason) &
       !reporter_active_in_range(entry_effective_date, expiry_date, years),
     exclusion_reason := "inactive_for_production_years"]

  dt[is.na(exclusion_reason) & reporter_entity_type != "country_or_economy",
     exclusion_reason := "not_country_or_economy"]

  eligible <- dt[is.na(exclusion_reason)]
  excluded <- dt[!is.na(exclusion_reason)]
  if (nrow(excluded)) {
    excluded[, `:=`(
      reporter_iso3 = iso3,
      effective_date = entry_effective_date,
      excluded_at = utc_now()
    )]
  }
  list(eligible = eligible, excluded = excluded, all = dt)
}

write_reporter_exclusion_diagnostics <- function(excluded_dt, cfg = load_config()) {
  ensure_dir(cfg[['paths']]$interim)
  cols <- c(
    "reporter_code", "reporter_iso3", "reporter_name", "reporter_entity_type",
    "exclusion_reason", "source_is_group", "effective_date", "expiry_date", "excluded_at"
  )
  dt <- data.table::as.data.table(excluded_dt)
  if (!"reporter_iso3" %in% names(dt) && "iso3" %in% names(dt)) dt[, reporter_iso3 := iso3]
  if (!"effective_date" %in% names(dt) && "entry_effective_date" %in% names(dt)) {
    dt[, effective_date := entry_effective_date]
  }
  if (!"excluded_at" %in% names(dt)) dt[, excluded_at := utc_now()]
  for (c in cols) if (!c %in% names(dt)) dt[, (c) := NA]
  out <- dt[, ..cols]
  path <- file.path(cfg[['paths']]$interim, "comtrade_reporters_excluded.parquet")
  atomic_write_parquet_dt(out, path)
  invisible(path)
}
