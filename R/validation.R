.validation_row <- function(check_id, dataset, status, message,
                            affected_rows = 0L) {
  data.table::data.table(
    check_id = as.character(check_id),
    dataset = as.character(dataset),
    status = as.character(status),
    message = as.character(message),
    affected_rows = as.integer(affected_rows),
    checked_at = utc_now()
  )
}

validate_required_columns <- function(dt, required, dataset) {
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    .validation_row(
      "required_columns", dataset, "error",
      paste0("Missing columns: ", paste(missing, collapse = ", ")),
      length(missing)
    )
  } else {
    .validation_row("required_columns", dataset, "pass",
                    "All required columns present", 0L)
  }
}

validate_year_range <- function(years, start_year, end_year, dataset,
                                col = "year") {
  years <- suppressWarnings(as.integer(years))
  bad <- which(!is.na(years) & (years < start_year | years > end_year))
  if (length(bad)) {
    .validation_row(
      "year_range", dataset, "error",
      sprintf("Values in %s outside [%s, %s]", col, start_year, end_year),
      length(bad)
    )
  } else {
    .validation_row("year_range", dataset, "pass",
                    sprintf("%s within [%s, %s]", col, start_year, end_year), 0L)
  }
}

validate_iso3 <- function(x, dataset, col = "iso3") {
  x <- as.character(x)
  bad <- which(!is.na(x) & !is_iso3(x))
  if (length(bad)) {
    .validation_row(
      paste0("iso3_", col), dataset, "error",
      paste0("Invalid ISO-3 values in ", col), length(bad)
    )
  } else {
    .validation_row(paste0("iso3_", col), dataset, "pass",
                    paste0(col, " values are valid ISO-3"), 0L)
  }
}

validate_hs_character <- function(hs, dataset, col = "hs_code") {
  if (!is.character(hs)) {
    .validation_row(
      "hs_character", dataset, "error",
      paste0(col, " must be character; got ", typeof(hs)), length(hs)
    )
  } else {
    .validation_row("hs_character", dataset, "pass",
                    paste0(col, " stored as character"), 0L)
  }
}

validate_non_negative <- function(x, dataset, col = "trade_value_usd") {
  x <- suppressWarnings(as.numeric(x))
  bad <- which(!is.na(x) & x < 0)
  if (length(bad)) {
    .validation_row(
      "non_negative", dataset, "error",
      paste0(col, " contains negative values"), length(bad)
    )
  } else {
    .validation_row("non_negative", dataset, "pass",
                    paste0(col, " non-negative where present"), 0L)
  }
}

validate_unique_keys <- function(dt, keys, dataset, check_id = "unique_keys") {
  if (!all(keys %in% names(dt))) {
    return(.validation_row(
      check_id, dataset, "error",
      paste0("Key columns missing: ", paste(setdiff(keys, names(dt)), collapse = ", ")),
      0L
    ))
  }
  dt <- data.table::as.data.table(dt)
  n_dup <- as.integer(nrow(dt) - nrow(unique(dt, by = keys)))
  if (n_dup > 0) {
    .validation_row(
      check_id, dataset, "error",
      paste0("Duplicate business keys on: ", paste(keys, collapse = " + ")),
      n_dup
    )
  } else {
    .validation_row(
      check_id, dataset, "pass",
      paste0("Unique keys on: ", paste(keys, collapse = " + ")), 0L
    )
  }
}

validate_mapping_coverage <- function(codes, mapped, dataset, role = "reporter") {
  codes <- as.character(codes)
  mapped <- as.character(mapped)
  unmatched <- which(!is.na(codes) & (is.na(mapped) | !nzchar(mapped)))
  if (length(unmatched)) {
    .validation_row(
      paste0("mapping_", role), dataset, "warning",
      paste0("Unmatched ", role, " codes"), length(unmatched)
    )
  } else {
    .validation_row(
      paste0("mapping_", role), dataset, "pass",
      paste0("All ", role, " codes mapped"), 0L
    )
  }
}

validate_wdi_indicators <- function(codes, expected, dataset) {
  bad <- setdiff(unique(as.character(codes)), expected)
  if (length(bad)) {
    .validation_row(
      "wdi_indicators", dataset, "warning",
      paste0("Unexpected indicators: ", paste(bad, collapse = ", ")),
      length(bad)
    )
  } else {
    .validation_row("wdi_indicators", dataset, "pass",
                    "WDI indicator codes match expected set", 0L)
  }
}

validate_parquet_roundtrip <- function(path, dataset) {
  if (!file.exists(path)) {
    return(.validation_row(
      "parquet_exists", dataset, "error",
      paste0("Missing file: ", basename(path)), 0L
    ))
  }
  exists_row <- .validation_row(
    "parquet_exists", dataset, "pass",
    paste0("File exists: ", basename(path)), 0L
  )
  ok <- TRUE
  err <- NULL
  tryCatch({
    dt <- arrow::read_parquet(path)
    if (nrow(dt) < 0) ok <- FALSE
  }, error = function(e) {
    ok <<- FALSE
    err <<- conditionMessage(e)
  })
  readable <- if (ok) {
    .validation_row("parquet_readable", dataset, "pass",
                    "Parquet readable", 0L)
  } else {
    .validation_row("parquet_readable", dataset, "error",
                    paste0("Parquet read failed: ", err %||% "unknown"), 0L)
  }
  rbind(exists_row, readable)
}

bind_validation <- function(...) {
  parts <- list(...)
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) {
    return(data.table::data.table(
      check_id = character(), dataset = character(), status = character(),
      message = character(), affected_rows = integer(), checked_at = character()
    ))
  }
  data.table::rbindlist(parts, fill = TRUE)
}

summarise_validation <- function(results) {
  list(
    n_pass = sum(results$status == "pass", na.rm = TRUE),
    n_warning = sum(results$status == "warning", na.rm = TRUE),
    n_error = sum(results$status == "error", na.rm = TRUE)
  )
}
