validation_row <- function(check, status, detail = NA_character_, severity = "mandatory") {
  data.table::data.table(
    check = as.character(check),
    status = as.character(status),
    detail = as.character(detail),
    severity = as.character(severity)
  )
}

validate_release_bundle <- function(bundle_dir,
                                    expected_profile = NULL,
                                    allow_demo_label = TRUE) {
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- validation_row(...)

  if (!dir.exists(bundle_dir)) {
    add("bundle_exists", "fail", "directory missing")
    return(data.table::rbindlist(rows, fill = TRUE))
  }
  add("bundle_exists", "pass")

  man_path <- file.path(bundle_dir, "release_bundle_manifest.json")
  if (!file.exists(man_path)) {
    add("bundle_manifest", "fail", "release_bundle_manifest.json missing")
    return(data.table::rbindlist(rows, fill = TRUE))
  }
  man <- safe_read_json(man_path)
  if (is.null(man)) {
    add("bundle_manifest", "fail", "unreadable manifest")
    return(data.table::rbindlist(rows, fill = TRUE))
  }
  add("bundle_manifest", "pass")

  if (!is.null(expected_profile) && !identical(man$runtime_profile, expected_profile)) {
    add("bundle_profile", "fail",
        paste0("expected=", expected_profile, " actual=", man$runtime_profile %||% NA))
  } else {
    add("bundle_profile", "pass", man$runtime_profile %||% NA_character_)
  }

  allow <- release_bundle_allowlist()
  on_disk <- list.files(bundle_dir, recursive = TRUE)
  unexpected <- setdiff(on_disk, allow)
  if (length(unexpected)) {
    add("bundle_allowlist", "fail", paste(unexpected, collapse = ","))
  } else {
    add("bundle_allowlist", "pass")
  }

  required_core <- c(
    "analytical_universe.json",
    "trade_data_profile.json",
    "production_pipeline_manifest.json",
    "trade_global_hs85_annual.parquet",
    "trade_detailed_enriched.parquet",
    "forecast_profile.json",
    "release_bundle_manifest.json"
  )
  missing <- required_core[!file.exists(file.path(bundle_dir, required_core))]
  if (length(missing)) {
    add("required_files", "fail", paste(missing, collapse = ","))
  } else {
    add("required_files", "pass")
  }

  checksum_ok <- TRUE
  for (entry in man$files %||% list()) {
    rel <- entry$path %||% entry$relative_path
    if (is.null(rel) || identical(rel, "release_bundle_manifest.json")) next
    p <- file.path(bundle_dir, rel)
    if (!file.exists(p)) {
      checksum_ok <- FALSE
      add("checksum_missing_file", "fail", rel)
      next
    }
    got <- file_sha256(p)
    exp <- entry$sha256 %||% entry$checksum
    if (!identical(got, exp)) {
      checksum_ok <- FALSE
      add("checksum_mismatch", "fail", rel)
    }
  }
  if (checksum_ok) add("bundle_checksums", "pass")

  global <- man$global_production_status %||% NA_character_
  detailed <- man$detailed_production_status %||% NA_character_
  if (identical(detailed, "complete") &&
      !is.null(man$represented_reporter_count) &&
      !is.null(man$selected_reporter_count) &&
      as.integer(man$represented_reporter_count) < as.integer(man$selected_reporter_count)) {
    add("detailed_status_consistency", "fail",
        "detailed marked complete but represented < selected")
  } else if (identical(detailed, "partial")) {
    add("detailed_partial_preserved", "pass")
  } else {
    add("detailed_status_consistency", "warning", detailed)
  }

  if (identical(global, "complete")) {
    add("global_complete_preserved", "pass")
  } else {
    add("global_status", "warning", global)
  }

  fc_mode <- man$forecast_data_mode %||% NA_character_
  if (identical(fc_mode, "fixture_synthetic")) {
    add("forecast_fixture_provenance", "pass")
  } else {
    add("forecast_fixture_provenance", "warning", fc_mode)
  }
  live_ok <- as.integer(man$live_monthly_successful_requests %||% 0L)
  prod_fc <- isTRUE(man$production_forecast_available)
  if (identical(live_ok, 0L) && isTRUE(prod_fc)) {
    add("production_forecast_false_when_live_zero", "fail")
  } else if (identical(live_ok, 0L) && !isTRUE(prod_fc)) {
    add("production_forecast_false_when_live_zero", "pass")
  } else {
    add("production_forecast_false_when_live_zero", "warning",
        paste0("live=", live_ok))
  }

  sec <- scan_bundle_for_secrets(bundle_dir)
  if (isTRUE(sec$ok)) {
    add("secret_scan", "pass")
  } else {
    add("secret_scan", "fail", paste(vapply(sec$hits, function(h) h$check, ""), collapse = ","))
  }

  readable <- TRUE
  for (f in c("trade_global_hs85_annual.parquet", "analytical_universe.json")) {
    p <- file.path(bundle_dir, f)
    if (!file.exists(p)) next
    ok <- tryCatch({
      if (grepl("\\.json$", f)) !is.null(safe_read_json(p))
      else !is.null(arrow::read_parquet(p, as_data_frame = TRUE))
    }, error = function(e) FALSE)
    if (!ok) readable <- FALSE
  }
  add("bundle_readable", if (readable) "pass" else "fail")

  if (nzchar(man$universe_checksum %||% "")) {
    add("universe_checksum", "pass", man$universe_checksum)
  } else {
    add("universe_checksum", "fail", "missing")
  }

  data.table::rbindlist(rows, fill = TRUE)
}

validate_runtime_profile_or_stop <- function(runtime_cfg, bundle_dir) {
  prof <- runtime_cfg$runtime_profile
  if (!prof %in% runtime_profile_names()) {
    stop("Unknown runtime profile: ", prof, call. = FALSE)
  }
  man_path <- file.path(bundle_dir, "release_bundle_manifest.json")
  if (!file.exists(man_path)) {
    stop("Bundle manifest missing for profile ", prof, call. = FALSE)
  }
  res <- validate_release_bundle(bundle_dir, expected_profile = if (prof == "external") NULL else prof)
  fails <- res[status == "fail"]
  if (nrow(fails)) {
    stop(
      "Bundle validation failed for profile ", prof, ": ",
      paste(fails$check, collapse = ", "),
      call. = FALSE
    )
  }

  man <- safe_read_json(man_path)
  if (prof %in% c("release", "external")) {
    if (identical(man$runtime_profile, "demo") && !identical(prof, "demo")) {
      stop("Refusing silent demo fallback for profile ", prof, call. = FALSE)
    }
  }
  invisible(res)
}

gate_status_from_rows <- function(dt) {
  if (!nrow(dt)) return("not_run")
  sev <- if ("severity" %in% names(dt)) dt$severity else rep("mandatory", nrow(dt))
  mandatory <- dt[sev == "mandatory" | is.na(sev) | !nzchar(sev)]
  if (nrow(mandatory) && any(mandatory$status == "fail")) return("fail")
  if (any(dt$status == "fail")) return("fail")
  if (nrow(mandatory) && any(mandatory$status == "warning")) return("warning")
  if (any(dt$status == "warning")) return("warning")

  if (nrow(mandatory) && any(mandatory$status == "not_run")) return("warning")
  "pass"
}
