build_release_bundle <- function(source_dir,
                                 dest_dir,
                                 profile = "release",
                                 include_scenario_history = FALSE,
                                 root = find_project_root()) {
  if (!dir.exists(source_dir)) {
    stop("Source data directory missing: ", basename(source_dir), call. = FALSE)
  }
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  allow <- release_bundle_allowlist()
  allow <- setdiff(allow, "release_bundle_manifest.json")
  copied <- character()
  missing_required <- character()
  required_core <- c(
    "analytical_universe.json",
    "trade_data_profile.json",
    "production_pipeline_manifest.json",
    "trade_global_hs85_annual.parquet",
    "trade_global_enriched.parquet",
    "country_year_analytics.parquet",
    "trade_detailed_top20.parquet",
    "trade_detailed_enriched.parquet",
    "forecast_profile.json"
  )
  for (rel in allow) {
    src <- file.path(source_dir, rel)
    if (!file.exists(src)) {
      if (rel %in% required_core) missing_required <- c(missing_required, rel)
      next
    }
    if (release_path_is_excluded(rel)) {
      stop("Allowlist entry matches exclude pattern: ", rel, call. = FALSE)
    }
    dst <- file.path(dest_dir, rel)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(src, dst, overwrite = TRUE)
    if (!isTRUE(ok)) stop("Failed to copy ", rel, call. = FALSE)
    copied <- c(copied, rel)
  }
  if (length(missing_required)) {
    stop("Required release files missing: ", paste(missing_required, collapse = ", "),
         call. = FALSE)
  }

  existing <- list.files(dest_dir, recursive = TRUE, all.files = FALSE, no.. = TRUE)
  unexpected <- setdiff(existing, c(copied, "release_bundle_manifest.json"))
  if (length(unexpected)) {
    unlink(file.path(dest_dir, unexpected))
  }
  if (isTRUE(include_scenario_history)) {
    warning("Scenario history inclusion is documented-option only; not copied by default.",
            call. = FALSE)
  }

  normalize_bundle_json_files(dest_dir)
  manifest <- build_release_bundle_manifest(
    dest_dir = dest_dir,
    files = copied,
    profile = profile,
    root = root
  )
  man_path <- file.path(dest_dir, "release_bundle_manifest.json")
  write_json_atomic(manifest, man_path)
  list(
    dest_dir = dest_dir,
    files = c(copied, "release_bundle_manifest.json"),
    manifest = manifest,
    manifest_path = man_path
  )
}

release_path_is_excluded <- function(rel) {
  pats <- release_bundle_exclude_patterns()
  any(vapply(pats, function(p) grepl(p, rel, ignore.case = TRUE, perl = TRUE), logical(1)))
}

schema_fingerprint <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    if (ext == "parquet") {
      df <- arrow::read_parquet(path, as_data_frame = TRUE)
      cols <- paste(names(df), collapse = ",")
      paste0("parquet:", ncol(df), ":", digest_string(cols))
    } else if (ext == "json") {
      raw <- paste(readLines(path, warn = FALSE), collapse = "\n")
      paste0("json:", digest_string(raw))
    } else if (ext == "rds") {
      paste0("rds:", file.info(path)$size)
    } else {
      paste0("file:", file.info(path)$size)
    }
  }, error = function(e) NA_character_)
}

digest_string <- function(x) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(as.character(x), algo = "sha256"))
  }
  sprintf("%08x", sum(utf8ToInt(substr(paste(as.character(x), collapse = ""), 1, 2000))) %% 1e8)
}

row_count_for_file <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  if (!grepl("\\.parquet$", path, ignore.case = TRUE)) return(NA_integer_)
  tryCatch({
    as.integer(nrow(arrow::read_parquet(path, as_data_frame = TRUE)))
  }, error = function(e) NA_integer_)
}

build_release_bundle_manifest <- function(dest_dir, files, profile = "release",
                                          root = find_project_root()) {
  meta <- git_release_metadata(root)
  entries <- lapply(files, function(rel) {
    p <- file.path(dest_dir, rel)
    list(
      path = rel,
      size_bytes = as.numeric(file.info(p)$size),
      sha256 = file_sha256(p),
      row_count = row_count_for_file(p),
      schema_fingerprint = schema_fingerprint(p)
    )
  })
  provenance <- extract_bundle_provenance(dest_dir)
  renv_lock <- file.path(root, "renv.lock")
  list(
    release_bundle_version = RELEASE_BUNDLE_VERSION,
    application_version = meta$application_version,
    git_head = meta$git_head,
    dirty_working_tree = meta$dirty_working_tree,
    generated_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    runtime_profile = profile,
    file_allowlist = release_bundle_allowlist(),
    files = entries,
    data_provenance = provenance$data_provenance,
    global_production_status = provenance$global_production_status,
    detailed_production_status = provenance$detailed_production_status,
    selected_reporter_count = provenance$selected_reporter_count,
    represented_reporter_count = provenance$represented_reporter_count,
    forecast_data_mode = provenance$forecast_data_mode,
    production_forecast_available = provenance$production_forecast_available,
    live_monthly_successful_requests = provenance$live_monthly_successful_requests,
    universe_checksum = provenance$universe_checksum,
    renv_lock_checksum = if (file.exists(renv_lock)) file_sha256(renv_lock) else NA_character_,
    geometry_source = provenance$geometry_source,
    shock_engine_version = meta$shock_engine_version,
    forecast_engine_version = meta$forecast_engine_version,
    performance_evidence_status = "not_included_in_bundle",
    demo_or_synthetic_label = provenance$demo_or_synthetic_label
  )
}

extract_bundle_provenance <- function(dest_dir) {
  au <- safe_read_json(file.path(dest_dir, "analytical_universe.json"))
  tp <- safe_read_json(file.path(dest_dir, "trade_data_profile.json"))
  pm <- safe_read_json(file.path(dest_dir, "production_pipeline_manifest.json"))
  fp <- safe_read_json(file.path(dest_dir, "forecast_profile.json"))
  if (!is.null(fp) && exists("normalize_forecast_profile", mode = "function")) {
    fp <- normalize_forecast_profile(fp)
  }
  selected <- as.integer(
    tp$selected_reporters %||%
      length(au$selected_reporters %||% list()) %||%
      pm$selected_reporter_count %||% NA_integer_
  )
  if (length(selected) != 1L || is.na(selected)) {
    selected <- length(unlist(au$selected_reporters %||% list()))
  }
  represented <- as.integer(
    tp$represented_reporters %||%
      length(au$represented_reporters %||% list()) %||%
      pm$represented_reporter_count %||% NA_integer_
  )
  if (length(represented) != 1L || is.na(represented)) {
    represented <- length(unlist(au$represented_reporters %||% list()))
  }
  global_status <- as.character(
    tp$global_status %||% pm$global_production_status %||% pm$production_status_global %||%
      "complete"
  )
  detailed_status <- as.character(
    tp$detailed_status %||% pm$detailed_production_status %||%
      pm$production_status %||% "partial"
  )
  forecast_mode <- as.character(fp$data_mode %||% "fixture_synthetic")
  prod_fc <- isTRUE(fp$production_forecast_available)
  live_ok <- as.integer(fp$live_monthly_successful_requests %||% 0L)
  if (identical(live_ok, 0L)) prod_fc <- FALSE
  list(
    data_provenance = list(
      trade_source = "un_comtrade_processed_snapshot",
      forecast_source = fp$data_source %||% "synthetic_offline_fixtures",
      macro_source = "world_bank_wdi_processed_snapshot"
    ),
    global_production_status = global_status,
    detailed_production_status = detailed_status,
    selected_reporter_count = selected,
    represented_reporter_count = represented,
    forecast_data_mode = forecast_mode,
    production_forecast_available = prod_fc,
    live_monthly_successful_requests = live_ok,
    universe_checksum = as.character(
      au$universe_checksum %||% au$checksum %||% tp$universe_checksum %||% NA_character_
    ),
    geometry_source = "ne_countries_simplified.rds",
    demo_or_synthetic_label = if (identical(forecast_mode, "fixture_synthetic") ||
                                    grepl("demo", dest_dir, ignore.case = TRUE)) {
      "Demo or Synthetic Fixture Data"
    } else {
      NA_character_
    }
  )
}

build_demo_bundle <- function(dest_dir = NULL, root = find_project_root()) {
  paths <- ensure_release_dirs(release_paths(root))
  if (is.null(dest_dir)) dest_dir <- paths$demo
  if (dir.exists(dest_dir)) {
    unlink(list.files(dest_dir, full.names = TRUE), recursive = TRUE, force = TRUE)
  }
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  src <- file.path(root, "data", "processed")
  if (dir.exists(src) && file.exists(file.path(src, "trade_global_hs85_annual.parquet"))) {
    res <- build_release_bundle(src, dest_dir, profile = "demo", root = root)

    stamp_demo_provenance(dest_dir)
    normalize_bundle_json_files(dest_dir)
    res$manifest <- build_release_bundle_manifest(
      dest_dir,
      setdiff(list.files(dest_dir), "release_bundle_manifest.json"),
      profile = "demo",
      root = root
    )
    write_json_atomic(res$manifest, file.path(dest_dir, "release_bundle_manifest.json"))
    normalize_json_file_lf(file.path(dest_dir, "release_bundle_manifest.json"))
    return(res)
  }
  synthesise_demo_bundle(dest_dir, root)
}

stamp_demo_provenance <- function(dest_dir) {
  fp_path <- file.path(dest_dir, "forecast_profile.json")
  if (file.exists(fp_path)) {
    fp <- safe_read_json(fp_path) %||% list()
    fp$data_mode <- "fixture_synthetic"
    fp$data_source <- "synthetic_offline_fixtures"
    fp$production_forecast_available <- FALSE
    fp$live_monthly_successful_requests <- 0L
    fp$demo_label <- "Synthetic Fixture Data"
    fp$mape_claim_below_15 <- FALSE
    write_json_atomic(fp, fp_path)
  }
  tp_path <- file.path(dest_dir, "trade_data_profile.json")
  if (file.exists(tp_path)) {
    tp <- safe_read_json(tp_path) %||% list()
    tp$runtime_bundle_label <- "Demo or Synthetic Fixture Data"
    tp$demo_mode <- TRUE
    write_json_atomic(tp, tp_path)
  }
  man <- file.path(dest_dir, "production_pipeline_manifest.json")
  if (file.exists(man)) {
    pm <- safe_read_json(man) %||% list()
    pm$runtime_bundle_label <- "Demo or Synthetic Fixture Data"
    write_json_atomic(pm, man)
  }
  invisible(TRUE)
}

synthesise_demo_bundle <- function(dest_dir, root = find_project_root()) {

  years <- 2019:2021
  countries <- c("DEU", "IND", "ITA", "KOR", "SGP", "THA")
  global <- data.table::CJ(year = years, reporter_iso3 = countries, flow_code = c("M", "X"))
  global[, `:=`(
    partner_iso3 = "WLD",
    hs_code = "85",
    trade_value_usd = as.numeric(1000L + seq_len(.N)),
    is_synthetic = TRUE
  )]
  detailed <- data.table::CJ(
    year = years, reporter_iso3 = countries[1:3], partner_iso3 = countries,
    flow_code = c("M", "X"), hs_code = c("8517", "8542")
  )
  detailed <- detailed[reporter_iso3 != partner_iso3]
  detailed[, `:=`(trade_value_usd = as.numeric(500L + seq_len(.N)), is_synthetic = TRUE)]
  cy <- unique(global[, .(year, reporter_iso3)])
  cy[, `:=`(
    imports_usd = 1e6, exports_usd = 9e5, trade_balance_usd = -1e5,
    gdp_usd = 1e12, population = 5e7, is_synthetic = TRUE
  )]
  write_parquet_demo <- function(dt, name) {
    arrow::write_parquet(as.data.frame(dt), file.path(dest_dir, name))
  }
  write_parquet_demo(global, "trade_global_hs85_annual.parquet")
  write_parquet_demo(global, "trade_global_enriched.parquet")
  write_parquet_demo(detailed, "trade_detailed_top20.parquet")
  write_parquet_demo(detailed, "trade_detailed_enriched.parquet")
  write_parquet_demo(cy, "country_year_analytics.parquet")
  write_parquet_demo(data.table::data.table(reporter_iso3 = countries, rank = seq_along(countries)),
                     "top_reporters.parquet")
  write_parquet_demo(data.table::data.table(partner_iso3 = countries, rank = seq_along(countries)),
                     "top_partners.parquet")
  write_parquet_demo(data.table::data.table(hs4 = c("8517", "8542"), rank = 1:2), "top_hs4.parquet")
  write_parquet_demo(data.table::data.table(iso3 = countries, year = 2021L, NY.GDP.MKTP.CD = 1e12),
                     "wdi_production_long.parquet")
  write_parquet_demo(data.table::data.table(iso3 = countries, year = 2021L, gdp = 1e12, pop = 5e7),
                     "wdi_production_wide.parquet")
  write_parquet_demo(data.table::data.table(iso3 = countries), "macro_country_universe.parquet")
  write_parquet_demo(data.table::data.table(check = "demo", status = "pass"),
                     "production_validation_results.parquet")
  write_parquet_demo(data.table::data.table(check = "demo", status = "pass"),
                     "phase3_validation_results.parquet")
  write_parquet_demo(data.table::data.table(iso3 = countries, ne_name = countries),
                     "geographic_crosswalk.parquet")

  saveRDS(list(demo = TRUE, is_synthetic = TRUE), file.path(dest_dir, "ne_countries_simplified.rds"))

  monthly <- data.table::CJ(
    series_id = paste0("demo_", countries[1:2], "_8517_M"),
    period = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
  )
  monthly[, `:=`(
    reporter_iso3 = ifelse(grepl("DEU", series_id), "DEU", "IND"),
    partner_iso3 = "WLD", hs_code = "8517", flow_code = "M",
    trade_value_usd = as.numeric(100 + seq_len(.N)), is_synthetic = TRUE
  )]
  write_parquet_demo(monthly, "monthly_trade_long.parquet")
  write_parquet_demo(data.table::data.table(series_id = unique(monthly$series_id), ok = TRUE),
                     "monthly_series_quality.parquet")
  write_parquet_demo(data.table::data.table(series_id = unique(monthly$series_id), selected = TRUE),
                     "monthly_trade_candidates.parquet")
  write_parquet_demo(data.table::data.table(series_id = unique(monthly$series_id), selected = TRUE),
                     "forecast_selected_series.parquet")
  write_parquet_demo(data.table::data.table(series_id = unique(monthly$series_id), model = "naive", mape = 0.2),
                     "forecast_model_metrics.parquet")
  write_parquet_demo(data.table::data.table(series_id = unique(monthly$series_id), selected_model = "naive"),
                     "forecast_selected_models.parquet")
  write_parquet_demo(data.table::data.table(series_id = unique(monthly$series_id)[1],
                                            period = as.Date("2020-04-01"), point = 110),
                     "forecast_predictions.parquet")
  write_parquet_demo(data.table::data.table(series_id = unique(monthly$series_id)[1],
                                            period = as.Date("2020-02-01"), actual = 100, pred = 95),
                     "forecast_backtest_predictions.parquet")
  write_parquet_demo(data.table::data.table(series_id = unique(monthly$series_id)[1], residual = 1),
                     "forecast_residual_diagnostics.parquet")
  write_parquet_demo(data.table::data.table(check = "phase12_demo", status = "pass"),
                     "phase12_validation_results.parquet")

  write_json_atomic(list(
    universe_checksum = "uv_demo_synthetic_0001",
    selected_reporters = countries,
    represented_reporters = countries,
    is_synthetic = TRUE,
    demo_label = "Demo or Synthetic Fixture Data"
  ), file.path(dest_dir, "analytical_universe.json"))
  write_json_atomic(list(
    global_status = "complete",
    detailed_status = "partial",
    selected_reporters = 20L,
    represented_reporters = 6L,
    universe_checksum = "uv_demo_synthetic_0001",
    demo_mode = TRUE,
    runtime_bundle_label = "Demo or Synthetic Fixture Data"
  ), file.path(dest_dir, "trade_data_profile.json"))
  write_json_atomic(list(status = "demo", is_synthetic = TRUE),
                    file.path(dest_dir, "macro_data_profile.json"))
  write_json_atomic(list(
    production_status = "partial",
    detailed_production_status = "partial",
    global_production_status = "complete",
    selected_reporter_count = 20L,
    represented_reporter_count = 6L,
    runtime_bundle_label = "Demo or Synthetic Fixture Data"
  ), file.path(dest_dir, "production_pipeline_manifest.json"))
  write_json_atomic(list(
    data_mode = "fixture_synthetic",
    data_source = "synthetic_offline_fixtures",
    production_forecast_available = FALSE,
    live_monthly_successful_requests = 0L,
    mape_claim_below_15 = FALSE,
    demo_label = "Synthetic Fixture Data"
  ), file.path(dest_dir, "forecast_profile.json"))
  write_json_atomic(list(status = "demo"), file.path(dest_dir, "forecast_pipeline_manifest.json"))
  write_json_atomic(list(
    data_mode = "fixture_synthetic",
    data_source = "synthetic_offline_fixtures"
  ), file.path(dest_dir, "forecast_data_provenance.json"))

  normalize_bundle_json_files(dest_dir)
  files <- setdiff(list.files(dest_dir), "release_bundle_manifest.json")
  manifest <- build_release_bundle_manifest(dest_dir, files, profile = "demo", root = root)
  write_json_atomic(manifest, file.path(dest_dir, "release_bundle_manifest.json"))
  normalize_json_file_lf(file.path(dest_dir, "release_bundle_manifest.json"))
  list(dest_dir = dest_dir, files = c(files, "release_bundle_manifest.json"),
       manifest = manifest,
       manifest_path = file.path(dest_dir, "release_bundle_manifest.json"))
}
