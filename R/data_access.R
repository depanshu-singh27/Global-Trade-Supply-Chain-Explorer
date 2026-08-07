.gte_paths <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- load_config()
  list(
    trade = file.path(cfg[['paths']]$processed, "trade_pilot.parquet"),
    wdi_long = file.path(cfg[['paths']]$processed, "wdi_pilot_long.parquet"),
    wdi_wide = file.path(cfg[['paths']]$processed, "wdi_pilot_wide.parquet"),
    countries = file.path(cfg[['paths']]$processed, "country_reference.parquet"),
    summary = file.path(cfg[['paths']]$processed, "pilot_country_summary.parquet"),
    validation = file.path(cfg[['paths']]$processed, "validation_results.parquet"),
    manifest = file.path(cfg[['paths']]$processed, "pipeline_manifest.json"),

    trade_global = file.path(cfg[['paths']]$processed, "trade_global_hs85_annual.parquet"),
    trade_detailed = file.path(cfg[['paths']]$processed, "trade_detailed_top20.parquet"),
    trade_global_enriched = file.path(cfg[['paths']]$processed, "trade_global_enriched.parquet"),
    trade_detailed_enriched = file.path(cfg[['paths']]$processed, "trade_detailed_enriched.parquet"),
    wdi_production_long = file.path(cfg[['paths']]$processed, "wdi_production_long.parquet"),
    wdi_production_wide = file.path(cfg[['paths']]$processed, "wdi_production_wide.parquet"),
    country_year_analytics = file.path(cfg[['paths']]$processed, "country_year_analytics.parquet"),
    phase3_validation = file.path(cfg[['paths']]$processed, "phase3_validation_results.parquet"),
    production_manifest = file.path(cfg[['paths']]$processed, "production_pipeline_manifest.json"),
    macro_profile = file.path(cfg[['paths']]$processed, "macro_data_profile.json"),
    trade_profile = file.path(cfg[['paths']]$processed, "trade_data_profile.json"),
    production_validation = file.path(cfg[['paths']]$processed, "production_validation_results.parquet"),
    top_reporters = file.path(cfg[['paths']]$processed, "top_reporters.parquet"),
    top_partners = file.path(cfg[['paths']]$processed, "top_partners.parquet"),
    top_hs4 = file.path(cfg[['paths']]$processed, "top_hs4.parquet"),
    analytical_universe = file.path(cfg[['paths']]$processed, "analytical_universe.json"),
    macro_country_universe = file.path(cfg[['paths']]$processed, "macro_country_universe.parquet"),
    map_geometry = file.path(cfg[['paths']]$processed, MAP_GEOMETRY_FILENAME),
    geographic_crosswalk = file.path(cfg[['paths']]$processed, MAP_CROSSWALK_FILENAME)
  )
}

.gtsc_data_backend_state <- new.env(parent = emptyenv())

log_runtime_data_backend_once <- function(backend) {
  if (isTRUE(.gtsc_data_backend_state$logged)) {
    return(invisible(NULL))
  }

  .gtsc_data_backend_state$logged <- TRUE

  if (exists("runtime_log", mode = "function")) {
    runtime_log(
      "INFO",
      "runtime_data_backend",
      list(backend = backend)
    )
  }

  invisible(NULL)
}

safe_read_parquet_dt <- function(path) {
  prefer_rds <- identical(
    tolower(Sys.getenv("GTSC_PREFER_RDS", "false")),
    "true"
  )

  rds_root <- Sys.getenv(
    "GTSC_RDS_ROOT",
    "/opt/gtsc/runtime-rds"
  )

  rds_path <- file.path(
    rds_root,
    sub(
      "\\.parquet$",
      ".rds",
      basename(path),
      ignore.case = TRUE
    )
  )

  if (prefer_rds && file.exists(rds_path)) {
    log_runtime_data_backend_once("rds")

    return(tryCatch(
      data.table::as.data.table(readRDS(rds_path)),
      error = function(e) {
        warning(
          "Failed to read ",
          basename(rds_path),
          ": ",
          conditionMessage(e),
          call. = FALSE
        )
        NULL
      }
    ))
  }

  if (!file.exists(path)) {
    return(NULL)
  }

  log_runtime_data_backend_once("parquet")

  tryCatch(
    data.table::as.data.table(arrow::read_parquet(path)),
    error = function(e) {
      warning(
        "Failed to read ",
        basename(path),
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
}

load_processed_snapshot <- function(cfg = load_config()) {
  paths <- .gte_paths(cfg)
  production_manifest <- safe_read_json(paths$production_manifest)
  macro_profile <- safe_read_json(paths$macro_profile)
  trade_profile <- safe_read_json(paths$trade_profile)
  analytical_universe <- safe_read_json(paths$analytical_universe)
  compact_runtime_data <- (
    identical(
      tolower(Sys.getenv("GTSC_PREFER_RDS", "false")),
      "true"
    ) &&
    identical(
      tolower(Sys.getenv("GTSC_READ_ONLY_MODE", "false")),
      "true"
    )
  )

  trade_global <- safe_read_parquet_dt(paths$trade_global)
  trade_global_enriched <- safe_read_parquet_dt(paths$trade_global_enriched)
  wdi_production_wide <- safe_read_parquet_dt(paths$wdi_production_wide)
  country_year_analytics <- safe_read_parquet_dt(paths$country_year_analytics)

  trade_detailed_enriched <- safe_read_parquet_dt(
    paths$trade_detailed_enriched
  )

  trade_detailed <- NULL

  if (is.null(trade_detailed_enriched) ||
      !nrow(trade_detailed_enriched)) {
    trade_detailed <- safe_read_parquet_dt(
      paths$trade_detailed
    )
    trade_detailed_enriched <- trade_detailed
  } else if (!compact_runtime_data) {
    trade_detailed <- safe_read_parquet_dt(
      paths$trade_detailed
    )
  }

  detailed_for_explorer <- if (
    !is.null(trade_detailed_enriched) &&
    nrow(trade_detailed_enriched)
  ) {
    tryCatch({

      prep_input <- if (compact_runtime_data) {
        trade_detailed_enriched
      } else {
        data.table::copy(trade_detailed_enriched)
      }

      prep <- prepare_detailed_trade(prep_input)

      if (nrow(prep) &&
          all(
            c(
              "year",
              "flow_code",
              "reporter_iso3",
              "hs_code"
            ) %in% names(prep)
          )) {
        data.table::setindexv(
          prep,
          c("year", "flow_code")
        )
        data.table::setindexv(
          prep,
          c(
            "year",
            "reporter_iso3",
            "hs_code"
          )
        )
      }

      prep
    }, error = function(e) {
      warning(
        "Detailed trade prepare failed: ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    })
  } else {
    NULL
  }

  if (compact_runtime_data &&
      !is.null(detailed_for_explorer) &&
      nrow(detailed_for_explorer)) {
    trade_detailed_enriched <- detailed_for_explorer
    trade_detailed <- detailed_for_explorer

    if (exists("runtime_log", mode = "function")) {
      runtime_log(
        "INFO",
        "runtime_detailed_compaction",
        list(
          rows = nrow(detailed_for_explorer),
          columns = ncol(detailed_for_explorer),
          shared_snapshot_object = TRUE
        )
      )
    }
  }

  snap_partial <- list(
    production_manifest = production_manifest,
    analytical_universe = analytical_universe,
    trade_detailed_enriched = detailed_for_explorer,
    trade_detailed = trade_detailed,
    top_reporters = safe_read_parquet_dt(paths$top_reporters),
    production_validation = safe_read_parquet_dt(paths$production_validation),
    phase3_validation = safe_read_parquet_dt(paths$phase3_validation),
    pipeline_status = list(
      detailed_trade = production_manifest$production_status %||% "absent"
    )
  )

  detailed_coverage <- if (exists("trade_flow_coverage_status", mode = "function")) {
    trade_flow_coverage_status(snap_partial)
  } else {
    list()
  }

  map_analytics <- if (!is.null(country_year_analytics) && nrow(country_year_analytics) &&
                       exists("prepare_map_analytics", mode = "function")) {
    tryCatch(prepare_map_analytics(data.table::copy(country_year_analytics)),
             error = function(e) {
               warning("Map analytics prepare failed: ", conditionMessage(e), call. = FALSE)
               country_year_analytics
             })
  } else {
    country_year_analytics
  }

  map_geometry <- if (exists("load_map_geometry", mode = "function")) {
    tryCatch(load_map_geometry(cfg), error = function(e) {
      warning("Map geometry load failed: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  } else {
    NULL
  }

  geographic_crosswalk <- NULL
  if (!is.null(map_analytics) && nrow(map_analytics) &&
      exists("build_geographic_crosswalk", mode = "function")) {
    geographic_crosswalk <- build_geographic_crosswalk(
      unique(map_analytics$reporter_iso3), map_geometry
    )
    read_only_mode <- identical(
      tolower(Sys.getenv("GTSC_READ_ONLY_MODE", "false")),
      "true"
    )

    if (!read_only_mode &&
        exists("persist_geographic_crosswalk", mode = "function")) {
      persist_geographic_crosswalk(geographic_crosswalk, cfg)
    }
  }

  list(
    available = file.exists(paths$trade),
    overview_available = !is.null(country_year_analytics) && nrow(country_year_analytics) > 0,
    trade_flows_available = !is.null(detailed_for_explorer) && nrow(detailed_for_explorer) > 0,
    map_available = !is.null(map_analytics) && nrow(map_analytics) > 0,
    paths = paths,
    trade = safe_read_parquet_dt(paths$trade),
    wdi_long = safe_read_parquet_dt(paths$wdi_long),
    wdi_wide = safe_read_parquet_dt(paths$wdi_wide),
    countries = {
      ctry <- safe_read_parquet_dt(paths$countries)
      if (is.null(ctry)) build_country_reference() else ctry
    },
    summary = safe_read_parquet_dt(paths$summary),
    validation = safe_read_parquet_dt(paths$validation),
    manifest = safe_read_json(paths$manifest),
    trade_global = trade_global,
    trade_detailed = trade_detailed,
    trade_global_enriched = trade_global_enriched,
    trade_detailed_enriched = detailed_for_explorer,
    wdi_production_long = safe_read_parquet_dt(paths$wdi_production_long),
    wdi_production_wide = wdi_production_wide,
    country_year_analytics = country_year_analytics,
    map_analytics = map_analytics,
    map_geometry = map_geometry,
    geographic_crosswalk = geographic_crosswalk,
    macro_country_universe = safe_read_parquet_dt(paths$macro_country_universe),
    phase3_validation = snap_partial$phase3_validation,
    production_validation = snap_partial$production_validation,
    top_reporters = snap_partial$top_reporters,
    top_partners = safe_read_parquet_dt(paths$top_partners),
    top_hs4 = safe_read_parquet_dt(paths$top_hs4),
    production_manifest = production_manifest,
    macro_profile = macro_profile,
    trade_profile = trade_profile,
    analytical_universe = analytical_universe,
    detailed_coverage = detailed_coverage,
    detailed_represented_reporters = detailed_coverage$represented_reporters %||% character(),
    detailed_missing_reporters = detailed_coverage$missing_reporters %||% character(),
    pipeline_status = list(
      global_trade = if (!is.null(trade_global) && nrow(trade_global)) "complete" else "absent",
      detailed_trade = production_manifest$production_status %||% (
        if (!is.null(trade_detailed) && nrow(trade_detailed)) "partial_or_unknown" else "absent"
      ),
      macro = if (!is.null(wdi_production_wide) && nrow(wdi_production_wide)) "complete" else "absent"
    ),
    loaded_at = utc_now()
  )
}

MANDATORY_SNAPSHOT_TABLES <- c(
  "country_year_analytics",
  "trade_global",
  "trade_global_enriched",
  "trade_detailed",
  "top_reporters",
  "top_partners",
  "top_hs4",
  "wdi_production_long",
  "wdi_production_wide"
)

snapshot_table_report <- function(snap) {
  if (!is.list(snap)) return(data.table::data.table())
  tabular <- vapply(snap, function(x) is.data.frame(x), logical(1))
  nms <- names(snap)[tabular]
  if (!length(nms)) return(data.table::data.table())
  data.table::data.table(
    table = nms,
    rows = vapply(nms, function(nm) as.integer(nrow(snap[[nm]])), integer(1)),
    cols = vapply(nms, function(nm) as.integer(ncol(snap[[nm]])), integer(1))
  )[order(table)]
}

assert_mandatory_snapshot_tables <- function(snap,
                                             required = MANDATORY_SNAPSHOT_TABLES) {
  missing <- character()
  empty <- character()
  for (nm in required) {
    x <- snap[[nm]]
    if (is.null(x) || !is.data.frame(x)) {
      missing <- c(missing, nm)
    } else if (nrow(x) == 0L) {
      empty <- c(empty, nm)
    }
  }
  if (length(missing) || length(empty)) {
    stop(
      "Mandatory snapshot tables unusable. Missing: ",
      if (length(missing)) paste(missing, collapse = ", ") else "<none>",
      ". Empty: ",
      if (length(empty)) paste(empty, collapse = ", ") else "<none>",
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.gtsc_snapshot_cache <- new.env(parent = emptyenv())

app_snapshot <- function(cfg = load_config(), refresh = FALSE) {
  key <- as.character(cfg[["paths"]]$processed %||% "")
  if (!isTRUE(refresh) &&
      identical(.gtsc_snapshot_cache$key, key) &&
      !is.null(.gtsc_snapshot_cache$value)) {
    return(.gtsc_snapshot_cache$value)
  }
  started <- Sys.time()
  snap <- load_processed_snapshot(cfg)
  .gtsc_snapshot_cache$key <- key
  .gtsc_snapshot_cache$value <- snap
  .gtsc_snapshot_cache$duration_seconds <-
    as.numeric(difftime(Sys.time(), started, units = "secs"))
  snap
}

app_snapshot_load_seconds <- function() {
  .gtsc_snapshot_cache$duration_seconds %||% NA_real_
}

reset_app_snapshot_cache <- function() {
  rm(list = ls(.gtsc_snapshot_cache), envir = .gtsc_snapshot_cache)
  invisible(TRUE)
}

overview_metrics <- function(snap, cfg = load_config()) {
  trade <- snap$trade
  list(
    project_name = cfg$app$name,
    hs_chapter = as.character(cfg$pilot$hs_chapter),
    reporters = cfg$pilot$reporters,
    partners = cfg$pilot$partners,
    years = seq.int(cfg$pilot$start_year, cfg$pilot$end_year),
    data_available = isTRUE(snap$available) && !is.null(trade) && nrow(trade) > 0,
    n_trade_rows = if (!is.null(trade)) nrow(trade) else 0L,
    n_reporters = if (!is.null(trade) && nrow(trade)) {
      data.table::uniqueN(trade$reporter_iso3)
    } else {
      0L
    },
    n_partners = if (!is.null(trade) && nrow(trade)) {
      data.table::uniqueN(trade$partner_iso3)
    } else {
      0L
    },
    total_trade_value = if (!is.null(trade) && nrow(trade)) {
      sum(trade$trade_value_usd, na.rm = TRUE)
    } else {
      0
    },
    latest_ingested_at = if (!is.null(trade) && nrow(trade) &&
                             "ingested_at" %in% names(trade)) {
      max(trade$ingested_at, na.rm = TRUE)
    } else {
      NA_character_
    },
    instruction = "Run Rscript scripts/run_pilot_pipeline.R to generate the pilot dataset.",
    pipeline_status = snap$pipeline_status %||% list()
  )
}

validation_summary <- function(snap) {

  v1 <- snap$validation
  v3 <- snap$phase3_validation
  parts <- list()
  if (!is.null(v1) && nrow(v1)) parts[[length(parts) + 1L]] <- v1
  if (!is.null(v3) && nrow(v3)) parts[[length(parts) + 1L]] <- v3
  v <- if (length(parts)) data.table::rbindlist(parts, fill = TRUE) else data.table::data.table()
  if (!nrow(v)) {
    return(list(
      available = FALSE,
      n_pass = 0L, n_warning = 0L, n_error = 0L,
      table = data.table::data.table()
    ))
  }
  list(
    available = TRUE,
    n_pass = sum(v$status == "pass", na.rm = TRUE),
    n_warning = sum(v$status == "warning", na.rm = TRUE),
    n_error = sum(v$status == "error", na.rm = TRUE),
    table = v
  )
}
