RELEASE_BUNDLE_VERSION <- "14.0.0-rc"
RELEASE_OUTPUT_ROOT_REL <- "data/release"

release_env_allowlist <- function() {
  c(
    "GTSC_RUNTIME_PROFILE",
    "GTSC_DATA_ROOT",
    "GTSC_SCENARIO_ROOT",
    "GTSC_PERFORMANCE_ROOT",
    "GTSC_HOST",
    "GTSC_PORT",
    "GTSC_LOG_LEVEL",
    "GTSC_PUBLIC_MODE",
    "GTSC_ALLOW_SCENARIO_WRITES",
    "GTSC_READ_ONLY_MODE",
    "GTSC_PREFER_RDS",
    "GTSC_RDS_ROOT",
    "GTSC_ENABLE_TECHNICAL_DIAGNOSTICS",
    "GTSC_RELEASE_MANIFEST",
    "GTSC_HEALTHCHECK_PATH",
    "GTSC_STARTUP_TIMEOUT_SECONDS",
    "GTSC_RELEASE_MODE",
    "GTSC_RELEASE_PROFILE",
    "GTSC_RELEASE_BUILD_IMAGE",
    "GTSC_RELEASE_RUN_CONTAINER",
    "GTSC_RELEASE_SECURITY_SCAN",
    "GTSC_RELEASE_IMAGE_TAG",
    "GTSC_RELEASE_OUTPUT_DIR",
    "GTSC_INCLUDE_SCENARIO_HISTORY",
    "GTE_ENV",
    "R_LIBS_USER",
    "R_LIBS_SITE",
    "HOME",
    "TMPDIR",
    "TMP",
    "TEMP",
    "PATH",
    "LANG",
    "LC_ALL",
    "TZ"
  )
}

release_forbidden_env <- function() {
  c("COMTRADE_PRIMARY", "COMTRADE_SECONDARY", "COMTRADE_API_KEY")
}

release_bundle_allowlist <- function() {
  c(
    "analytical_universe.json",
    "trade_data_profile.json",
    "macro_data_profile.json",
    "production_pipeline_manifest.json",
    "country_year_analytics.parquet",
    "trade_global_hs85_annual.parquet",
    "trade_global_enriched.parquet",
    "trade_detailed_top20.parquet",
    "trade_detailed_enriched.parquet",
    "top_reporters.parquet",
    "top_partners.parquet",
    "top_hs4.parquet",
    "wdi_production_long.parquet",
    "wdi_production_wide.parquet",
    "macro_country_universe.parquet",
    "production_validation_results.parquet",
    "phase3_validation_results.parquet",
    "ne_countries_simplified.rds",
    "geographic_crosswalk.parquet",
    "monthly_trade_long.parquet",
    "monthly_series_quality.parquet",
    "monthly_trade_candidates.parquet",
    "forecast_selected_series.parquet",
    "forecast_model_metrics.parquet",
    "forecast_selected_models.parquet",
    "forecast_predictions.parquet",
    "forecast_backtest_predictions.parquet",
    "forecast_residual_diagnostics.parquet",
    "forecast_profile.json",
    "forecast_pipeline_manifest.json",
    "forecast_data_provenance.json",
    "phase12_validation_results.parquet",
    "release_bundle_manifest.json"
  )
}

release_bundle_exclude_patterns <- function() {
  c(
    "^\\.Renviron$",
    "COMTRADE",
    "raw/",
    "interim/",
    "cache/",
    "\\.git",
    "request_headers",
    "subscription",
    "api[_-]?key",
    "pipeline_lock",
    "\\.running$",
    "benchmark_",
    "performance/",
    "scenarios/results",
    "renv/library",
    "\\.Rhistory$",
    "\\.RData$"
  )
}

release_paths <- function(root = find_project_root(),
                          output_rel = Sys.getenv("GTSC_RELEASE_OUTPUT_DIR", RELEASE_OUTPUT_ROOT_REL)) {
  base <- validate_release_output_root(output_rel, root)
  list(
    root = root,
    output = base,
    demo = file.path(base, "demo"),
    current = file.path(base, "current"),
    manifests = file.path(base, "manifests"),
    inventories = file.path(base, "inventories"),
    validation = file.path(base, "validation"),
    logs = file.path(base, "logs")
  )
}

validate_release_output_root <- function(path, root = find_project_root()) {
  p <- as.character(path %||% "")
  if (!nzchar(p)) stop("Release output directory is empty.", call. = FALSE)
  if (grepl("\\.\\.", p)) stop("Unsafe release output path (parent traversal).", call. = FALSE)
  abs_root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  abs_root <- gsub("\\\\", "/", abs_root)

  abs_p <- normalizePath(resolve_project_path(p, abs_root),
                         winslash = "/", mustWork = FALSE)
  abs_p <- gsub("\\\\", "/", abs_p)

  under_proj <- startsWith(tolower(abs_p), tolower(abs_root))
  under_opt <- grepl("^/opt/gtsc(/|$)", abs_p, ignore.case = TRUE) ||
    grepl("^[A-Za-z]:/opt/gtsc(/|$)", abs_p, ignore.case = TRUE)
  under_release <- grepl("/data/release(/|$)", abs_p, ignore.case = TRUE)
  if (!(under_proj || under_opt || under_release)) {
    stop("Rejected unsafe release output root: ", basename(abs_p), call. = FALSE)
  }
  invisible(abs_p)
}

ensure_release_dirs <- function(paths = release_paths()) {
  for (d in c(paths$output, paths$demo, paths$current, paths$manifests,
              paths$inventories, paths$validation, paths$logs)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(paths)
}

file_sha256 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest <- tryCatch({
    if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(file = path, algo = "sha256")
    } else {

      NA_character_
    }
  }, error = function(e) NA_character_)
  if (!is.na(digest)) return(digest)

  if (requireNamespace("openssl", quietly = TRUE)) {
    return(as.character(openssl::sha256(file(path, "rb"))))
  }

  if (.Platform$OS.type == "windows") {
    out <- tryCatch(
      system2("certutil", c("-hashfile", path, "SHA256"), stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    hit <- out[grepl("^[0-9A-Fa-f]{64}$", out)]
    if (length(hit)) return(tolower(hit[[1]]))
  } else {
    out <- tryCatch(
      system2("sha256sum", path, stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    if (length(out)) {
      tok <- strsplit(out[[1]], "\\s+")[[1]][1]
      if (grepl("^[0-9a-f]{64}$", tok)) return(tok)
    }
  }
  stop("Unable to compute SHA-256 for ", basename(path), call. = FALSE)
}

git_release_metadata <- function(root = find_project_root()) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  git_out <- function(args) {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    ok <- tryCatch({
      setwd(root)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) return(character())
    tryCatch(
      system2("git", args, stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
  }
  head <- git_out(c("rev-parse", "HEAD"))
  if (!length(head) || !nzchar(head[[1]]) || grepl("^fatal", head[[1]])) {
    head <- NA_character_
  } else {
    head <- head[[1]]
  }
  dirty <- tryCatch({
    st <- git_out(c("status", "--porcelain"))
    length(st) > 0L && !any(grepl("^fatal", st))
  }, error = function(e) NA)
  branch <- git_out(c("rev-parse", "--abbrev-ref", "HEAD"))
  if (!length(branch) || !nzchar(branch[[1]]) || grepl("^fatal", branch[[1]])) {
    branch <- NA_character_
  } else {
    branch <- branch[[1]]
  }
  list(
    git_head = head,
    git_branch = branch,
    dirty_working_tree = isTRUE(dirty),
    application_version = RELEASE_APP_VERSION,
    release_bundle_version = RELEASE_BUNDLE_VERSION,
    shock_engine_version = if (exists("SHOCK_ENGINE_VERSION")) SHOCK_ENGINE_VERSION else "phase11+",
    forecast_engine_version = if (exists("FORECAST_ENGINE_VERSION")) FORECAST_ENGINE_VERSION else "phase12+",
    release_candidate_id = paste0("rc14-", substr(as.character(head %||% "unknown"), 1, 8))
  )
}
