persist_performance_outputs <- function(runs_dt,
                                          memory_dt = NULL,
                                          reactive_dt = NULL,
                                          payload_dt = NULL,
                                          validation_dt = NULL,
                                          comparison_dt = NULL,
                                          cfg = normalise_performance_config(),
                                          claim_250 = NULL) {
  paths <- ensure_performance_dirs(cfg)
  env <- capture_benchmark_environment(cfg)
  runs_dt <- data.table::as.data.table(runs_dt)
  phase <- cfg$phase_label %||% "baseline"

  atomic_write_parquet_dt(runs_dt, file.path(paths$results, paste0("benchmark_runs_", phase, ".parquet")))

  existing <- safe_read_parquet_dt(file.path(paths$results, "benchmark_runs.parquet"))
  if (!is.null(existing) && nrow(existing)) {
    existing <- existing[phase != runs_dt$phase[1]]
    runs_all <- data.table::rbindlist(list(existing, runs_dt), fill = TRUE)
  } else {
    runs_all <- runs_dt
  }
  atomic_write_parquet_dt(runs_all, file.path(paths$results, "benchmark_runs.parquet"))

  summary <- runs_dt[, .(
    n = .N,
    median_of_medians_ms = stats::median(median_ms, na.rm = TRUE),
    median_of_p95_ms = stats::median(p95_ms, na.rm = TRUE),
    n_ok = sum(benchmark_status == "ok"),
    n_error = sum(benchmark_status == "error"),
    n_skipped = sum(benchmark_status %in% c("skipped", "unavailable"))
  ), by = .(phase, module, dataset_tier)]
  atomic_write_parquet_dt(summary, file.path(paths$results, "benchmark_summary.parquet"))

  if (!is.null(comparison_dt) && nrow(comparison_dt)) {
    atomic_write_parquet_dt(comparison_dt, file.path(paths$results, "benchmark_comparison.parquet"))
  }
  if (!is.null(memory_dt)) {
    atomic_write_parquet_dt(data.table::as.data.table(memory_dt),
                            file.path(paths$results, "memory_profile.parquet"))
  }
  if (!is.null(reactive_dt)) {
    atomic_write_parquet_dt(data.table::as.data.table(reactive_dt),
                            file.path(paths$results, "reactive_profile.parquet"))
  }
  if (!is.null(payload_dt)) {
    atomic_write_parquet_dt(data.table::as.data.table(payload_dt),
                            file.path(paths$results, "payload_profile.parquet"))
  }
  if (!is.null(validation_dt)) {
    atomic_write_parquet_dt(data.table::as.data.table(validation_dt),
                            file.path(paths$results, "performance_validation.parquet"))
  }

  profile <- strip_unsafe_metadata_paths(list(
    engine_version = PERF_ENGINE_VERSION,
    phase_label = phase,
    generated_at = utc_now(),
    environment = env,
    config = cfg[setdiff(names(cfg), "output_root")],
    claim_250ms = claim_250 %||% list(supported = FALSE, reason = "not_evaluated"),
    browser_rendering_measured = FALSE,
    note = "Server-side calculation timings only unless browser automation is explicitly enabled."
  ))
  write_json_atomic(profile, file.path(paths$results, "performance_profile.json"))

  manifest <- list(
    engine_version = PERF_ENGINE_VERSION,
    generated_at = utc_now(),
    phase_label = phase,
    files = list(
      runs = "benchmark_runs.parquet",
      summary = "benchmark_summary.parquet",
      validation = "performance_validation.parquet",
      profile = "performance_profile.json",
      report = "../reports/phase13_performance_report.md"
    ),
    contains_credentials = FALSE,
    git_head = env$git_head
  )
  write_json_atomic(manifest, file.path(paths$results, "performance_manifest.json"))

  report <- build_performance_report(runs_all, summary, comparison_dt, validation_dt, profile, claim_250)
  report_path <- file.path(paths$reports, "phase13_performance_report.md")
  writeLines(report, report_path, useBytes = TRUE)
  invisible(list(paths = paths, profile = profile, manifest = manifest, report_path = report_path))
}

build_performance_report <- function(runs, summary, comparison, validation, profile, claim_250) {
  runs <- data.table::as.data.table(runs)
  lines <- c(
    "# Phase 13 performance report",
    "",
    "Generated automatically. Server-side calculation timings unless noted.",
    "**Browser rendering was not measured.**",
    "",
    sprintf("- Engine: %s", profile$engine_version %||% PERF_ENGINE_VERSION),
    sprintf("- Generated at: %s", profile$generated_at %||% utc_now()),
    sprintf("- Git HEAD: %s", profile$environment$git_head %||% "unknown"),
    sprintf("- R: %s", profile$environment$R_version %||% "unknown"),
    sprintf("- Platform: %s", profile$environment$platform %||% "unknown"),
    sprintf("- CPUs: %s", profile$environment$logical_cpus %||% "unknown"),
    sprintf("- Universe: %s", profile$config$universe_version %||% "unknown"),
    sprintf("- Fixture version: %s", profile$config$fixture_version %||% "unknown"),
    sprintf("- Iterations / warmup: %s / %s",
            profile$config$iterations %||% NA, profile$config$warmup_iterations %||% NA),
    "",
    "## Claim policy",
    "",
    if (isTRUE(claim_250$supported)) {
      sprintf(
        "Shock p95 statistic **%.3f ms** supports an under-250 ms statement for the defined headline scenarios (%s).",
        claim_250$statistic, claim_250$reason
      )
    } else {
      sprintf(
        "No under-250 ms shock claim is made. Evidence: %s (statistic=%s).",
        claim_250$reason %||% "not_evaluated",
        as.character(claim_250$statistic %||% NA)
      )
    },
    "",
    "Forecast fixture timings are **not** production monthly forecasting performance.",
    "",
    "## Tier status",
    "",
    "- Tier 1 actual processed data: measured",
    "- Tier 2 synthetic scaled fixtures: measured when generated",
    "- Tier 3 complete production (20/20): **Not available — detailed production coverage remains 6/20.**",
    "",
    "## Summary by module",
    ""
  )
  if (!is.null(summary) && nrow(summary)) {
    for (i in seq_len(nrow(summary))) {
      r <- summary[i]
      lines <- c(lines, sprintf(
        "- `%s` / %s / %s: median-of-medians=%.3f ms, median-of-p95=%.3f ms (ok=%s)",
        r$phase, r$module, r$dataset_tier, r$median_of_medians_ms, r$median_of_p95_ms, r$n_ok
      ))
    }
  }
  lines <- c(lines, "", "## Selected operations", "")
  if (nrow(runs)) {
    show <- runs[benchmark_status == "ok"]
    if (nrow(show)) {
      for (i in seq_len(min(40L, nrow(show)))) {
        r <- show[i]
        lines <- c(lines, sprintf(
          "- [%s] %s::%s (%s, %s): median=%.3f ms, p95=%.3f ms, n=%s, rows=%s",
          r$phase, r$module, r$operation, r$dataset_tier, r$cold_or_warm,
          r$median_ms, r$p95_ms, r$iterations, r$input_rows
        ))
      }
    }
  }
  if (!is.null(comparison) && nrow(comparison)) {
    lines <- c(lines, "", "## Baseline versus optimised", "")
    for (i in seq_len(min(30L, nrow(comparison)))) {
      r <- comparison[i]
      lines <- c(lines, sprintf(
        "- %s::%s: baseline median=%.3f -> optimised=%.3f (%.1f%%), checksum_match=%s",
        r$module, r$operation, r$median_ms_baseline, r$median_ms_optimised,
        r$median_improvement_pct %||% NA_real_, r$checksum_match
      ))
    }
  }
  if (!is.null(validation) && nrow(validation)) {
    lines <- c(lines, "", "## Validation", "")
    for (i in seq_len(nrow(validation))) {
      r <- validation[i]
      lines <- c(lines, sprintf("- %s: %s", r$check_id, r$status))
    }
  }
  lines <- c(
    lines, "",
    "## Limitations",
    "",
    "- Timings are machine-specific and not portable SLA guarantees.",
    "- Object sizes are not process peak RSS.",
    "- Leaflet/Plotly browser paint time was not measured.",
    "- Synthetic fixtures do not represent real trade values.",
    ""
  )
  paste(lines, collapse = "\n")
}

load_performance_summary_for_ui <- function(cfg = load_config()) {
  path <- file.path("data", "performance", "results", "performance_profile.json")
  if (!file.exists(path)) {
    return(list(available = FALSE, message = "No Phase 13 benchmark results available."))
  }
  prof <- tryCatch(safe_read_json(path), error = function(e) NULL)
  if (is.null(prof)) {
    return(list(available = FALSE, message = "Benchmark profile unreadable."))
  }
  list(
    available = TRUE,
    generated_at = prof$generated_at,
    mode = prof$config$benchmark_mode %||% "unknown",
    phase = prof$phase_label %||% "unknown",
    claim_250_supported = isTRUE(prof$claim_250ms$supported),
    browser_measured = isTRUE(prof$browser_rendering_measured),
    message = "Server-side benchmark summary only."
  )
}
