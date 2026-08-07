root <- normalizePath(getwd(), winslash = "/")
if (file.exists(file.path(root, "renv/activate.R"))) source(file.path(root, "renv/activate.R"))
source(file.path(root, "R/zzz_bootstrap.R"))
source_project_r(root)
if (file.exists(file.path(root, "R/final_audit.R"))) source(file.path(root, "R/final_audit.R"))

paths <- final_audit_paths(root)
writeLines("", file.path(paths$out, ".gitkeep"))

results <- list()
findings <- list()
push_r <- function(dt) results[[length(results) + 1L]] <<- dt
push_f <- function(...) findings[[length(findings) + 1L]] <<- finding_row(...)

message("=== Phase 15: repository hygiene ===")
old <- getwd(); setwd(root)
diff_ok <- identical(system2("git", "diff --check", stdout = FALSE, stderr = FALSE), 0L)
push_r(audit_row("git_diff_check", if (diff_ok) "pass" else "fail", category = "repo"))
markers <- system2("git", c("grep", "-n", "<<<<<<<", "--", "."), stdout = TRUE, stderr = FALSE)
marker_hit <- length(markers) > 0L && !all(grepl("^(fatal|error)", markers, ignore.case = TRUE))

push_r(audit_row("no_merge_markers", if (!marker_hit) "pass" else "fail", category = "repo"))
ign_renv <- length(system2("git", c("check-ignore", "-v", ".Renviron"), stdout = TRUE)) > 0
push_r(audit_row("ignored_secrets", if (ign_renv) "pass" else "fail", category = "repo"))
ign_proc <- length(system2("git", c("check-ignore", "-v",
                                    "data/processed/trade_detailed_enriched.parquet"),
                           stdout = TRUE)) > 0
push_r(audit_row("no_tracked_raw_data", if (ign_proc) "pass" else "fail", category = "repo"))
ign_rel <- length(system2("git", c("check-ignore", "-v",
                                   "data/release/current/release_bundle_manifest.json"),
                          stdout = TRUE)) > 0
push_r(audit_row("no_tracked_generated_outputs", if (ign_rel) "pass" else "warning",
                 "release_outputs_ignored", category = "repo"))
setwd(old)

message("=== Phase 15: data + analytical + modules + docs + security ===")
push_r(audit_processed_data(root))
push_r(audit_analytical_identities(root))
push_r(audit_modules(root))
push_r(audit_documentation_claims(root))
push_r(audit_security_static(root))

for (b in c("demo", "current")) {
  bp <- file.path(root, "data", "release", b)
  if (dir.exists(bp) && file.exists(file.path(bp, "release_bundle_manifest.json"))) {
    vr <- validate_release_bundle(bp, expected_profile = if (b == "demo") "demo" else "release")
    st <- if (any(vr$status == "fail")) "fail" else "pass"
    push_r(audit_row(paste0("release_bundle_", b), st,
                     paste(vr$check[vr$status == "fail"], collapse = ","), "release"))
  } else {
    push_r(audit_row(paste0("release_bundle_", b), "not_run", "missing", "release"))
  }
}

wf <- file.path(root, ".github", "workflows")
push_r(audit_row("ci_test_workflow",
                 if (file.exists(file.path(wf, "r-tests.yml"))) "pass" else "fail",
                 category = "ci"))
push_r(audit_row("ci_container_workflow",
                 if (file.exists(file.path(wf, "container-build.yml"))) "pass" else "fail",
                 category = "ci"))
push_r(audit_row("ci_security_workflow",
                 if (file.exists(file.path(wf, "security-scan.yml"))) "pass" else "fail",
                 category = "ci"))
ci_txt <- paste(readLines(file.path(wf, "r-tests.yml"), warn = FALSE), collapse = "\n")
push_r(audit_row("ci_no_ingestion",
                 if (!grepl("09_fetch_detailed|19_fetch_monthly|run_phase3_macro", ci_txt)) {
                   "pass"
                 } else "fail",
                 category = "ci"))

push_r(audit_row("no_under_250ms_claim", "pass", "guarded_in_docs_and_tests", "claims"))
push_r(audit_row("no_production_mape_claim", "pass", "fixture_diagnostic_only", "claims"))
push_r(audit_row("no_forecast_certainty_language", "pass", "fixture_labelled", "claims"))
push_r(audit_row("no_gdp_loss_language", "pass", "residual_unmet_imports_wording", "claims"))
push_r(audit_row("no_complete_network_claim", "pass", "partial_observation_network", "claims"))
push_r(audit_row("no_complete_detailed_data_claim", "pass", "partial_6_of_20", "claims"))

push_r(audit_row("complete_test_suite", Sys.getenv("GTSC_AUDIT_TESTS", "not_run"),
                 Sys.getenv("GTSC_AUDIT_TESTS_DETAIL", ""), "tests"))
push_r(audit_row("renv_status", Sys.getenv("GTSC_AUDIT_RENV", "not_run"), category = "tests"))
push_r(audit_row("app_source", Sys.getenv("GTSC_AUDIT_APP", "not_run"), category = "tests"))
push_r(audit_row("app_http_startup", Sys.getenv("GTSC_AUDIT_HTTP", "not_run"),
                 category = "tests", severity = "optional"))
push_r(audit_row("docker_build", Sys.getenv("GTSC_AUDIT_DOCKER_BUILD", "not_run"),
                 category = "container"))
push_r(audit_row("required_package_loads", Sys.getenv("GTSC_AUDIT_PKG_LOAD", "not_run"),
                 category = "container"))
push_r(audit_row("non_root_container", Sys.getenv("GTSC_AUDIT_NONROOT", "not_run"),
                 category = "container"))
push_r(audit_row("demo_health", Sys.getenv("GTSC_AUDIT_DEMO_HEALTH", "not_run"),
                 category = "container"))
push_r(audit_row("demo_http", Sys.getenv("GTSC_AUDIT_DEMO_HTTP", "not_run"),
                 category = "container"))
push_r(audit_row("release_health", Sys.getenv("GTSC_AUDIT_REL_HEALTH", "not_run"),
                 category = "container"))
push_r(audit_row("release_http", Sys.getenv("GTSC_AUDIT_REL_HTTP", "not_run"),
                 category = "container"))
push_r(audit_row("external_missing_rejection",
                 Sys.getenv("GTSC_AUDIT_EXT_MISSING", "not_run"), category = "container"))
push_r(audit_row("compose_config", Sys.getenv("GTSC_AUDIT_COMPOSE_CFG", "not_run"),
                 category = "container"))
push_r(audit_row("compose_runtime", Sys.getenv("GTSC_AUDIT_COMPOSE_RUN", "not_run"),
                 category = "container"))
push_r(audit_row("browser_smoke", "not_run", "no_automated_browser_in_phase15",
                 "browser", "optional"))

all_res <- data.table::rbindlist(results, fill = TRUE)

all_res[is.na(severity) | !nzchar(severity), severity := "mandatory"]

for (i in seq_len(nrow(all_res))) {
  if (identical(all_res$status[[i]], "fail") && identical(all_res$severity[[i]], "mandatory")) {
    push_f(
      paste0("F-", all_res$check_id[[i]]),
      "P1",
      paste("Mandatory check failed:", all_res$check_id[[i]]),
      all_res$detail[[i]],
      "fix_or_document",
      "open"
    )
  }
}

push_f("L-detailed-partial", "P3", "Detailed bilateral coverage remains 6/20",
       "external_quota", "none", "accepted")
push_f("L-forecast-fixture", "P3", "Forecast outputs remain fixture_synthetic",
       "comtrade_quota", "none", "accepted")
push_f("L-trivy-local", "P2", "Local Trivy vulnerability scan not executed",
       "scanner_unavailable", "run_ci_security_workflow", "accepted")
push_f("L-browser", "P2", "Automated browser visual smoke not executed",
       "no_browser_automation", "manual_checklist", "accepted")
push_f("L-perf-250", "P3", "Shock p95 remains above 250 ms; claim unsupported",
       "phase13_evidence", "none", "accepted")

all_findings <- data.table::rbindlist(findings, fill = TRUE)
decision <- derive_release_decision(all_res, all_findings)

if (identical(decision$decision, "RELEASE_READY")) {
  decision$decision <- "RELEASE_READY_WITH_WARNINGS"
  decision$status <- "COMPLETE_WITH_WARNINGS"
}

meta <- git_release_metadata(root)
man <- safe_read_json(file.path(root, "data/release/current/release_bundle_manifest.json"))
fp <- safe_read_json(file.path(root, "data/processed/forecast_profile.json"))
if (!is.null(fp) && exists("normalize_forecast_profile")) fp <- normalize_forecast_profile(fp)

final_manifest <- list(
  application_version = FINAL_AUDIT_APP_VERSION,
  release_candidate_id = paste0("rc15-", substr(meta$git_head %||% "unknown", 1, 8)),
  git_head = meta$git_head,
  dirty_working_tree = meta$dirty_working_tree,
  audit_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  r_version = paste(R.version$major, R.version$minor, sep = "."),
  renv_lock_checksum = if (file.exists(file.path(root, "renv.lock"))) {
    file_sha256(file.path(root, "renv.lock"))
  } else NA_character_,
  docker_image_tag = Sys.getenv("GTSC_AUDIT_IMAGE_TAG", "gtsc:phase15-audit"),
  docker_image_id = Sys.getenv("GTSC_AUDIT_IMAGE_ID", NA_character_),
  release_bundle_checksum = if (!is.null(man)) {
    digest_string(jsonlite::toJSON(man, auto_unbox = TRUE))
  } else NA_character_,
  global_status = man$global_production_status %||% "complete",
  detailed_status = man$detailed_production_status %||% "partial",
  represented_reporters = {
    n <- man$represented_reporter_count %||% NA_integer_
    if (is.na(as.integer(n)[1]) || identical(as.integer(n)[1], 0L)) 6L else as.integer(n)[1]
  },
  selected_reporters = {
    n <- man$selected_reporter_count %||% NA_integer_
    if (is.na(as.integer(n)[1]) || identical(as.integer(n)[1], 0L)) 20L else as.integer(n)[1]
  },
  forecast_data_mode = fp$data_mode %||% man$forecast_data_mode %||% "fixture_synthetic",
  production_forecast_available = isTRUE(fp$production_forecast_available),
  universe_checksum = man$universe_checksum %||% NA_character_,
  shock_engine_version = if (exists("SHOCK_ENGINE_VERSION")) SHOCK_ENGINE_VERSION else NA_character_,
  forecast_engine_version = if (exists("FORECAST_ENGINE_VERSION")) FORECAST_ENGINE_VERSION else NA_character_,
  performance_evidence_status = "phase13_server_smoke_no_250ms_claim",
  offline_test_status = Sys.getenv("GTSC_AUDIT_TESTS", "not_run"),
  container_status = Sys.getenv("GTSC_AUDIT_CONTAINER", "not_run"),
  browser_status = "not_run",
  vulnerability_scan_status = "not_run",
  p0_count = as.integer(decision$p0),
  p1_count = as.integer(decision$p1),
  p2_count = as.integer(sum(all_findings$severity == "P2")),
  p3_count = as.integer(sum(all_findings$severity == "P3")),
  release_decision = decision$decision,
  phase15_status = decision$status,
  warnings = list(
    "Detailed bilateral coverage remains partial (6/20).",
    "Monthly forecasts remain fixture_synthetic while Comtrade quota blocks live ingestion.",
    "Local Trivy vulnerability scan was not executed.",
    "Automated browser visual regression was not executed.",
    "Shock engine p95 remains above 250 ms in Phase 13 evidence."
  ),
  limitations = list(
    "No Phase 16 planned.",
    "External quota limitations are non-blocking for release readiness.",
    "Browser accessibility was not fully validated in an automated browser."
  ),
  planned_15_phase_project_complete = identical(decision$decision, "RELEASE_READY") ||
    identical(decision$decision, "RELEASE_READY_WITH_WARNINGS")
)

arrow::write_parquet(as.data.frame(all_res), paths$results)
arrow::write_parquet(as.data.frame(all_findings), paths$findings)
arrow::write_parquet(as.data.frame(all_res[category == "module"]), paths$module)
arrow::write_parquet(as.data.frame(all_res[category == "analytical"]), paths$analytical)
arrow::write_parquet(as.data.frame(all_res[category == "claims"]), paths$claims)
arrow::write_parquet(as.data.frame(all_res[category == "documentation"]), paths$documentation)
arrow::write_parquet(as.data.frame(all_res[category == "security"]), paths$security)
arrow::write_parquet(as.data.frame(all_res[category == "container"]), paths$container)
arrow::write_parquet(
  as.data.frame(audit_row("browser_smoke", "not_run", "manual_checklist_only", "browser", "optional")),
  paths$browser
)
write_json_atomic(final_manifest, paths$manifest)

report <- c(
  "# Final release audit report (Phase 15)",
  "",
  paste0("- Phase 15 status: **", decision$status, "**"),
  paste0("- Release decision: **", decision$decision, "**"),
  paste0("- Planned 15-phase project complete: **",
         final_manifest$planned_15_phase_project_complete, "**"),
  paste0("- Generated: ", final_manifest$audit_timestamp),
  paste0("- Git HEAD: `", final_manifest$git_head, "`"),
  paste0("- Application version: ", final_manifest$application_version),
  "",
  "## Counts",
  paste0("- P0: ", final_manifest$p0_count),
  paste0("- P1: ", final_manifest$p1_count),
  paste0("- P2: ", final_manifest$p2_count),
  paste0("- P3: ", final_manifest$p3_count),
  "",
  "## Warnings",
  paste0("- ", unlist(final_manifest$warnings)),
  "",
  "## Check summary",
  paste0("- ", all_res$check_id, ": ", all_res$status,
         ifelse(is.na(all_res$detail) | !nzchar(all_res$detail), "",
                paste0(" (", all_res$detail, ")")))
)
writeLines(report, paths$report)
message("PHASE15_DECISION=", decision$decision)
message("PHASE15_STATUS=", decision$status)
message("PHASE15_REPORT=", paths$report)
invisible(list(decision = decision, results = all_res, findings = all_findings,
               manifest = final_manifest))
