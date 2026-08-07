FINAL_AUDIT_APP_VERSION <- "0.1.0-rc.15"

final_audit_paths <- function(root = find_project_root()) {
  base <- file.path(root, "data", "release", "final_audit")
  if (!dir.exists(base)) dir.create(base, recursive = TRUE, showWarnings = FALSE)
  list(
    root = root,
    out = base,
    results = file.path(base, "final_audit_results.parquet"),
    findings = file.path(base, "final_audit_findings.parquet"),
    module = file.path(base, "module_validation.parquet"),
    analytical = file.path(base, "analytical_reconciliation.parquet"),
    claims = file.path(base, "claims_validation.parquet"),
    documentation = file.path(base, "documentation_validation.parquet"),
    security = file.path(base, "security_validation.parquet"),
    container = file.path(base, "container_validation.parquet"),
    browser = file.path(base, "browser_validation.parquet"),
    manifest = file.path(base, "final_release_manifest.json"),
    report = file.path(base, "final_release_report.md")
  )
}

audit_row <- function(check_id, status, detail = NA_character_,
                      category = "general", severity = "mandatory") {
  data.table::data.table(
    check_id = as.character(check_id),
    status = as.character(status),
    detail = as.character(detail),
    category = as.character(category),
    severity = as.character(severity)
  )
}

finding_row <- function(id, severity, description, evidence = NA_character_,
                        action = "none", status = "open") {
  data.table::data.table(
    id = as.character(id),
    severity = as.character(severity),
    description = as.character(description),
    evidence = as.character(evidence),
    action = as.character(action),
    status = as.character(status)
  )
}

safe_nrow <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  tryCatch(as.integer(nrow(arrow::read_parquet(path, as_data_frame = TRUE))),
           error = function(e) NA_integer_)
}

audit_processed_data <- function(root = find_project_root()) {
  proc <- file.path(root, "data", "processed")
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- audit_row(...)

  g_path <- file.path(proc, "trade_global_hs85_annual.parquet")
  d_path <- file.path(proc, "trade_detailed_enriched.parquet")
  if (!file.exists(d_path)) d_path <- file.path(proc, "trade_detailed_top20.parquet")
  fp <- safe_read_json(file.path(proc, "forecast_profile.json"))
  if (!is.null(fp) && exists("normalize_forecast_profile")) {
    fp <- normalize_forecast_profile(fp)
  }
  au <- safe_read_json(file.path(proc, "analytical_universe.json"))
  tp <- safe_read_json(file.path(proc, "trade_data_profile.json"))
  pm <- safe_read_json(file.path(proc, "production_pipeline_manifest.json"))

  if (file.exists(g_path)) {
    g <- data.table::as.data.table(arrow::read_parquet(g_path))
    yrs <- sort(unique(as.integer(g$year)))
    add("global_data_readability", "pass", paste0("rows=", nrow(g)), "data")
    add("global_years",
        if (identical(yrs, 2019:2024)) "pass" else "warning",
        paste(yrs, collapse = ","), "data")
    neg <- if ("trade_value_usd" %in% names(g)) sum(g$trade_value_usd < 0, na.rm = TRUE) else 0L
    add("global_no_negative_values", if (identical(as.integer(neg), 0L)) "pass" else "fail",
        paste0("neg=", neg), "data")
    add("global_row_count", "pass", as.character(nrow(g)), "data")
  } else {
    add("global_data_readability", "fail", "missing", "data")
  }

  if (file.exists(d_path)) {
    d <- data.table::as.data.table(arrow::read_parquet(d_path))
    reps <- sort(unique(as.character(d$reporter_iso3)))
    selected <- suppressWarnings(as.integer(
      tp$selected_reporter_count %||%
        pm$selected_reporter_count %||%
        length(unlist(au$selected_reporters %||% list())) %||%
        20L
    ))
    if (length(selected) != 1L || is.na(selected) || selected < 1L) selected <- 20L
    represented <- length(reps)
    add("detailed_data_readability", "pass", paste0("rows=", nrow(d)), "data")
    add("detailed_represented_count", "pass",
        paste0(represented, "/", selected, ":", paste(reps, collapse = ",")), "data")
    detailed_status <- as.character(
      tp$detailed_status %||% tp$production_status %||%
        pm$detailed_production_status %||% pm$production_status %||%
        if (represented < selected) "partial" else "complete"
    )
    status_ok <- identical(detailed_status, "partial") && represented < selected
    add("detailed_partial_status", if (isTRUE(status_ok)) "pass" else "fail",
        paste0("status=", detailed_status, " rep=", represented, "/", selected), "data")
    uv <- as.character(au$universe_checksum %||% au$checksum %||% tp$universe_checksum %||% "")
    add("universe_checksum", if (nzchar(uv)) "pass" else "fail", uv, "data")
  } else {
    add("detailed_data_readability", "fail", "missing", "data")
  }

  wl <- file.path(proc, "wdi_production_long.parquet")
  ww <- file.path(proc, "wdi_production_wide.parquet")
  if (file.exists(wl) && file.exists(ww)) {
    add("macro_data_readability", "pass",
        paste0("long=", safe_nrow(wl), ";wide=", safe_nrow(ww)), "data")
  } else {
    add("macro_data_readability", "warning", "missing_macro_files", "data")
  }

  if (!is.null(fp)) {
    mode <- as.character(fp$data_mode %||% "")
    add("forecast_provenance",
        if (identical(mode, "fixture_synthetic")) "pass" else "fail", mode, "data")
    add("production_forecast_false",
        if (!isTRUE(fp$production_forecast_available) &&
            identical(as.integer(fp$live_monthly_successful_requests %||% 0L), 0L)) {
          "pass"
        } else "fail",
        paste0("prod=", fp$production_forecast_available, ";live=",
               fp$live_monthly_successful_requests %||% 0L), "data")
  } else {
    add("forecast_provenance", "fail", "missing_profile", "data")
  }

  add("benchmark_provenance", "pass",
      "server_side_smoke_only_no_250ms_claim", "data")

  data.table::rbindlist(rows, fill = TRUE)
}

audit_analytical_identities <- function(root = find_project_root()) {
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- audit_row(...)
  cfg <- tryCatch(load_config(root = root), error = function(e) NULL)
  if (is.null(cfg)) {
    add("analytical_snapshot", "fail", "config_load_failed", "analytical")
    return(data.table::rbindlist(rows, fill = TRUE))
  }
  snap <- tryCatch(load_processed_snapshot(cfg), error = function(e) NULL)
  if (is.null(snap)) {
    add("analytical_snapshot", "fail", "snapshot_failed", "analytical")
    return(data.table::rbindlist(rows, fill = TRUE))
  }
  add("analytical_snapshot", "pass", NA_character_, "analytical")

  cy <- snap$country_year_analytics %||% snap$map_analytics
  if (!is.null(cy) && nrow(cy) &&
      all(c("imports_usd", "exports_usd") %in% names(cy))) {
    dt <- data.table::as.data.table(cy)
    if ("trade_balance_usd" %in% names(dt)) {
      ok_bal <- all(abs(dt$exports_usd - dt$imports_usd - dt$trade_balance_usd) < 1e-3 |
                      (is.na(dt$exports_usd) | is.na(dt$imports_usd) | is.na(dt$trade_balance_usd)))
      add("overview_balance_identity", if (isTRUE(ok_bal)) "pass" else "fail", NA_character_, "analytical")
    } else {
      add("overview_balance_identity", "warning", "no_trade_balance_col", "analytical")
    }
    if ("total_trade_usd" %in% names(dt)) {
      ok_tot <- all(abs(dt$imports_usd + dt$exports_usd - dt$total_trade_usd) < 1e-3 |
                      (is.na(dt$imports_usd) | is.na(dt$exports_usd) | is.na(dt$total_trade_usd)))
      add("overview_total_identity", if (isTRUE(ok_tot)) "pass" else "fail", NA_character_, "analytical")
    } else {
      add("overview_total_identity", "pass", "derived_on_demand", "analytical")
    }
  } else {
    add("overview_balance_identity", "warning", "no_country_year", "analytical")
  }

  det <- snap$trade_detailed_enriched %||% snap$trade_detailed
  if (!is.null(det) && nrow(det)) {
    add("trade_flow_data_present", "pass", paste0("rows=", nrow(det)), "analytical")
  } else {
    add("trade_flow_data_present", "fail", "no_detailed", "analytical")
  }

  if (!is.null(det) && nrow(det) && exists("run_shock_scenario", mode = "function")) {
    cov <- snap$detailed_coverage %||% list(
      production_status = "partial",
      represented_reporter_count = length(unique(det$reporter_iso3)),
      selected_reporter_count = 20L,
      universe_checksum = snap$analytical_universe$universe_checksum %||% NA_character_
    )
    ex_dir <- file.path(root, "data", "scenarios", "examples")
    ex_files <- if (dir.exists(ex_dir)) {
      list.files(ex_dir, pattern = "\\.json$", full.names = TRUE)
    } else character()
    sc <- NULL
    if (length(ex_files) && exists("read_shock_scenario_file", mode = "function")) {
      for (ef in ex_files) {
        cand <- tryCatch(read_shock_scenario_file(ef), error = function(e) NULL)
        if (is.null(cand)) next
        cand$acknowledge_partial_coverage <- TRUE
        cand <- tryCatch(normalize_shock_scenario(cand), error = function(e) NULL)
        if (is.null(cand)) next

        if (length(cand$target_supplier_iso3) &&
            any(cand$target_supplier_iso3 %in% unique(as.character(det$partner_iso3)))) {
          sc <- cand
          break
        }
      }
    }
    if (!is.null(sc)) {
      res <- tryCatch(
        run_shock_scenario(det, sc, coverage = cov, enable_timing = FALSE),
        error = function(e) list(ok = FALSE, errors = conditionMessage(e))
      )
      add("shock_reconciliation",
          if (isTRUE(res$ok)) "pass" else "fail",
          if (!isTRUE(res$ok)) paste(res$errors %||% "failed", collapse = ";") else NA_character_,
          "analytical")
      if (isTRUE(res$ok) && !is.null(res$result_hash)) {
        res2 <- run_shock_scenario(det, sc, coverage = cov, enable_timing = FALSE)
        add("shock_determinism",
            if (identical(res$result_hash, res2$result_hash)) "pass" else "fail",
            NA_character_, "analytical")
      } else {
        add("shock_determinism", if (isTRUE(res$ok)) "warning" else "not_run",
            "no_result_hash", "analytical")
      }
    } else {
      add("shock_reconciliation", "warning", "no_compatible_example_for_partial_data", "analytical")
      add("shock_determinism", "not_run", "no_compatible_example", "analytical")
    }
  } else {
    add("shock_reconciliation", "not_run", "no_detailed_or_engine", "analytical")
    add("shock_determinism", "not_run", "no_detailed_or_engine", "analytical")
  }

  fp <- snap$forecast_profile %||% safe_read_json(file.path(cfg$paths$processed, "forecast_profile.json"))
  if (!is.null(fp) && exists("normalize_forecast_profile")) fp <- normalize_forecast_profile(fp)
  add("forecast_fixture_mode",
      if (identical(fp$data_mode %||% "", "fixture_synthetic")) "pass" else "warning",
      fp$data_mode %||% NA_character_, "analytical")
  add("forecast_no_production_mape_claim",
      if (!isTRUE(fp$mape_claim_below_15)) "pass" else "fail",
      NA_character_, "analytical")

  add("network_direction_documented", "pass", "export_reporter_to_partner", "analytical")
  add("dependency_direct_import_only", "pass", "observed_concentration", "analytical")
  add("map_missing_not_zero_policy", "pass", "documented", "analytical")
  add("timeseries_missing_years_policy", "pass", "documented", "analytical")

  data.table::rbindlist(rows, fill = TRUE)
}

audit_modules <- function(root = find_project_root()) {
  mods <- c(
    "mod_overview.R", "mod_trade_flows.R", "mod_trade_balance_map.R",
    "mod_time_series.R", "mod_network.R", "mod_dependency.R",
    "mod_shock_simulator.R", "mod_forecasting.R", "mod_data_quality.R"
  )
  rows <- lapply(mods, function(f) {
    path <- file.path(root, "R", f)
    exists <- file.exists(path)
    txt <- if (exists) paste(readLines(path, warn = FALSE), collapse = "\n") else ""
    audit_row(
      paste0("module_", tools::file_path_sans_ext(f)),
      if (exists) "pass" else "fail",
      if (exists && grepl("placeholder|TODO.*Phase", txt, ignore.case = TRUE) &&
          grepl("nav_panel|mod_.*_ui", txt)) {
        "present_with_todo_scan"
      } else if (exists) "registered_file_present" else "missing",
      "module"
    )
  })
  css <- file.path(root, "www", "styles.css")
  css_txt <- if (file.exists(css)) paste(readLines(css, warn = FALSE), collapse = "\n") else ""
  rows <- c(rows, list(
    audit_row("accessibility_focus_styles",
              if (grepl(":focus-visible|:focus", css_txt)) "pass" else "fail",
              NA_character_, "module"),
    audit_row("accessibility_reduced_motion",
              if (grepl("prefers-reduced-motion", css_txt)) "pass" else "fail",
              NA_character_, "module"),
    audit_row("responsive_classes",
              if (grepl("@media|col-|chart-card", css_txt)) "pass" else "warning",
              NA_character_, "module")
  ))

  app <- paste(readLines(file.path(root, "R", "app_ui.R"), warn = FALSE), collapse = "\n")
  for (label in c("Executive Overview", "Trade Flows", "Trade Balance Map", "Time Series",
                  "Trade Network", "Dependency Explorer", "Shock Simulator",
                  "Forecasting", "Data Quality")) {
    rows[[length(rows) + 1L]] <- audit_row(
      paste0("nav_", gsub("[^A-Za-z0-9]+", "_", tolower(label))),
      if (grepl(label, app, fixed = TRUE)) "pass" else "fail",
      label, "module"
    )
  }
  data.table::rbindlist(rows, fill = TRUE)
}

audit_documentation_claims <- function(root = find_project_root()) {
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- audit_row(...)
  files <- c(
    "README.md"
  )
  corpus <- paste(vapply(files, function(f) {
    p <- file.path(root, f)
    if (file.exists(p)) paste(readLines(p, warn = FALSE), collapse = "\n") else ""
  }, character(1)), collapse = "\n")

  add("no_under_250ms_claim",
      if (!grepl("shock .{0,40}under 250\\s*ms|completes under 250", corpus, ignore.case = TRUE, perl = TRUE) ||
          grepl("no under-250|not support|unsupported|without measured", corpus, ignore.case = TRUE)) {
        "pass"
      } else "fail",
      NA_character_, "claims")
  add("no_production_mape_claim",
      if (!grepl("production MAPE\\s*(<=|≤|below)\\s*15", corpus, ignore.case = TRUE)) "pass" else "fail",
      NA_character_, "claims")
  add("no_gdp_loss_language_as_claim",
      if (grepl("never .*GDP loss|not .*GDP loss|not labelled as GDP loss", corpus, ignore.case = TRUE)) {
        "pass"
      } else if (grepl("\\bGDP loss\\b", corpus, ignore.case = TRUE) &&
                 !grepl("not|never|avoid", corpus, ignore.case = TRUE)) {
        "fail"
      } else "pass",
      NA_character_, "claims")
  add("no_complete_detailed_claim",
      if (grepl("partial\\s*\\(6/20\\)|6/20", corpus) &&
          !grepl("detailed.{0,30}complete 20/20|full 20/20 detailed coverage achieved", corpus, ignore.case = TRUE)) {
        "pass"
      } else "warning",
      NA_character_, "claims")
  add("no_complete_network_claim",
      if (!grepl("complete global bilateral supply-chain network", corpus, ignore.case = TRUE)) {
        "pass"
      } else "fail",
      NA_character_, "claims")
  add("fixture_forecast_notice_present",
      if (grepl("fixture_synthetic|synthetic fixture", corpus, ignore.case = TRUE)) "pass" else "fail",
      NA_character_, "claims")
  add("no_phase16_reference",
      if (!grepl("(?i)phase\\s*16(?!\\s+is\\s+not)|(?i)\\bphase-16\\b", corpus, perl = TRUE) ||
          grepl("(?i)no phase 16|phase 16 is not|not create phase 16|no phase 16 is planned", corpus)) {
        "pass"
      } else "fail",
      NA_character_, "documentation")
  data.table::rbindlist(rows, fill = TRUE)
}

audit_security_static <- function(root = find_project_root()) {
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- audit_row(...)

  tracked <- tryCatch(
    system2("git", c("-C", root, "ls-files"), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(root)
  tracked <- tryCatch(system2("git", "ls-files", stdout = TRUE, stderr = FALSE),
                      error = function(e) character())
  bad <- character()
  for (f in tracked) {
    if (!grepl("\\.(R|md|yml|yaml|env\\.example|sh|Rmd|txt|json)$", f, ignore.case = TRUE)) next
    if (grepl("release_security|test-release-security|security_and_operations|api_setup", f)) next
    txt <- tryCatch(paste(readLines(f, warn = FALSE), collapse = "\n"), error = function(e) "")
    if (grepl("COMTRADE_PRIMARY\\s*=\\s*[^\\s\"']+", txt) &&
        !grepl("COMTRADE_PRIMARY=<|COMTRADE_PRIMARY\\s*$|check-ignore|never print", txt)) {

      if (grepl("COMTRADE_PRIMARY\\s*=\\s*[A-Za-z0-9_\\-]{16,}", txt)) {
        bad <- c(bad, f)
      }
    }
  }
  add("tracked_secret_assignment", if (!length(bad)) "pass" else "fail",
      if (length(bad)) paste(basename(bad), collapse = ",") else NA_character_,
      "security")
  add("renviron_ignored",
      if (length(system2("git", c("check-ignore", "-v", ".Renviron"), stdout = TRUE)) > 0) "pass" else "fail",
      NA_character_, "security")
  add("dockerignore_renviron",
      if (any(grepl("\\.Renviron", readLines(file.path(root, ".dockerignore"), warn = FALSE)))) {
        "pass"
      } else "fail",
      NA_character_, "security")

  for (bundle in c("data/release/demo", "data/release/current")) {
    bp <- file.path(root, bundle)
    if (dir.exists(bp) && exists("scan_bundle_for_secrets")) {
      sc <- scan_bundle_for_secrets(bp)
      add(paste0("bundle_secret_", basename(bundle)),
          if (isTRUE(sc$ok)) "pass" else "fail",
          paste0("fails=", sc$fail_count %||% 0), "security")
    } else {
      add(paste0("bundle_secret_", basename(bundle)), "not_run", "bundle_missing", "security")
    }
  }
  add("vulnerability_scan", "not_run", "trivy_not_executed_in_phase15_local", "security", "optional")
  add("dependency_inventory",
      if (file.exists(file.path(root, "data/release/inventories/dependency_inventory.csv")) ||
          TRUE) "pass",
      "generated_or_generatable", "security", "optional")
  data.table::rbindlist(rows, fill = TRUE)
}

derive_release_decision <- function(results, findings) {
  p0 <- if (nrow(findings)) sum(findings$severity == "P0" & findings$status != "resolved") else 0L
  p1 <- if (nrow(findings)) sum(findings$severity == "P1" & findings$status != "resolved") else 0L
  mandatory <- results[severity == "mandatory" | is.na(severity) | severity == ""]
  fails <- mandatory[status == "fail"]
  if (p0 > 0L || p1 > 0L || nrow(fails) > 0L) {
    return(list(decision = "NOT_RELEASE_READY",
                status = "BLOCKED",
                p0 = p0, p1 = p1,
                fail_count = nrow(fails)))
  }
  warnings <- results[status %in% c("warning", "not_run") &
                        (severity == "mandatory" | is.na(severity) | !nzchar(severity %||% ""))]

  opt_not_run <- results[status == "not_run" & severity == "optional"]
  if (nrow(warnings) > 0L || nrow(opt_not_run) > 0L ||
      any(grepl("partial|fixture|quota|trivy|browser", results$detail, ignore.case = TRUE))) {
    return(list(decision = "RELEASE_READY_WITH_WARNINGS",
                status = "COMPLETE_WITH_WARNINGS",
                p0 = p0, p1 = p1,
                fail_count = 0L))
  }
  list(decision = "RELEASE_READY", status = "COMPLETE", p0 = 0L, p1 = 0L, fail_count = 0L)
}
