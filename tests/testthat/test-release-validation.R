test_that("validation statuses preserve not_run/warning/fail semantics", {
  dt <- data.table::rbindlist(list(
    validation_row("a", "pass"),
    validation_row("b", "warning"),
    validation_row("c", "not_run", severity = "optional")
  ))
  expect_identical(gate_status_from_rows(dt), "warning")
  dt2 <- data.table::rbindlist(list(
    validation_row("a", "pass"),
    validation_row("b", "fail", severity = "mandatory")
  ))
  expect_identical(gate_status_from_rows(dt2), "fail")
  expect_identical(gate_status_from_rows(validation_row("x", "not_run")), "warning")
})

test_that("public mode disables persistence; enabled persistence uses scenario flag", {
  with_temp_env(c(
    GTSC_ALLOW_SCENARIO_WRITES = "false",
    GTSC_READ_ONLY_MODE = "true",
    GTSC_PUBLIC_MODE = "true",
    GTSC_RUNTIME_PROFILE = "demo"
  ), {
    cfg <- normalise_runtime_config(list(
      runtime_profile = "demo", allow_scenario_writes = FALSE, read_only_mode = TRUE,
      host = "0.0.0.0", port = 3838L, data_root = "data/processed",
      scenario_root = "data/scenarios", performance_root = "data/performance"
    ))
    expect_false(runtime_allows_scenario_persistence(cfg))
    cfg2 <- cfg
    cfg2$allow_scenario_writes <- TRUE
    cfg2$read_only_mode <- FALSE
    expect_true(runtime_allows_scenario_persistence(cfg2))
    cfg3 <- cfg2
    cfg3$read_only_mode <- TRUE
    expect_false(runtime_allows_scenario_persistence(cfg3))
  })
})

test_that("no silent demo fallback for release/external when demo manifest supplied", {
  root <- release_test_root()
  dest <- file.path(tempdir(), paste0("fb-", as.integer(Sys.time())))
  build_demo_bundle(dest_dir = dest, root = root)
  cfg <- normalise_runtime_config(list(
    runtime_profile = "release", host = "0.0.0.0", port = 3838L,
    data_root = dest, scenario_root = "data/scenarios",
    performance_root = "data/performance"
  ))

  cfg$runtime_profile <- "release"
  expect_error(validate_runtime_profile_or_stop(cfg, dest), "demo fallback|validation failed|Bundle validation")
})

test_that("app detects demo/partial/fixture provenance from snapshot helpers", {
  root <- release_test_root()
  dest <- file.path(tempdir(), paste0("snap-", as.integer(Sys.time())))
  build_demo_bundle(dest_dir = dest, root = root)
  man <- safe_read_json(file.path(dest, "release_bundle_manifest.json"))
  expect_identical(man$runtime_profile, "demo")
  expect_true(
    grepl("partial|complete", man$detailed_production_status %||% "partial", ignore.case = TRUE)
  )
  expect_identical(man$forecast_data_mode, "fixture_synthetic")
  tp <- safe_read_json(file.path(dest, "trade_data_profile.json"))
  expect_true(isTRUE(tp$demo_mode) || grepl("Demo|Synthetic", tp$runtime_bundle_label %||% ""))
})

test_that("generated release outputs are gitignored", {
  root <- release_test_root()
  ignore_rules <- readLines(
    file.path(root, ".gitignore"),
    warn = FALSE
  )
  ignore_rules <- trimws(ignore_rules)

  expect_true("data/release/current/*" %in% ignore_rules)
  expect_false(
    "!data/release/current/release_bundle_manifest.json" %in% ignore_rules
  )
})

test_that("performance and forecast claim guards are preserved in runtime code", {
  root <- release_test_root()

  skip_if_not(file.exists(file.path(root, "R/performance_validation.R")))
  txt <- paste(readLines(file.path(root, "R/performance_validation.R")), collapse = "\n")
  expect_true(grepl("250|claim250|under.250", txt, ignore.case = TRUE))
  ft <- paste(readLines(file.path(root, "R/forecast_formatters.R")), collapse = "\n")
  expect_true(grepl("fixture_synthetic|production_forecast_available", ft))
})
