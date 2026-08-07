test_that("demo bundle builds, labels synthetic, deterministic checksum stable", {
  root <- release_test_root()
  dest <- file.path(tempdir(), paste0("gtsc-demo-", as.integer(Sys.time())))
  res <- build_demo_bundle(dest_dir = dest, root = root)
  expect_true(file.exists(res$manifest_path))
  expect_identical(res$manifest$runtime_profile, "demo")
  expect_identical(res$manifest$forecast_data_mode, "fixture_synthetic")
  expect_false(isTRUE(res$manifest$production_forecast_available))
  expect_identical(as.integer(res$manifest$live_monthly_successful_requests %||% 0L), 0L)
  fp <- safe_read_json(file.path(dest, "forecast_profile.json"))
  expect_true(grepl("synthetic|fixture", fp$data_mode %||% "", ignore.case = TRUE) ||
                grepl("synthetic", fp$data_source %||% "", ignore.case = TRUE))

  f <- "forecast_profile.json"
  h1 <- file_sha256(file.path(dest, f))
  res2 <- build_demo_bundle(dest_dir = dest, root = root)
  h2 <- file_sha256(file.path(dest, f))
  expect_identical(h1, h2)

  vr <- validate_release_bundle(dest, expected_profile = "demo")
  expect_false(any(vr$status == "fail"))
})

test_that("release allowlist rejects unexpected files and missing required", {
  root <- release_test_root()
  src <- file.path(root, "data", "processed")
  skip_if_not(file.exists(file.path(src, "trade_global_hs85_annual.parquet")))
  dest <- file.path(tempdir(), paste0("gtsc-rel-", as.integer(Sys.time())))
  dir.create(dest, recursive = TRUE)
  res <- build_release_bundle(src, dest, profile = "release", root = root)
  expect_true("trade_global_hs85_annual.parquet" %in% basename(res$files))
  expect_false(any(grepl("\\.Renviron|raw/|interim/", res$files)))

  writeLines("x", file.path(dest, "evil.txt"))
  vr <- validate_release_bundle(dest, expected_profile = "release")
  expect_true(any(vr$check == "bundle_allowlist" & vr$status == "fail"))
  unlink(file.path(dest, "evil.txt"))

  unlink(file.path(dest, "forecast_profile.json"))
  vr2 <- validate_release_bundle(dest, expected_profile = "release")
  expect_true(any(vr2$status == "fail"))
})

test_that("release bundle preserves status and checksums when processed data exist", {
  root <- release_test_root()
  src <- file.path(root, "data", "processed")
  skip_if_not(file.exists(file.path(src, "analytical_universe.json")))
  dest <- file.path(tempdir(), paste0("gtsc-rel2-", as.integer(Sys.time())))
  res <- build_release_bundle(src, dest, profile = "release", root = root)
  expect_true(
    identical(res$manifest$detailed_production_status, "complete") ||
      grepl("partial|complete", res$manifest$detailed_production_status %||% "", ignore.case = TRUE)
  )
  expect_true(nzchar(res$manifest$universe_checksum %||% ""))
  expect_true(length(res$manifest$files) > 0)
  entry <- res$manifest$files[[1]]
  expect_true(nzchar(entry$sha256 %||% ""))
  expect_true(!is.null(entry$schema_fingerprint))
  vr <- validate_release_bundle(dest, expected_profile = "release")
  expect_validation_has(vr, "bundle_checksums", "pass")
  expect_validation_has(vr, "forecast_fixture_provenance", "pass")
  expect_validation_has(vr, "production_forecast_false_when_live_zero", "pass")
  expect_validation_has(vr, "secret_scan", "pass")
})
