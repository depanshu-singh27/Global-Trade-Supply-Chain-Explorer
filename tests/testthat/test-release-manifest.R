test_that("release candidate manifest has no absolute paths or secrets", {
  root <- release_test_root()
  man <- build_release_candidate_manifest(
    bundle_manifest = list(
      runtime_profile = "demo",
      global_production_status = "complete",
      detailed_production_status = "partial",
      forecast_data_mode = "fixture_synthetic"
    ),
    image_meta = list(base_image = "rocker/r-ver:4.6.1", image_tag = "gtsc:phase14-rc"),
    root = root
  )
  txt <- jsonlite::toJSON(man, auto_unbox = TRUE)
  expect_false(grepl("COMTRADE_PRIMARY", txt))
  expect_false(grepl("[A-Za-z]:\\\\Users\\\\", txt))
  expect_true(nzchar(man$application_version))
  expect_true(nzchar(man$release_candidate_id))
  expect_true(!is.null(man$dirty_working_tree))
  expect_true(nzchar(man$renv_lock_checksum %||% "") || is.na(man$renv_lock_checksum))
  inv <- build_dependency_inventory(root)
  expect_true(nrow(inv) > 0)
  expect_true(all(c("package", "version", "source") %in% names(inv)))
  sys <- build_system_dependency_inventory("rocker/r-ver:4.6.1")
  expect_true(nrow(sys) > 0)
})
