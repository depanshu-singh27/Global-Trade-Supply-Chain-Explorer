test_that("UI validation blocks invalid and enables valid runs", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  bl <- build_shock_baseline(det, 2024, 2024, coverage = cov)$baseline

  bad <- shock_ui_validate_inputs(
    make_shock_ui_inputs(acknowledge_partial_coverage = FALSE),
    baseline = bl,
    coverage = cov
  )
  expect_false(bad$ok)
  expect_equal(bad$status, "Error")

  good <- shock_ui_validate_inputs(make_shock_ui_inputs(), baseline = bl, coverage = cov)
  expect_true(good$ok)
  grp <- shock_ui_validation_groups(good)
  expect_true(grp$can_run)
  expect_true(grepl("Valid", grp$accessible_summary))

  complete_cov <- make_shock_coverage("complete")
  ok2 <- shock_ui_validate_inputs(
    make_shock_ui_inputs(acknowledge_partial_coverage = FALSE),
    baseline = bl,
    coverage = complete_cov
  )

  expect_false(any(grepl("Acknowledge partial", ok2$errors)))
})

test_that("no-target and absent supplier validation", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  bl <- build_shock_baseline(det, 2024, 2024, coverage = cov)$baseline
  v <- shock_ui_validate_inputs(
    make_shock_ui_inputs(target_supplier_iso3 = "ZZZ", target_hs_codes = "8542"),
    baseline = bl,
    coverage = cov
  )
  expect_false(v$ok)
})
