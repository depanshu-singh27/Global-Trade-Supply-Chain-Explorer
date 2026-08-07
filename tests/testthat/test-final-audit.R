test_that("Phase 15 final audit artefacts and README claims are valid", {
  root <- release_test_root()

  expect_true(file.exists(file.path(root, "R/final_audit.R")))
  expect_true(file.exists(file.path(root, "scripts/run_phase15_final_audit.R")))

  readme <- paste(
    readLines(file.path(root, "README.md"), warn = FALSE),
    collapse = "\n"
  )

  expect_true(grepl("Phase \\d+", readme, perl = TRUE))
  expect_false(
    grepl("(?i)create Phase 16|Phase 16 is complete", readme, perl = TRUE)
  )
  expect_true(grepl("partial|6/20", readme))
  expect_true(grepl("fixture", readme, ignore.case = TRUE))
})

test_that("portfolio claims remain protected by final audit validation", {
  root <- release_test_root()

  claims <- audit_documentation_claims(root)

  expected_checks <- c(
    "no_under_250ms_claim",
    "no_production_mape_claim",
    "no_gdp_loss_language_as_claim",
    "no_complete_detailed_claim",
    "no_complete_network_claim",
    "fixture_forecast_notice_present",
    "no_phase16_reference"
  )

  expect_true(all(expected_checks %in% claims$check_id))
  expect_false(any(claims$status == "fail"))
})
