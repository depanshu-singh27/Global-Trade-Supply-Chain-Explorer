test_that("comparison helpers handle empty and invalid denominators", {
  empty <- list(reporter_impacts = data.table::data.table())
  cmp <- compare_shock_scenarios(empty, empty)
  expect_equal(nrow(cmp$aligned_reporters), 0L)

  a <- list(reporter_impacts = data.table::data.table(
    reporter_iso3 = "DEU", residual_unmet_value_usd = 0,
    direct_disrupted_value_usd = 0, substitution_allocated_value_usd = 0,
    scenario_rank = 1L
  ))
  b <- list(reporter_impacts = data.table::data.table(
    reporter_iso3 = "DEU", residual_unmet_value_usd = 10,
    direct_disrupted_value_usd = 10, substitution_allocated_value_usd = 0,
    scenario_rank = 1L
  ))
  cmp2 <- compare_shock_scenarios(a, b)
  expect_true(is.na(cmp2$aligned_reporters$residual_pct_diff[1]) ||
                is.finite(cmp2$aligned_reporters$residual_pct_diff[1]))
  expect_true(isTRUE(cmp2$aligned_reporters$newly_affected[1]))
})
