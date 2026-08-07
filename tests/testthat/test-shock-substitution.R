test_that("no-substitution residual equals disruption", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  res <- run_shock_scenario(
    det,
    make_base_scenario(substitution_mode = "none", shock_size_pct = 30),
    coverage = cov
  )
  expect_true(res$ok)
  d <- sum(res$edge_impacts$direct_disrupted_value_usd, na.rm = TRUE)
  s <- sum(res$edge_impacts$substitution_allocated_usd, na.rm = TRUE)
  u <- sum(res$edge_impacts$residual_unmet_value_usd, na.rm = TRUE)
  expect_equal(s, 0)
  expect_equal(u, d, tolerance = 1e-6)
})

test_that("proportional and capacity-constrained substitution", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  prop <- run_shock_scenario(
    det,
    make_base_scenario(substitution_mode = "proportional", shock_size_pct = 30),
    coverage = cov
  )
  expect_true(prop$ok)
  d <- sum(prop$edge_impacts$direct_disrupted_value_usd)
  s <- sum(prop$edge_impacts$substitution_allocated_usd)
  u <- sum(prop$edge_impacts$residual_unmet_value_usd)
  expect_equal(d, s + u, tolerance = 1e-6)
  expect_true(s > 0)

  cap <- run_shock_scenario(
    det,
    make_base_scenario(
      substitution_mode = "capacity_constrained",
      substitution_capacity_pct = 10,
      shock_size_pct = 50
    ),
    coverage = cov
  )
  expect_true(cap$ok)

  expect_true(sum(cap$edge_impacts$residual_unmet_value_usd) > 0)

  expect_true(
    sum(cap$edge_impacts$substitution_allocated_usd) <=
      sum(cap$edge_impacts$direct_disrupted_value_usd) + 1e-6
  )
})

test_that("full substitution can zero residual when capacity ample", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  res <- run_shock_scenario(
    det,
    make_base_scenario(
      substitution_mode = "capacity_constrained",
      substitution_capacity_pct = 100,
      shock_size_pct = 10
    ),
    coverage = cov
  )
  expect_true(res$ok)

  deu <- res$edge_impacts[reporter_iso3 == "DEU"]
  expect_true(sum(deu$residual_unmet_value_usd) < sum(deu$direct_disrupted_value_usd))
})

test_that("post-shock identities and rates", {
  det <- make_shock_detailed_fixture()
  res <- run_shock_scenario(det, make_base_scenario(), coverage = make_shock_coverage())
  ed <- res$edge_impacts
  expect_true(isTRUE(res$reconciliation$ok))
  expect_false(any(ed$residual_unmet_value_usd < -1e-9, na.rm = TRUE))
  expect_false(any(is.infinite(ed$residual_unmet_value_usd), na.rm = TRUE))
  expect_false(any(is.nan(ed$residual_unmet_value_usd), na.rm = TRUE))
  rates <- ed$substitution_rate[ed$direct_disrupted_value_usd > 0]
  expect_true(all(is.na(rates) | (rates >= -1e-9 & rates <= 1 + 1e-9)))
})
