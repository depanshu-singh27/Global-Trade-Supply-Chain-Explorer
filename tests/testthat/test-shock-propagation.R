test_that("first-order same-HS4 propagation and no cross-HS4", {
  det <- make_shock_propagation_fixture()
  cov <- make_shock_coverage()
  res <- run_shock_scenario(
    det,
    make_base_scenario(
      substitution_mode = "none",
      shock_size_pct = 50,
      propagation_mode = "first_order",
      maximum_propagation_steps = 1,
      propagation_pass_through = 1
    ),
    coverage = cov
  )
  expect_true(res$ok)
  expect_true(nrow(res$impact_paths) >= 1)
  expect_true(all(res$impact_paths$hs_code == "8542"))
  expect_false(any(res$impact_paths$hs_code == "8517"))
  expect_true(all(res$impact_paths$depth == 1L))
})

test_that("direct-only has no propagated paths", {
  det <- make_shock_propagation_fixture()
  res <- run_shock_scenario(
    det,
    make_base_scenario(propagation_mode = "direct_only", substitution_mode = "none"),
    coverage = make_shock_coverage()
  )
  expect_equal(nrow(res$impact_paths), 0L)
})

test_that("cycle prevention and depth cap", {

  det <- data.table::data.table(
    year = 2024L,
    reporter_iso3 = c("AAA", "AAA", "BBB", "BBB"),
    reporter_name = c("A", "A", "B", "B"),
    partner_iso3 = c("CHN", "BBB", "AAA", "USA"),
    partner_name = c("China", "B", "A", "USA"),
    flow_code = "M",
    flow_name = "Import",
    hs_code = "8542",
    commodity_description = "ICs",
    trade_value_usd = c(100, 40, 40, 20),
    reporter_gdp_current_usd = 1e12
  )
  res <- run_shock_scenario(
    det,
    make_base_scenario(
      target_supplier_iso3 = "CHN",
      substitution_mode = "none",
      shock_size_pct = 100,
      propagation_mode = "multi_step",
      maximum_propagation_steps = 5,
      propagation_pass_through = 1,
      propagation_decay = 1
    ),
    coverage = make_shock_coverage()
  )
  expect_true(res$ok)

  expect_true(is.finite(sum(res$impact_paths$propagated_value_usd)))
  expect_true(sum(res$impact_paths$propagated_value_usd, na.rm = TRUE) < 1e6)
})

test_that("propagation edge and threshold caps", {
  det <- make_shock_propagation_fixture()
  res <- run_shock_scenario(
    det,
    make_base_scenario(
      substitution_mode = "none",
      shock_size_pct = 100,
      propagation_mode = "first_order",
      minimum_propagated_value_usd = 1e12
    ),
    coverage = make_shock_coverage()
  )
  expect_equal(nrow(res$impact_paths), 0L)
})
