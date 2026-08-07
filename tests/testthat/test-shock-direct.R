test_that("direct shock 0%, partial, 100%", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  bl <- build_shock_baseline(det, 2024, 2024, coverage = cov)$baseline

  d0 <- apply_direct_shock(bl, make_base_scenario(shock_size_pct = 0))
  expect_equal(d0$direct_disrupted_value, 0)

  d30 <- apply_direct_shock(bl, make_base_scenario(shock_size_pct = 30))
  tgt <- d30$targets
  expect_true(nrow(tgt) > 0)
  expect_equal(
    sum(tgt$direct_disrupted_value_usd),
    sum(tgt$baseline_import_value_usd) * 0.3,
    tolerance = 1e-8
  )
  expect_equal(
    tgt$direct_disrupted_value_usd,
    tgt$baseline_import_value_usd - tgt$post_shock_supplier_value_usd,
    tolerance = 1e-8
  )

  unaff <- d30$edges[is_targeted == FALSE]
  expect_true(all(unaff$direct_disrupted_value_usd == 0))
  expect_equal(unaff$post_shock_supplier_value_usd, unaff$baseline_import_value_usd)

  d100 <- apply_direct_shock(bl, make_base_scenario(shock_size_pct = 100))
  expect_true(all(d100$targets$post_shock_supplier_value_usd == 0))
})

test_that("duplicate targets not double-counted", {
  det <- make_shock_detailed_fixture()
  bl <- build_shock_baseline(det, 2024, 2024, coverage = make_shock_coverage())$baseline
  sc <- make_base_scenario(target_supplier_iso3 = c("CHN", "CHN"))
  d <- apply_direct_shock(bl, sc)
  n_tgt <- sum(d$edges$is_targeted)
  expect_equal(n_tgt, nrow(unique(d$targets[, .(reporter_iso3, supplier_iso3, hs_code)])))
})
