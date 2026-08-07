test_that("scenario schema, validation and deterministic hash", {
  sc <- make_base_scenario()
  n1 <- normalize_shock_scenario(sc)
  n2 <- normalize_shock_scenario(sc)
  expect_equal(shock_scenario_hash(n1), shock_scenario_hash(n2))
  sc2 <- make_base_scenario(shock_size_pct = 40)
  expect_false(identical(shock_scenario_hash(sc), shock_scenario_hash(sc2)))

  bad <- make_base_scenario(shock_size_pct = 150)
  v <- validate_shock_scenario(bad, coverage = make_shock_coverage())
  expect_false(v$ok)

  bad2 <- make_base_scenario(acknowledge_partial_coverage = FALSE)
  v2 <- validate_shock_scenario(bad2, coverage = make_shock_coverage("partial"))
  expect_false(v2$ok)

  stale <- make_base_scenario(universe_version = "uv_other")
  v3 <- validate_shock_scenario(stale, coverage = make_shock_coverage())
  expect_false(v3$ok)
})

test_that("baseline uses imports only and excludes aggregates/self/world", {
  det <- make_shock_detailed_fixture()

  det <- rbind(det, data.table::copy(det[1])[, flow_code := "X"][, trade_value_usd := 123])
  bl <- build_shock_baseline(det, 2024, 2024, coverage = make_shock_coverage())
  expect_true(nrow(bl$baseline) > 0)
  expect_false(any(bl$baseline$supplier_iso3 %in% c("WLD", "EUR")))
  expect_false(any(bl$baseline$reporter_iso3 == bl$baseline$supplier_iso3))
  expect_equal(nrow(det), 9L)
})

test_that("absent supplier and HS4 rejected", {
  det <- make_shock_detailed_fixture()
  bl <- build_shock_baseline(det, 2024, 2024, coverage = make_shock_coverage())
  v <- validate_shock_scenario(
    make_base_scenario(target_supplier_iso3 = "ZZZ"),
    baseline = bl$baseline,
    coverage = make_shock_coverage()
  )
  expect_false(v$ok)
  v2 <- validate_shock_scenario(
    make_base_scenario(target_hs_codes = "9999"),
    baseline = bl$baseline,
    coverage = make_shock_coverage()
  )
  expect_false(v2$ok)
})
