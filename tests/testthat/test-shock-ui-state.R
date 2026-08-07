test_that("default builder state and control choices", {
  cov <- make_shock_coverage()
  defs <- shock_builder_defaults(cov, years = c(2019L, 2024L))
  expect_equal(defs$shock_size_pct, 30)
  expect_equal(defs$substitution_mode, "capacity_constrained")
  expect_equal(defs$propagation_mode, "direct_only")
  expect_false(isTRUE(defs$acknowledge_partial_coverage))

  det <- make_shock_detailed_fixture()
  bl <- build_shock_baseline(det, 2024, 2024, coverage = cov)$baseline
  ch <- shock_control_choices(bl, cov)
  expect_true("CHN" %in% unname(ch$suppliers))
  expect_false("WLD" %in% unname(ch$suppliers))
  expect_true(all(ch$reporters %in% c("DEU", "IND", "KOR")))
  expect_true("8542" %in% unname(ch$hs_codes))
})

test_that("control visibility and schema mapping", {
  vis <- shock_control_visibility("none", "direct_only", "supplier_export_reduction")
  expect_false(vis$show_capacity)
  expect_false(vis$show_propagation_advanced)
  vis2 <- shock_control_visibility("capacity_constrained", "multi_step", "reporter_specific_bilateral_reduction")
  expect_true(vis2$show_capacity)
  expect_true(vis2$show_multi_step)
  expect_true(vis2$require_reporters)

  sc <- shock_ui_to_scenario(make_shock_ui_inputs(enable_max_share = TRUE, maximum_substitute_supplier_share = 40))
  expect_equal(sc$maximum_substitute_supplier_share, 0.4)
  expect_equal(sc$engine_version, SHOCK_ENGINE_VERSION)
})

test_that("partial acknowledgement rules", {
  expect_true(shock_partial_ack_required(make_shock_coverage("partial")))
  expect_false(shock_partial_ack_required(make_shock_coverage("complete")))
})
