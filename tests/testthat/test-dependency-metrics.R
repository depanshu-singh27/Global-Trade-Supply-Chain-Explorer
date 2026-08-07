test_that("top-one, top-three, HHI, supplier counts", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019,
                                      reporters = "DEU", hs_codes = "8542")
  gc <- supplier_concentration_by_group(built$shares)
  expect_equal(nrow(gc), 1L)
  expect_equal(gc$top_1_share, 0.5, tolerance = 1e-8)
  expect_equal(gc$top_3_share, 1, tolerance = 1e-8)

  expect_equal(gc$supplier_hhi, 0.375, tolerance = 1e-8)
  expect_equal(gc$supplier_count, 3L)
  expect_equal(gc$effective_supplier_count, 1 / 0.375, tolerance = 1e-8)
  expect_true(gc$supplier_hhi >= 0 && gc$supplier_hhi <= 1)
  expect_true(gc$top_3_share >= gc$top_1_share && gc$top_3_share <= 1 + 1e-8)
})

test_that("one-supplier effective count and fewer-than-three top3", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019,
                                      reporters = "IND", hs_codes = "8542")
  gc <- supplier_concentration_by_group(built$shares)
  expect_equal(gc$supplier_count, 1L)
  expect_equal(gc$top_1_share, 1, tolerance = 1e-8)
  expect_equal(gc$top_3_share, 1, tolerance = 1e-8)
  expect_equal(gc$supplier_hhi, 1, tolerance = 1e-8)
  expect_equal(gc$effective_supplier_count, 1, tolerance = 1e-8)
})

test_that("equal-share HHI for two suppliers", {

  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2020, year_max = 2020,
                                      reporters = "DEU", hs_codes = "8542")
  gc <- supplier_concentration_by_group(built$shares)
  expect_equal(gc$supplier_hhi, 0.52, tolerance = 1e-8)
})

test_that("commodity importance and weighted reporter concentration", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019)
  gc <- add_commodity_importance(supplier_concentration_by_group(built$shares))
  expect_true(all(is.finite(gc$commodity_import_share) | is.na(gc$commodity_import_share)))

  ind <- gc[reporter_iso3 == "IND"]
  expect_equal(sum(ind$commodity_import_share), 1, tolerance = 1e-8)
  rw <- reporter_weighted_concentration(gc)
  expect_true(all(is.finite(rw$weighted_hhi) | is.na(rw$weighted_hhi)))
  expect_false(any(is.infinite(rw$weighted_hhi), na.rm = TRUE))
  expect_false(any(is.nan(rw$weighted_hhi), na.rm = TRUE))

  expect_equal(rw[reporter_iso3 == "IND"]$weighted_hhi, 1, tolerance = 1e-8)
})

test_that("concentrated import exposure and supplier exposure", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019)
  gc <- supplier_concentration_by_group(built$shares)
  expect_true("concentrated_import_value" %in% names(gc))
  expect_equal(
    gc[reporter_iso3 == "DEU" & hs_code == "8542"]$concentrated_import_value,
    100 * 0.5,
    tolerance = 1e-8
  )
  exp <- supplier_exposure_summary(built$shares)
  expect_true(nrow(exp) > 0)
  expect_true(all(exp$top_supplier_event_count >= 0))
  expect_true(all(exp$share_gt_50_count >= 0))
  chn <- exp[partner_iso3 == "CHN"]
  expect_true(chn$total_import_value_supplied > 0)
})

test_that("concentration bands and finite metrics", {
  expect_equal(classify_hhi_band(0.1), "Diversified")
  expect_equal(classify_hhi_band(0.4), "High concentration")
  expect_equal(classify_share_band(0.8), "Above 75%")
  expect_equal(classify_hhi_band(NA_real_), "Unavailable")
})

test_that("dependency trends preserve missing years without interpolation", {
  det <- make_dependency_fixture()

  tr <- dependency_trend_by_year(det, reporters = "DEU", hs_codes = "8542",
                                 year_min = 2019, year_max = 2021)
  expect_equal(tr$year, 2019:2021)
  expect_true(is.finite(tr$weighted_hhi[1]))
  expect_true(is.na(tr$weighted_hhi[3]))
})

test_that("selected profiles and ranking ties", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019)
  gc <- add_commodity_importance(supplier_concentration_by_group(built$shares))
  prof <- selected_reporter_profile(built$shares, gc, "DEU")
  expect_equal(prof$reporter_iso3, "DEU")
  cprof <- selected_commodity_profile(built$shares, gc, "8542")
  expect_equal(cprof$hs_code, "8542")
  ranked <- rank_dependency_groups(gc, "top_1_share", 5L)
  expect_true(nrow(ranked) <= 5L)
  nodes <- data.table::data.table(
    reporter_iso3 = c("BBB", "AAA"), hs_code = c("1", "1"),
    top_1_share = c(0.9, 0.9), supplier_hhi = c(0.8, 0.8),
    reporter_commodity_total = 1, commodity_description = "x",
    reporter_name = c("B", "A")
  )
  r <- rank_dependency_groups(nodes, "top_1_share", 2L)
  expect_equal(r$reporter_iso3[1], "AAA")
})

test_that("accessibility summary generation", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019)
  gc <- add_commodity_importance(supplier_concentration_by_group(built$shares))
  rw <- reporter_weighted_concentration(gc)
  s <- dependency_accessibility_summary(built, gc, rw, list(
    represented_reporter_count = 3L, selected_reporter_count = 8L
  ))
  expect_true(grepl("direct reported-import", s, ignore.case = TRUE))
})
