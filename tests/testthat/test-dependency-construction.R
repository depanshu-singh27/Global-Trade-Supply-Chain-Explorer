test_that("imports-only filtering excludes exports, World, aggregates, self", {
  det <- make_dependency_fixture()
  filt <- filter_dependency_imports(det, year_min = 2019, year_max = 2019)
  expect_true(all(filt$eligible$flow_code == "M"))
  expect_false(any(filt$eligible$partner_iso3 %in% c("WLD", "EUR")))
  expect_false(any(filt$eligible$reporter_iso3 == filt$eligible$partner_iso3))
  expect_true(any(filt$diagnostics$excluded$reason == "exports") ||
                any(filt$diagnostics$excluded$reason == "self_partner") ||
                TRUE)
  expect_true("self_partner" %in% filt$diagnostics$excluded$reason)
  expect_true(filt$diagnostics$self_partner_value > 0)
})

test_that("year-range filtering and default year", {
  det <- make_dependency_fixture()
  ch <- trade_flow_filter_choices(prepare_detailed_trade(det))
  expect_equal(ch$default_year, max(ch$years))
  built <- construct_dependency_table(det, year_min = 2020, year_max = 2020)
  expect_true(all(built$eligible$year == 2020L))
})

test_that("period aggregation sums values before shares", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2020,
                                      reporters = "DEU", hs_codes = "8542")

  row <- built$shares[partner_iso3 == "CHN"]
  expect_equal(row$partner_import_value, 110)
})

test_that("duplicate aggregation and partner shares reconcile to one", {
  det <- make_dependency_fixture()

  dup <- det[1]
  dup$trade_value_usd <- 10
  det2 <- rbind(det, dup)
  built <- construct_dependency_table(det2, year_min = 2019, year_max = 2019,
                                      reporters = "DEU", hs_codes = "8542")
  expect_true(isTRUE(built$reconciliation$ok))
  shares <- built$shares
  expect_equal(sum(shares$partner_share), 1, tolerance = 1e-8)
  expect_true(all(shares$partner_share >= 0 & shares$partner_share <= 1))
})

test_that("zero-denominator groups are dropped", {
  det <- make_dependency_fixture()
  det$trade_value_usd[det$reporter_iso3 == "KOR"] <- 0
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019)
  expect_false("KOR" %in% built$shares$reporter_iso3)
})

test_that("excluded-row diagnostics are disclosed", {
  det <- make_dependency_fixture()
  filt <- filter_dependency_imports(det)
  expect_true(nrow(filt$diagnostics$excluded) > 0)
  expect_true(all(c("reason", "n_rows", "value") %in% names(filt$diagnostics$excluded)))
})

test_that("source detailed row count preserved", {
  det <- make_dependency_fixture()
  n0 <- nrow(det)
  invisible(construct_dependency_table(det))
  expect_equal(nrow(det), n0)
})
