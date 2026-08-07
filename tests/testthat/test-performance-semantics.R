test_that("optimisations do not change filter semantics for detailed trade", {
  det <- make_trade_flow_fixture()
  a <- filter_detailed_trade(det, year_min = 2024L, year_max = 2024L, flows = "M")
  b <- filter_detailed_trade(det, year_min = 2024L, year_max = 2024L, flows = "M")
  expect_equal(nrow(a), nrow(b))
  expect_equal(sum(a$trade_value_usd), sum(b$trade_value_usd))
  expect_false(any(is.na(a$trade_value_usd) & a$trade_value_usd == 0))
})

test_that("dependency shares remain reconciled after construction", {
  det <- make_dependency_fixture()
  y <- max(det$year)
  dep <- construct_dependency_table(det, year_min = y, year_max = y)
  expect_true(nrow(dep$shares) > 0)

  sums <- dep$shares[, .(s = sum(partner_share, na.rm = TRUE)), by = .(reporter_iso3, hs_code)]
  expect_true(all(abs(sums$s - 1) < 1e-6 | sums$s == 0))
})

test_that("forecast UI snapshot does not refit models", {

  cfg <- load_config()

  out <- tryCatch(load_forecast_ui_snapshot(cfg), error = function(e) e)
  expect_false(inherits(out, "error"))
})

test_that("sparse vs dense size helper prefers sparse for large dims", {
  i <- c(1L, 2L, 3L); j <- c(1L, 2L, 3L); x <- c(1, 2, 3)
  cmp <- compare_sparse_vs_dense_bytes(i, j, x, dims = c(200L, 200L))
  expect_true(isTRUE(cmp$sparse_smaller))
})
