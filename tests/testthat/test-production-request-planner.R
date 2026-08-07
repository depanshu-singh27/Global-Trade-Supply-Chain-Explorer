test_that("request IDs are deterministic for identical plan inputs", {
  cfg <- load_config("development", TEST_ROOT)
  p1 <- build_global_hs85_plan(cfg = cfg, reporter_codes = c("842", "156"), years = 2019:2020)
  p2 <- build_global_hs85_plan(cfg = cfg, reporter_codes = c("842", "156"), years = 2019:2020)
  expect_equal(p1$request_id, p2$request_id)
  expect_true(all(nzchar(p1$request_id)))
  expect_equal(nrow(p1), 2L)
  expect_false(any(p1$reporter_code == "0"))
})

test_that("request plan does not include secret-bearing metadata fields", {
  cfg <- load_config("development", TEST_ROOT)
  p <- build_global_hs85_plan(cfg = cfg, reporter_codes = "842", years = 2024)
  secretish <- c("COMTRADE_PRIMARY", "KEY", "SECRET", "subscription")
  expect_false(any(grepl(paste(secretish, collapse = "|"), names(p), ignore.case = TRUE)))
})

test_that("global plan rejects empty reporters", {
  cfg <- load_config("development", TEST_ROOT)
  expect_error(build_global_hs85_plan(cfg = cfg, reporter_codes = "0", years = 2024))
})
