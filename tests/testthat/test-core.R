test_that("configuration loading works", {
  cfg <- load_config("development", TEST_ROOT)
  expect_true(is.list(cfg))
  expect_equal(cfg$pilot$hs_chapter, "85")
  expect_true(dir.exists(cfg[['paths']]$raw) || is.character(cfg[['paths']]$raw))
  expect_false(any(grepl("COMTRADE|KEY|SECRET", names(unlist(cfg)), ignore.case = TRUE) &
                     grepl("primary|secret|key", names(unlist(cfg)), ignore.case = TRUE) &
                     FALSE))
  expect_true("reporters" %in% names(cfg$pilot))
})

test_that("missing Comtrade key detection does not reveal a value", {
  old <- Sys.getenv("COMTRADE_PRIMARY", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("COMTRADE_PRIMARY") else Sys.setenv(COMTRADE_PRIMARY = old)
  }, add = TRUE)
  Sys.unsetenv("COMTRADE_PRIMARY")
  expect_false(comtrade_key_present())
  expect_error(comtrade_subscription_key(), "COMTRADE_PRIMARY")

  err <- tryCatch(comtrade_subscription_key(), error = function(e) conditionMessage(e))
  expect_false(grepl("[0-9a-f]{20,}", err, ignore.case = TRUE))
})

test_that("country-code standardisation succeeds", {
  raw <- data.table::data.table(
    period = "2023", ref_year = 2023, reporter_code = "842", partner_code = "156",
    flow_code = "M", cmd_code = "85", cmd_desc = "Electrical",
    primary_value = 100, net_wgt = 1, qty = 1, qty_unit = "kg",
    flow_desc = "Import", aggr_level = 2, reporter_desc = "USA", partner_desc = "China"
  )
  cfg <- load_config("development", TEST_ROOT)
  out <- clean_trade_data(raw, cfg)
  expect_equal(out$trade$reporter_iso3[1], "USA")
  expect_equal(out$trade$partner_iso3[1], "CHN")
  expect_equal(nrow(out$unmatched_reporters), 0)
})

test_that("unmatched country-code detection works", {
  raw <- data.table::data.table(
    period = "2023", ref_year = 2023, reporter_code = "999", partner_code = "998",
    flow_code = "M", cmd_code = "85", cmd_desc = "Electrical",
    primary_value = 100, net_wgt = NA, qty = NA, qty_unit = NA,
    flow_desc = "Import", aggr_level = 2, reporter_desc = "X", partner_desc = "Y"
  )
  cfg <- load_config("development", TEST_ROOT)
  out <- clean_trade_data(raw, cfg)
  expect_gt(nrow(out$unmatched_reporters), 0)
  expect_gt(nrow(out$unmatched_partners), 0)
  row <- validate_mapping_coverage(
    out$trade$reporter_code, out$trade$reporter_iso3, "t", "reporter"
  )
  expect_equal(row$status, "warning")
})

test_that("HS codes remain character including leading zeroes", {
  raw <- data.table::data.table(
    period = "2023", ref_year = 2023, reporter_code = "842", partner_code = "156",
    flow_code = "X", cmd_code = "0851", cmd_desc = "Fixture",
    primary_value = 10, net_wgt = NA, qty = NA, qty_unit = NA,
    flow_desc = "Export", aggr_level = 4, reporter_desc = "USA", partner_desc = "China"
  )
  cfg <- load_config("development", TEST_ROOT)
  out <- clean_trade_data(raw, cfg)
  expect_true(is.character(out$trade$hs_code))
  expect_equal(out$trade$hs_code[1], "0851")
  expect_equal(validate_hs_character(out$trade$hs_code, "t")$status, "pass")
})

test_that("duplicate trade-row detection works", {
  dt <- data.table::data.table(
    year = c(2023, 2023), frequency = c("A", "A"),
    reporter_code = c("842", "842"), partner_code = c("156", "156"),
    flow_code = c("M", "M"), hs_code = c("85", "85"),
    trade_value_usd = c(1, 2)
  )
  res <- validate_unique_keys(
    dt, c("year", "frequency", "reporter_code", "partner_code", "flow_code", "hs_code"),
    "t"
  )
  expect_equal(res$status, "error")
  expect_gt(res$affected_rows, 0)
})

test_that("missing required columns fail validation", {
  dt <- data.table::data.table(year = 2023)
  res <- validate_required_columns(dt, c("year", "hs_code", "trade_value_usd"), "t")
  expect_equal(res$status, "error")
})

test_that("non-negative trade-value validation works", {
  expect_equal(validate_non_negative(c(0, 1, 2), "t")$status, "pass")
  expect_equal(validate_non_negative(c(1, -5), "t")$status, "error")
})
