test_that("valid WDI reshaping works", {
  long <- data.table::data.table(
    iso3 = c("USA", "USA", "CHN"),
    country_name = c("United States", "United States", "China"),
    year = c(2023L, 2023L, 2023L),
    indicator_code = c("NY.GDP.MKTP.CD", "SP.POP.TOTL", "SP.POP.TOTL"),
    indicator_name = c("GDP", "Pop", "Pop"),
    value = c(1e12, 3e8, 1e9),
    source_updated_at = utc_now(),
    ingested_at = utc_now()
  )
  cfg <- load_config("development", TEST_ROOT)
  out <- clean_wdi_data(long, cfg)
  expect_true("NY.GDP.MKTP.CD" %in% names(out$wide) || nrow(out$wide) >= 1)
  expect_equal(nrow(out$long), 3)
})

test_that("duplicate WDI observation detection works", {
  dt <- data.table::data.table(
    iso3 = c("USA", "USA"),
    year = c(2023L, 2023L),
    indicator_code = c("SP.POP.TOTL", "SP.POP.TOTL"),
    value = c(1, 2)
  )
  res <- validate_unique_keys(dt, c("iso3", "year", "indicator_code"), "wdi", "wdi_unique")
  expect_equal(res$status, "error")
})

test_that("trade/WDI join behaviour is stable", {
  trade <- data.table::data.table(
    reporter_iso3 = c("USA", "CHN"),
    reporter_name = c("United States", "China"),
    partner_iso3 = c("DEU", "USA"),
    trade_value_usd = c(10, 20),
    ingested_at = c("t1", "t1")
  )
  wdi_wide <- data.table::data.table(
    iso3 = c("USA", "CHN"),
    country_name = c("United States", "China"),
    year = c(2023L, 2023L),
    `NY.GDP.MKTP.CD` = c(100, 200)
  )
  summary <- build_pilot_country_summary(trade, wdi_wide)
  expect_true(nrow(summary) >= 1)
  expect_true("reporter_iso3" %in% names(summary))
})

test_that("Parquet write/read round trip works", {
  skip_if_not(requireNamespace("arrow", quietly = TRUE), "arrow not installed")
  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  dt <- data.table::data.table(hs_code = c("85", "0851"), value = c(1, 2))
  arrow::write_parquet(dt, tmp)
  back <- data.table::as.data.table(arrow::read_parquet(tmp))
  expect_equal(back$hs_code, c("85", "0851"))
  expect_true(is.character(back$hs_code))
  res <- validate_parquet_roundtrip(tmp, "fixture")
  expect_true(all(res$status == "pass"))
})

test_that("data-access handles absent processed files", {
  cfg <- load_config("development", TEST_ROOT)
  tmp_root <- tempfile("gte-absent-")
  dir.create(file.path(tmp_root, "data", "processed"), recursive = TRUE)
  cfg2 <- cfg
  cfg2$paths$processed <- file.path(tmp_root, "data", "processed")
  snap <- load_processed_snapshot(cfg2)
  expect_false(isTRUE(snap$available && !is.null(snap$trade) && nrow(snap$trade) > 0))
  metrics <- overview_metrics(snap, cfg2)
  expect_false(metrics$data_available)
  expect_match(metrics$instruction, "run_pilot_pipeline")
})

test_that("Comtrade response parsing from synthetic fixture", {
  body <- paste(readLines(file.path(FIXTURES, "comtrade_synthetic.json"), warn = FALSE),
                collapse = "\n")
  parsed <- parse_comtrade_payload(body)
  expect_equal(parsed$count, 2)
  dt <- comtrade_records_to_dt(parsed$records)
  expect_equal(nrow(dt), 2)
  expect_true("0851" %in% dt$cmd_code || any(dt$cmd_code == "0851"))
  expect_true(is.character(as_char_code(dt$cmd_code)))
})

test_that("World Bank response parsing from synthetic fixture", {
  body <- paste(readLines(file.path(FIXTURES, "wdi_synthetic.json"), warn = FALSE),
                collapse = "\n")
  parsed <- parse_wdi_payload(body)
  expect_equal(parsed$pages, 1)
  dt <- wdi_records_to_dt(parsed$records, "NY.GDP.MKTP.CD", "GDP")

  expect_gte(nrow(dt), 1)
  expect_true(all(c("iso3", "year", "value") %in% names(dt)))
})
