test_that("macro country universe excludes aggregates and includes top reporters", {
  trade_global <- data.table::data.table(
    reporter_iso3 = c("USA", "CHN", "EUR", "DEU"),
    reporter_name = c("USA", "China", "EU", "Germany"),
    year = 2023L, partner_iso3 = "W00", partner_code = "0",
    flow_code = "M", trade_value_usd = 1, ingested_at = "t"
  )
  top_reporters <- data.table::data.table(
    reporter_iso3 = c("USA", "CHN"), reporter_name = c("USA", "China"),
    reporter_code = c("842", "156")
  )
  top_partners <- data.table::data.table(
    partner_iso3 = c("DEU", "IND"), partner_name = c("Germany", "India"),
    partner_code = c("276", "699")
  )
  eligible <- data.table::data.table(
    iso3 = c("USA", "CHN", "DEU", "IND"),
    reporter_name = c("USA", "China", "Germany", "India"),
    reporter_entity_type = "country_or_economy"
  )
  uni <- build_macro_country_universe(
    cfg = load_config("development", TEST_ROOT),
    trade_global = trade_global,
    trade_detailed = data.table::data.table(),
    top_reporters = top_reporters,
    top_partners = top_partners,
    eligible_reporters = eligible
  )
  expect_false(any(uni[included == TRUE]$iso3 == "EUR"))
  expect_true(all(c("USA", "CHN") %in% uni[included == TRUE]$iso3))
  expect_true(all(uni[included == TRUE]$entity_type == "country_or_economy"))
})

test_that("WDI request plan is deterministic with stable IDs", {
  cfg <- load_config("development", TEST_ROOT)
  cfg2 <- cfg
  tmp <- tempfile("wdi_plan_")
  dir.create(file.path(tmp, "data", "raw"), recursive = TRUE)
  cfg2[['paths']]$raw <- file.path(tmp, "data", "raw")
  p1 <- build_wdi_production_plan(cfg2, c("USA", "CHN", "DEU"),
                                  start_year = 2019, end_year = 2024, chunk_size = 10)
  p2 <- build_wdi_production_plan(cfg2, c("USA", "CHN", "DEU"),
                                  start_year = 2019, end_year = 2024, chunk_size = 10)
  expect_equal(sort(p1$request_id), sort(p2$request_id))
  expect_equal(data.table::uniqueN(p1$request_id), nrow(p1))
  expect_true(all(c("indicator_code", "requested_country_codes", "page") %in% names(p1)))
})

test_that("WDI pagination parsing and null value preservation", {
  body <- '[{"page":1,"pages":2,"per_page":1000,"total":2},[{"countryiso3code":"USA","country":{"value":"United States"},"date":"2023","value":null},{"countryiso3code":"USA","country":{"value":"United States"},"date":"2022","value":100.5}]]'
  parsed <- parse_wdi_payload(body)
  expect_equal(parsed$pages, 2L)
  dt <- wdi_records_to_dt(parsed$records, "NY.GDP.MKTP.CD", "GDP")
  expect_true(is.na(dt$value[dt$year == 2023]))
  expect_equal(dt$value[dt$year == 2022], 100.5)
})

test_that("WDI wide GDP per capita and division-by-zero protection", {
  long <- data.table::data.table(
    iso3 = c("USA", "USA", "ZZZ", "ZZZ"),
    country_name = c("USA", "USA", "Z", "Z"),
    world_bank_code = c("USA", "USA", "ZZZ", "ZZZ"),
    year = c(2023L, 2023L, 2023L, 2023L),
    indicator_code = c("NY.GDP.MKTP.CD", "SP.POP.TOTL", "NY.GDP.MKTP.CD", "SP.POP.TOTL"),
    indicator_name = "x",
    value = c(200, 100, 50, 0),
    source_updated_at = "t", ingested_at = "t", request_id = "r"
  )
  wide <- wdi_long_to_wide_production(long)
  expect_equal(wide[iso3 == "USA"]$gdp_per_capita_usd, 2)
  expect_true(is.na(wide[iso3 == "ZZZ"]$gdp_per_capita_usd))
  expect_false(any(is.infinite(wide$gdp_per_capita_usd), na.rm = TRUE))
})

test_that("enrichment preserves rows and prefixes macro fields", {
  trade <- data.table::data.table(
    year = 2023L, reporter_iso3 = "USA", reporter_name = "USA",
    partner_iso3 = "CHN", partner_code = "156", partner_name = "China",
    flow_code = "M", trade_value_usd = 10, ingested_at = "t",
    universe_checksum = "uv_x"
  )
  wide <- data.table::data.table(
    iso3 = c("USA", "CHN"), country_name = c("USA", "China"), year = 2023L,
    gdp_current_usd = c(100, 80), population_total = c(10, 20),
    cpi_index = c(1, 1), inflation_annual_pct = c(2, 3),
    gdp_per_capita_usd = c(10, 4),
    source_updated_at = "t", ingested_at = "t"
  )
  g <- enrich_trade_global(
    data.table::data.table(
      year = 2023L, reporter_iso3 = "USA", flow_code = "M",
      trade_value_usd = 10, partner_iso3 = "W00", partner_code = "0",
      ingested_at = "t"
    ),
    wide
  )
  expect_equal(g$n_before, g$n_after)
  expect_true("reporter_gdp_current_usd" %in% names(g$data))

  d <- enrich_trade_detailed(trade, wide, universe_checksum = "uv_x", production_status = "partial")
  expect_equal(d$n_before, d$n_after)
  expect_true(all(c("reporter_gdp_current_usd", "partner_gdp_current_usd") %in% names(d$data)))
  expect_equal(unique(d$data$production_status), "partial")
  expect_equal(unique(d$data$universe_checksum), "uv_x")
})

test_that("country-year analytics reconcile balance and totals", {
  trade <- data.table::data.table(
    year = c(2022L, 2022L, 2023L, 2023L),
    reporter_code = "842", reporter_iso3 = "USA", reporter_name = "USA",
    partner_iso3 = "W00", partner_code = "0",
    flow_code = c("M", "X", "M", "X"),
    trade_value_usd = c(40, 50, 45, 60),
    ingested_at = "t", source_updated_at = "t"
  )
  wide <- data.table::data.table(
    iso3 = "USA", country_name = "USA", year = c(2022L, 2023L),
    gdp_current_usd = c(1000, 1100), population_total = c(100, 110),
    cpi_index = 100, inflation_annual_pct = 2,
    gdp_per_capita_usd = c(10, 10),
    source_updated_at = "t", ingested_at = "t"
  )
  cy <- build_country_year_analytics(trade, wide)
  expect_equal(nrow(cy), 2L)
  row23 <- cy[year == 2023]
  expect_equal(row23$trade_balance_usd, 15)
  expect_equal(row23$total_trade_value_usd, 105)
  expect_false(any(is.infinite(unlist(row23)), na.rm = TRUE))
  expect_true(!is.na(row23$imports_yoy_pct))
})

test_that("corrupt WDI cache detected", {
  tmp <- tempfile("bad.json")
  writeLines("not-json", tmp)
  chk <- validate_wdi_raw_cache(tmp)
  expect_false(chk$ok)
  expect_equal(chk$reason, "corrupt_json")
})

test_that("macro coverage calculation works", {
  uni <- data.table::data.table(iso3 = c("USA", "CHN"), included = TRUE)
  long <- data.table::data.table(
    iso3 = c("USA", "USA"), year = 2023L,
    indicator_code = c("NY.GDP.MKTP.CD", "SP.POP.TOTL"),
    value = c(1, 2)
  )
  cov <- build_macro_coverage_report(long, uni)
  expect_true(all(c("coverage_pct", "expected_country_count") %in% names(cov)))
  expect_equal(cov[indicator_code == "NY.GDP.MKTP.CD"]$observed_value_count, 1L)
})
