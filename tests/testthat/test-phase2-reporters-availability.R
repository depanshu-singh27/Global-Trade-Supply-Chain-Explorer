test_that("reporter reference parsing extracts codes and rejects code 0", {
  body <- '{"results":[[{"id":4,"text":"Afghanistan","reporterCode":4,"reporterDesc":"Afghanistan","reporterCodeIsoAlpha2":"AF","reporterCodeIsoAlpha3":"AFG","entryEffectiveDate":"1900-01-01T00:00:00","isGroup":false},{"id":0,"text":"World","reporterCode":0,"reporterDesc":"World","reporterCodeIsoAlpha2":"W0","reporterCodeIsoAlpha3":"W00","entryEffectiveDate":"1900-01-01T00:00:00","isGroup":true}]]}'
  dt <- parse_reporters_reference(body)
  expect_true(nrow(dt) >= 2)
  filt <- filter_eligible_reporters(dt, years = 2019:2024)
  expect_false("0" %in% filt$eligible$reporter_code)
  expect_true(any(filt$excluded$exclusion_reason == "invalid_or_zero_reporter_code") ||
                any(filt$excluded$exclusion_reason == "source_metadata_group") ||
                any(filt$excluded$exclusion_reason == "special_or_defensive_denylist"))
  expect_true("AFG" %in% filt$eligible$iso3)
})

test_that("World partner code 0 is accepted as partner, not reporter", {
  expect_equal(as.character(0), "0")

  cfg <- load_config("development", TEST_ROOT)
  p <- build_global_hs85_plan(cfg, reporter_codes = "842", years = 2024, partner_code = "0")
  expect_equal(p$partner_code[1], "0")
  expect_equal(p$reporter_code[1], "842")
})

test_that("availability parsing and intersection work", {
  body <- '{"data":[{"period":"2024","reporterCode":842,"reporterISO":"USA","reporterDesc":"USA","classificationCode":"H6","classificationSearchCode":"HS","totalRecords":10,"firstReleased":"2025-01-01","lastReleased":"2025-01-02"}]}'
  av <- parse_availability_payload(body)
  expect_equal(av$reporter_code[1], "842")
  expect_equal(av$year[1], 2024L)
  elig <- data.table::data.table(reporter_code = c("842", "156"), iso3 = c("USA", "CHN"))
  inter <- intersect_reporters_with_availability(elig, av, years = 2019:2024)
  expect_equal(inter$coverage_by_year$n_reporters[1], 1L)
  choice <- choose_ranking_year(
    data.table::data.table(year = c(2019L, 2023L, 2024L), n_reporters = c(100L, 95L, 40L)),
    prefer = 2024L
  )
  expect_equal(choice$year, 2023L)
})

test_that("obsolete reporterCode=0 plan migration marks invalid", {
  cfg <- load_config("development", TEST_ROOT)
  tmp_root <- tempfile("gte_mig_")
  dir.create(file.path(tmp_root, "data", "interim"), recursive = TRUE)
  cfg2 <- cfg
  cfg2[['paths']]$interim <- file.path(tmp_root, "data", "interim")
  cfg2[['paths']]$processed <- file.path(tmp_root, "data", "processed")
  cfg2[['paths']]$raw <- file.path(tmp_root, "data", "raw")
  dir.create(cfg2[['paths']]$processed, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg2[['paths']]$raw, recursive = TRUE, showWarnings = FALSE)

  plan <- data.table::data.table(
    request_id = "req_old_zero",
    dataset_type = "trade_global_hs85_annual",
    reporter_code = "0",
    partner_code = "0",
    year = 2022L,
    flow_code = "M"
  )
  arrow::write_parquet(plan, request_plan_file(cfg2))
  st <- data.table::data.table(
    request_id = "req_old_zero",
    dataset_type = "trade_global_hs85_annual",
    status = "running",
    attempts = 1L,
    started_at = NA_character_,
    completed_at = NA_character_,
    http_status = NA_integer_,
    result_row_count = NA_integer_,
    raw_file = NA_character_,
    raw_checksum = NA_character_,
    error_category = NA_character_,
    error_message = NA_character_
  )
  save_state(st, cfg2)
  mig <- migrate_invalid_reporter_zero_state(cfg2)
  expect_equal(length(mig$invalid_ids), 1L)
  st2 <- load_state(cfg2)
  expect_equal(st2$status[1], "invalid")
  expect_equal(st2$error_category[1], "invalid_reporter_strategy_replanned")
  expect_false(any(st2$status == "running"))
})

test_that("select_requests_to_run skips invalid statuses", {
  st <- data.table::data.table(
    request_id = c("a", "b", "c"),
    dataset_type = "d",
    status = c("invalid", "planned", "retryable_failed"),
    attempts = 0L,
    started_at = NA_character_,
    completed_at = NA_character_,
    http_status = NA_integer_,
    result_row_count = NA_integer_,
    raw_file = NA_character_,
    raw_checksum = NA_character_,
    error_category = NA_character_,
    error_message = NA_character_
  )
  pending <- select_requests_to_run(st, retry_failed_only = FALSE, max_requests = 10)
  expect_false("a" %in% pending$request_id)
  expect_true(all(pending$request_id %in% c("b", "c")))
})
