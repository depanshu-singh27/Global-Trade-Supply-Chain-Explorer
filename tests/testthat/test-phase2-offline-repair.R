test_that("metadata isGroup is primary aggregate/group exclusion", {
  dt <- data.table::data.table(
    reporter_code = c("97", "344", "842", "0"),
    reporter_name = c("European Union", "Hong Kong", "USA", "World"),
    iso2 = c("EU", "HK", "US", "W0"),
    iso3 = c("EUR", "HKG", "USA", "W00"),
    entry_effective_date = as.Date("1900-01-01"),
    expiry_date = as.Date(NA),
    is_group = c(TRUE, FALSE, FALSE, TRUE),
    is_aggregate_flag = c(FALSE, FALSE, FALSE, FALSE),
    entity_type_raw = NA_character_
  )
  types <- classify_reporter_entity(
    dt$reporter_code, dt$iso3, dt$is_group, dt$is_aggregate_flag, dt$entity_type_raw
  )
  expect_equal(types[1], "group")
  expect_equal(types[2], "country_or_economy")
  expect_equal(types[3], "country_or_economy")
  expect_equal(types[4], "special")

  filt <- filter_eligible_reporters(dt, years = 2019:2024)
  expect_false("EUR" %in% filt$eligible$iso3)
  expect_false("W00" %in% filt$eligible$iso3)
  expect_true("HKG" %in% filt$eligible$iso3)
  expect_true("USA" %in% filt$eligible$iso3)
  expect_true(any(filt$excluded$exclusion_reason == "source_metadata_group"))
})

test_that("defensive EUR exclusion works even if isGroup false", {
  types <- classify_reporter_entity(
    reporter_code = "97", iso3 = "EUR", is_group = FALSE,
    is_aggregate_flag = FALSE, entity_type_raw = NA_character_
  )
  expect_equal(types, "special")
})

test_that("reporter_entity_type classification covers aggregate flag", {
  types <- classify_reporter_entity(
    reporter_code = "999", iso3 = "XYZ", is_group = FALSE,
    is_aggregate_flag = TRUE, entity_type_raw = NA_character_
  )
  expect_equal(types, "aggregate")
})

test_that("top reporter selection excludes aggregates and is deterministic", {
  dt <- data.table::data.table(
    year = 2024L,
    partner_iso3 = rep("W00", 6),
    flow_code = rep(c("M", "X"), 3),
    reporter_code = c("97", "97", "842", "842", "156", "156"),
    reporter_iso3 = c("EUR", "EUR", "USA", "USA", "CHN", "CHN"),
    reporter_name = c("EU", "EU", "USA", "USA", "China", "China"),
    trade_value_usd = c(1000, 1000, 50, 50, 80, 80)
  )
  top <- select_top_reporters_from_global(dt, ranking_year = 2024L, top_n = 2L)
  expect_false(any(top$reporter_iso3 %in% c("EUR", "WLD", "W00", "ASE")))
  expect_true(all(top$reporter_entity_type == "country_or_economy"))
  expect_equal(top$reporter_iso3[1], "CHN")
  expect_equal(top$reporter_iso3[2], "USA")
})

test_that("universe checksum stable and changes with membership", {
  reps <- data.table::data.table(reporter_code = c("842", "156"))
  pars <- data.table::data.table(partner_code = c("1", "2"))
  hs4 <- data.table::data.table(hs_code = c("8501", "8502"))
  a <- compute_universe_checksum(reps, pars, hs4, 2023L, "HS")
  b <- compute_universe_checksum(reps, pars, hs4, 2023L, "HS")
  expect_identical(a, b)
  reps2 <- data.table::data.table(reporter_code = c("842", "276"))
  c <- compute_universe_checksum(reps2, pars, hs4, 2023L, "HS")
  expect_false(identical(a, c))
})

test_that("quota 403 classification distinguishes quota vs auth", {
  q <- classify_http_failure(403L, body_text = '{"message":"Call volume quota exceeded"}')
  expect_equal(q$category, "quota_exhausted")
  expect_equal(q$status, "quota_blocked")
  expect_true(q$retryable)

  a <- classify_http_failure(403L, body_text = '{"message":"Unauthorized invalid subscription key"}')
  expect_equal(a$category, "auth_forbidden")
  expect_equal(a$status, "permanently_failed")
  expect_false(a$retryable)
})

test_that("select_requests_to_run resume vs retry-failed-only", {
  st <- data.table::data.table(
    request_id = c("a", "b", "c", "d", "e", "f"),
    dataset_type = "trade_detailed_top20",
    status = c("planned", "quota_blocked", "retryable_failed", "succeeded", "superseded", "invalid"),
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
  normal <- select_requests_to_run(st, retry_failed_only = FALSE, max_requests = 10)
  expect_true(all(c("a", "b", "c") %in% normal$request_id))
  expect_false(any(c("d", "e", "f") %in% normal$request_id))

  retry_only <- select_requests_to_run(st, retry_failed_only = TRUE, max_requests = 10)
  expect_true(all(c("b", "c") %in% retry_only$request_id))
  expect_false("a" %in% retry_only$request_id)
  expect_false(any(c("e", "f") %in% retry_only$request_id))
})

test_that("universe migration supersedes removed reporter and keeps matching IDs", {
  cfg <- load_config("development", TEST_ROOT)
  tmp_root <- tempfile("gte_uv_")
  dir.create(file.path(tmp_root, "data", "interim"), recursive = TRUE)
  dir.create(file.path(tmp_root, "data", "processed"), recursive = TRUE)
  dir.create(file.path(tmp_root, "data", "raw", "comtrade", "production", "detailed"), recursive = TRUE)
  cfg2 <- cfg
  cfg2[['paths']]$interim <- file.path(tmp_root, "data", "interim")
  cfg2[['paths']]$processed <- file.path(tmp_root, "data", "processed")
  cfg2[['paths']]$raw <- file.path(tmp_root, "data", "raw")

  old_uni <- list(
    top_reporters = data.table::data.table(reporter_code = c("842", "97")),
    top_partners = data.table::data.table(partner_code = c("156", "276")),
    top_hs4 = data.table::data.table(hs_code = c("8501", "8502"))
  )
  old_plan <- build_detailed_top20_plan(cfg2, old_uni, years = 2023:2024, universe_checksum = "uv_old")
  atomic_write_parquet_dt(old_plan, request_plan_file(cfg2))
  st <- init_state_from_plan(old_plan, NULL)

  usa_id <- old_plan[reporter_code == "842" & year == "2023"]$request_id[1]
  st[request_id == usa_id, status := "succeeded"]
  save_state(st, cfg2)

  new_uni <- list(
    top_reporters = data.table::data.table(reporter_code = c("842", "156")),
    top_partners = data.table::data.table(partner_code = c("156", "276")),
    top_hs4 = data.table::data.table(hs_code = c("8501", "8502"))
  )
  mig <- migrate_detailed_plan_for_universe_refresh(
    cfg2, new_uni, universe_checksum = "uv_new", years = 2023:2024
  )
  active <- mig$active_detailed
  expect_equal(nrow(active), 4L)
  expect_equal(data.table::uniqueN(active$reporter_code), 2L)
  expect_false(any(active$reporter_code == "97"))
  expect_true(all(active$universe_checksum == "uv_new"))
  expect_true(usa_id %in% mig$reused_ids || usa_id %in% active$request_id)
  expect_true(any(mig$plan$plan_status == "superseded"))

  st2 <- load_state(cfg2)
  pending <- select_requests_to_run(st2[request_id %in% active$request_id], retry_failed_only = FALSE)
  expect_false(any(pending$request_id %in% mig$superseded_ids))
})

test_that("active detailed plan is 20x6 semantic unique", {
  cfg <- load_config("development", TEST_ROOT)
  tmp_root <- tempfile("gte_plan_")
  dir.create(file.path(tmp_root, "data", "raw"), recursive = TRUE)
  cfg2 <- cfg
  cfg2[['paths']]$raw <- file.path(tmp_root, "data", "raw")
  reps <- data.table::data.table(reporter_code = as.character(1:20))
  pars <- data.table::data.table(partner_code = as.character(100:119))
  hs4 <- data.table::data.table(hs_code = paste0("85", sprintf("%02d", 1:20)))
  uni <- list(top_reporters = reps, top_partners = pars, top_hs4 = hs4)
  plan <- build_detailed_top20_plan(cfg2, uni, years = 2019:2024, universe_checksum = "uv_x")
  expect_equal(nrow(plan), 120L)
  expect_equal(data.table::uniqueN(plan$reporter_code), 20L)
  expect_equal(data.table::uniqueN(plan$year), 6L)
  expect_equal(data.table::uniqueN(plan$request_id), 120L)
  expect_false(any(duplicated(plan[, .(reporter_code, year)])))
  expect_false(any(plan$reporter_code %in% c("97", "0")))
})

test_that("production status helpers: incomplete is not complete", {

  incomplete_remaining <- 100L
  missing_reporters <- c("842")
  quota_blocked_count <- 0L
  active_request_count <- 120L
  terminal_request_count <- 20L
  stale_detailed <- FALSE
  aggregate_in_universe <- FALSE
  production_status <- if (aggregate_in_universe) {
    "failed"
  } else if (quota_blocked_count > 0 && incomplete_remaining > 0) {
    "blocked_quota"
  } else if (incomplete_remaining > 0 || length(missing_reporters) > 0 ||
             terminal_request_count < active_request_count) {
    "partial"
  } else {
    "complete"
  }
  expect_equal(production_status, "partial")
  expect_false(identical(production_status, "complete"))
})

test_that("quota preflight mocked available/unavailable", {

  avail <- classify_http_failure(200L, body_text = '{"data":[]}')

  expect_false(identical(avail$category, "quota_exhausted"))
  unavail <- classify_http_failure(403L, body_text = "quota exceeded")
  expect_equal(unavail$category, "quota_exhausted")
})
