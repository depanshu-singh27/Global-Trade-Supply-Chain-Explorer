test_that("fixture provenance is explicit and not production", {
  bundle <- make_forecast_fixture_bundle(3L)
  state <- data.table::data.table(request_id = "r1", status = "planned")
  prof <- build_forecast_profile(
    bundle$candidates,
    compute_monthly_series_quality(bundle$monthly_long),
    select_stable_forecast_series(compute_monthly_series_quality(bundle$monthly_long), bundle$candidates, n = 2L),
    metrics = data.table::data.table(mape = c(5, 7)),
    selected_models = data.table::data.table(series_id = bundle$candidates$series_id[1:2], selected_model_id = "naive"),
    forecasts = data.table::data.table(),
    monthly_state = state,
    monthly_long = bundle$monthly_long,
    force_fixture = TRUE
  )
  expect_equal(prof$data_mode, FORECAST_DATA_MODE_FIXTURE)
  expect_equal(prof$data_source, FORECAST_DATA_SOURCE_FIXTURE)
  expect_false(isTRUE(prof$production_forecast_available))
  expect_equal(prof$live_monthly_successful_requests, 0L)
  expect_false(isTRUE(prof$mape_claim_below_15))
  expect_true(grepl("not production accuracy|synthetic", prof$fixture_accuracy_disclaimer, ignore.case = TRUE))
  expect_true(grepl("Synthetic fixture", forecast_fixture_notice(), fixed = TRUE))
})

test_that("unlabeled fixture markers cannot load as production", {
  bundle <- make_forecast_fixture_bundle(2L)
  bad <- list(
    production_forecast_available = TRUE,
    mape_claim_below_15 = TRUE,
    median_mape = 6.5
  )
  fixed <- normalize_forecast_profile(
    bad,
    monthly_long = bundle$monthly_long,
    candidates = bundle$candidates,
    monthly_state = data.table::data.table(status = "planned")
  )
  expect_equal(fixed$data_mode, FORECAST_DATA_MODE_FIXTURE)
  expect_false(isTRUE(fixed$production_forecast_available))
  expect_equal(fixed$live_monthly_successful_requests, 0L)
  expect_false(isTRUE(fixed$mape_claim_below_15))
})

test_that("live rebuild provenance replaces fixture state", {
  live_state <- data.table::data.table(
    request_id = c("a", "b"),
    status = c("succeeded", "planned")
  )
  live <- resolve_forecast_provenance(
    data_mode = FORECAST_DATA_MODE_LIVE,
    data_source = FORECAST_DATA_SOURCE_LIVE,
    monthly_state = live_state,
    monthly_long = data.table::data.table(request_id = "monthly_DEU__CHN__8542__M_201901_202412_HS"),
    force_fixture = FALSE
  )
  expect_equal(live$data_mode, FORECAST_DATA_MODE_LIVE)
  expect_true(isTRUE(live$production_forecast_available))
  expect_equal(live$live_monthly_successful_requests, 1L)

  blocked <- resolve_forecast_provenance(
    data_mode = FORECAST_DATA_MODE_LIVE,
    monthly_state = data.table::data.table(status = "planned"),
    monthly_long = data.table::data.table(request_id = "fixture_DEU__CHN__8542__M"),
    force_fixture = FALSE
  )
  expect_equal(blocked$data_mode, FORECAST_DATA_MODE_FIXTURE)
  expect_false(isTRUE(blocked$production_forecast_available))
  expect_equal(blocked$live_monthly_successful_requests, 0L)
})

test_that("downloads always include data_mode and data_source", {
  dt <- data.table::data.table(series_id = "DEU__CHN__8542__M", predicted_value_usd = 1)
  meta <- forecast_download_provenance_meta(list(
    data_mode = FORECAST_DATA_MODE_FIXTURE,
    data_source = FORECAST_DATA_SOURCE_FIXTURE,
    is_fixture = TRUE,
    production_forecast_available = FALSE,
    live_monthly_successful_requests = 0L,
    mape_claim_below_15 = FALSE
  ))
  out <- forecast_download_table(dt, meta)
  expect_true(all(c("data_mode", "data_source") %in% names(out)))
  expect_equal(unique(out$data_mode), FORECAST_DATA_MODE_FIXTURE)
  expect_equal(unique(out$data_source), FORECAST_DATA_SOURCE_FIXTURE)
  expect_equal(unique(out$accuracy_scope), "fixture_diagnostic_only_not_production_accuracy")
  expect_false(any(out$mape_claim_below_15 %||% FALSE))
})

test_that("persist writes fixture labels into profile and manifest", {
  bundle <- make_forecast_fixture_bundle(2L)
  q <- compute_monthly_series_quality(bundle$monthly_long)
  sel <- select_stable_forecast_series(q, bundle$candidates, n = 2L)
  tmp <- tempfile("fcprov")
  dir.create(file.path(tmp, "processed"), recursive = TRUE)
  cfg <- list(paths = list(processed = file.path(tmp, "processed"), interim = file.path(tmp, "interim"), raw = file.path(tmp, "raw")))
  prof <- build_forecast_profile(
    bundle$candidates, q, sel,
    metrics = data.table::data.table(mape = 8),
    selected_models = data.table::data.table(series_id = sel$series_id, selected_model_id = "naive"),
    forecasts = data.table::data.table(),
    monthly_long = bundle$monthly_long,
    monthly_state = data.table::data.table(status = "planned"),
    force_fixture = TRUE
  )
  out <- persist_phase12_outputs(
    bundle$candidates, bundle$monthly_long, q, sel,
    backtests = data.table::data.table(),
    metrics = data.table::data.table(mape = 8),
    selected_models = data.table::data.table(series_id = sel$series_id, selected_model_id = "naive"),
    forecasts = data.table::data.table(),
    residuals = data.table::data.table(),
    validation = data.table::data.table(),
    profile = prof,
    cfg = cfg
  )
  expect_equal(out$profile$data_mode, FORECAST_DATA_MODE_FIXTURE)
  expect_false(isTRUE(out$profile$production_forecast_available))
  expect_equal(out$manifest$data_mode, FORECAST_DATA_MODE_FIXTURE)
  expect_false(isTRUE(out$manifest$production_forecast_available))
  expect_equal(out$manifest$live_monthly_successful_requests, 0L)
  expect_false(isTRUE(out$manifest$mape_claim_below_15))
  disk_prof <- jsonlite::fromJSON(file.path(cfg$paths$processed, "forecast_profile.json"))
  disk_man <- jsonlite::fromJSON(file.path(cfg$paths$processed, "forecast_pipeline_manifest.json"))
  expect_equal(disk_prof$data_mode, FORECAST_DATA_MODE_FIXTURE)
  expect_equal(disk_man$data_source, FORECAST_DATA_SOURCE_FIXTURE)
  expect_true(file.exists(file.path(cfg$paths$processed, "forecast_data_provenance.json")))
})
