test_that("target preview matches selection logic", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  bl <- build_shock_baseline(det, 2024, 2024, coverage = cov)$baseline
  sc <- shock_ui_to_scenario(make_shock_ui_inputs())
  prev <- shock_target_preview(bl, sc)
  tg <- select_shock_target_edges(bl, sc)
  expect_equal(prev$n_target_edges, nrow(tg))
  expect_equal(prev$targeted_baseline_value_usd, sum(tg$baseline_import_value_usd))
  expect_equal(prev$potential_direct_disruption_usd, sum(tg$baseline_import_value_usd) * 0.3)

  zero <- shock_target_preview(bl, shock_ui_to_scenario(make_shock_ui_inputs(shock_size_pct = 0)))
  expect_true(isTRUE(zero$neutral_control))
  expect_equal(zero$potential_direct_disruption_usd, 0)

  empty <- shock_target_preview(bl, shock_ui_to_scenario(make_shock_ui_inputs(target_supplier_iso3 = "ZZZ")))
  expect_equal(empty$n_target_edges, 0)
})

test_that("KPI, rankings, map, stale, comparison helpers", {
  res <- make_shock_ui_result()
  expect_true(isTRUE(res$ok))
  k <- shock_prepare_kpis(res)
  expect_true(k$available)
  expect_true(is.finite(k$direct_disrupted_imports_usd))
  expect_false(grepl("GDP loss|forecast of realised|predicted loss", shock_kpi_text_summary(k), ignore.case = TRUE))

  rr <- shock_rank_reporters_ui(res$reporter_impacts, "residual_unmet_value_usd", 5L)
  expect_true(nrow(rr) <= 5)
  expect_false(any(!is.finite(rr$residual_unmet_value_usd) & !is.na(rr$residual_unmet_value_usd)))

  cr <- shock_rank_commodities_ui(res$commodity_impacts, "residual_unmet_value_usd", 5L)
  expect_true(nrow(cr) >= 1)

  alloc <- shock_supplier_allocation_summary(res$supplier_impacts)
  expect_true(is.list(alloc$totals))

  conc <- shock_prepare_concentration_changes(res$post_shock_dependency)
  expect_true(is.data.frame(conc) || data.table::is.data.table(conc))

  map_p <- shock_prepare_map_impacts(
    res$reporter_impacts,
    metric = "residual_unmet_value_usd",
    represented = c("DEU", "IND", "KOR"),
    selected_universe = c("DEU", "IND", "KOR", "USA", "FRA")
  )
  expect_true(any(is.na(map_p$table$map_value)))
  expect_false(any(map_p$table$reporter_iso3 == "USA" & is.finite(map_p$table$map_value) & map_p$table$map_value == 0 &
                     !("USA" %in% res$reporter_impacts$reporter_iso3)))
  expect_true(grepl("Unavailable|unavailable|Missing", shock_map_text_summary(map_p), ignore.case = TRUE))

  paths <- shock_filter_propagation_paths(res$impact_paths)
  expect_equal(nrow(paths), 0)

  stale <- shock_stale_result_state(
    shock_ui_to_scenario(make_shock_ui_inputs(shock_size_pct = 50)),
    res,
    coverage = make_shock_coverage()
  )
  expect_true(isTRUE(stale$inputs_changed))

  stale_uv <- shock_stale_result_state(
    res$scenario,
    res,
    coverage = list(universe_checksum = "uv_other", production_status = "partial")
  )
  expect_true(isTRUE(stale_uv$stale))

  res2 <- make_shock_ui_result(shock_size_pct = 50, substitution_mode = "none")
  cmp <- shock_prepare_comparison_ui(res, res2)
  expect_true(isTRUE(cmp$ok))
  expect_true(nrow(cmp$kpi_diff) > 0)
  expect_true(any(is.na(cmp$kpi_diff$pct_diff) | is.finite(cmp$kpi_diff$pct_diff)))
})

test_that("downloads and report avoid forbidden content", {
  res <- make_shock_ui_result()
  meta <- shock_result_download_meta(res)
  dt <- shock_ui_download_table(res$reporter_impacts, meta)
  expect_false("raw_file" %in% names(dt))
  expect_true("engine_version" %in% names(dt))
  md <- shock_prepare_scenario_report_md(res)
  expect_false(grepl("predicted loss|GDP loss|expected shortage|250 ms", md, ignore.case = TRUE))
  expect_true(grepl("deterministic scenario sensitivities|analytical sensitiv", md, ignore.case = TRUE))
  expect_false(shock_contains_forbidden_download_content(md))
})

test_that("safe deletion and history indexing", {
  tmp <- tempfile("shockui")
  dir.create(file.path(tmp, "data", "scenarios", "results"), recursive = TRUE)
  dir.create(file.path(tmp, "data", "scenarios", "examples"), recursive = TRUE)

  bad <- shock_safe_delete_result("C:/Windows/System32", root = tmp)
  expect_false(isTRUE(bad$ok))

  res <- make_shock_ui_result()
  persisted <- persist_shock_result(res, root = tmp)
  hist <- shock_index_scenario_history(tmp)
  expect_true(nrow(hist) >= 1)
  loaded <- shock_load_persisted_result(persisted$result_dir)
  expect_true(isTRUE(loaded$ok))
  del <- shock_safe_delete_result(persisted$result_dir, root = tmp)
  expect_true(isTRUE(del$ok))
  miss <- shock_safe_delete_result(persisted$result_dir, root = tmp)
  expect_false(isTRUE(miss$ok))
})

test_that("orchestration calls shared engine and preserves source rows", {
  det <- make_shock_detailed_fixture()
  n0 <- nrow(det)
  cov <- make_shock_coverage()
  tmp <- tempfile("orch")
  dir.create(file.path(tmp, "data", "scenarios", "results"), recursive = TRUE)
  dir.create(file.path(tmp, "data", "scenarios", "definitions"), recursive = TRUE)
  dir.create(file.path(tmp, "data", "scenarios", "examples"), recursive = TRUE)
  out <- run_shock_scenario_orchestrated(
    det,
    make_base_scenario(),
    coverage = cov,
    root = tmp,
    persist = TRUE
  )
  expect_true(isTRUE(out$ok))
  expect_true(dir.exists(out$result_dir))
  expect_equal(nrow(det), n0)
})

test_that("no API calls in shock UI sources; no 250ms claim", {
  root <- TEST_ROOT
  files <- c(
    "shock_ui_state.R", "shock_ui_validation.R", "shock_ui_calculations.R",
    "shock_ui_charts.R", "shock_ui_maps.R", "shock_ui_comparison.R",
    "shock_ui_downloads.R", "shock_ui_persistence.R", "mod_shock_simulator.R"
  )
  for (f in files) {
    txt <- paste(readLines(file.path(root, "R", f), warn = FALSE), collapse = "\n")
    expect_false(grepl("comtrade_get|wdi_get|httr2::req_perform", txt))
    expect_false(grepl(paste0("COMTRADE", "_", "PRIMARY"), txt))
    expect_false(grepl("250\\s*ms|250ms", txt, ignore.case = TRUE))
    expect_false(grepl("predicted loss|expected GDP loss|guaranteed substitute", txt, ignore.case = TRUE))
  }
})
