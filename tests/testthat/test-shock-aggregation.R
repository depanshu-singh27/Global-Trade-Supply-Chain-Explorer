test_that("aggregations, rankings, concentration, GDP handling", {
  det <- make_shock_detailed_fixture()
  res <- run_shock_scenario(det, make_base_scenario(), coverage = make_shock_coverage())
  expect_true(nrow(res$reporter_impacts) > 0)
  expect_true(nrow(res$commodity_impacts) > 0)
  expect_true(nrow(res$supplier_impacts) > 0)
  expect_true(all(res$reporter_impacts$scenario_rank == seq_len(nrow(res$reporter_impacts))))

  expect_false(any(is.infinite(res$reporter_impacts$residual_unmet_pct_gdp), na.rm = TRUE))
  expect_true(isTRUE(res$reconciliation$reporter_residual_matches_edges))

  conc <- res$post_shock_dependency
  if (nrow(conc)) {
    expect_true(all(is.na(conc$post_supplier_hhi) |
                      (conc$post_supplier_hhi >= -1e-9 & conc$post_supplier_hhi <= 1 + 1e-9)))
  }

  ranked <- rank_shock_impacts(res$reporter_impacts, "residual_unmet_value_usd", 5L)
  expect_true(nrow(ranked) <= 5L)
})

test_that("scenario comparison and ranking movement", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  a <- run_shock_scenario(det, make_base_scenario(shock_size_pct = 10, substitution_mode = "none"), coverage = cov)
  b <- run_shock_scenario(det, make_base_scenario(shock_size_pct = 50, substitution_mode = "none"), coverage = cov)
  cmp <- compare_shock_scenarios(a, b)
  expect_true(is.list(cmp$totals))
  expect_true(cmp$totals$residual_diff > 0)
  expect_true(nrow(cmp$aligned_reporters) > 0)
})

test_that("identical inputs produce identical outputs; persistence safe", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  sc <- make_base_scenario()
  r1 <- run_shock_scenario(det, sc, coverage = cov)
  r2 <- run_shock_scenario(det, sc, coverage = cov)
  expect_equal(r1$result_hash, r2$result_hash)
  expect_equal(
    sum(r1$edge_impacts$residual_unmet_value_usd),
    sum(r2$edge_impacts$residual_unmet_value_usd)
  )

  tmp_root <- tempfile("shockroot")
  dir.create(file.path(tmp_root, "data", "scenarios", "results"), recursive = TRUE)

  writeLines("default:\n  app:\n    name: test\n", file.path(tmp_root, "config.yml"))
  persisted <- persist_shock_result(r1, root = tmp_root)
  expect_true(dir.exists(persisted$result_dir))
  man <- jsonlite::fromJSON(file.path(persisted$result_dir, "scenario_manifest.json"))
  expect_equal(man$engine_version, SHOCK_ENGINE_VERSION)

  def <- paste(readLines(file.path(persisted$result_dir, "scenario_definition.json")), collapse = "\n")
  expect_false(grepl("secret|COMTRADE|/secret/", def, ignore.case = TRUE))
})

test_that("example scenarios validate; engine status; no API calls", {
  ex <- list.files(
    file.path(TEST_ROOT, "data", "scenarios", "examples"),
    pattern = "\\.json$", full.names = TRUE
  )
  expect_true(length(ex) >= 4)
  for (f in ex) {
    sc <- read_shock_scenario_file(f)
    expect_true(nzchar(sc$scenario_id))
    expect_true(sc$shock_size_pct >= 0 && sc$shock_size_pct <= 100)
  }
  st <- shock_engine_status(list(
    detailed_coverage = make_shock_coverage()
  ))
  expect_true(isTRUE(st$ready))
  expect_true(grepl("analytical sensitivities", st$methodology_notice, ignore.case = TRUE))
  expect_false(grepl("GDP loss|forecast of realised", st$methodology_notice, ignore.case = TRUE))

  root <- TEST_ROOT
  for (f in c(
    "shock_formatters.R", "shock_scenario.R", "shock_validation.R",
    "shock_direct.R", "shock_substitution.R", "shock_propagation.R",
    "shock_aggregation.R", "shock_comparison.R", "shock_diagnostics.R",
    "shock_downloads.R", "mod_shock_simulator.R"
  )) {
    txt <- paste(readLines(file.path(root, "R", f), warn = FALSE), collapse = "\n")
    expect_false(grepl("comtrade_get|wdi_get|httr2::req_perform", txt))
  }
})

test_that("timing disabled by default; baseline cache key stable", {
  det <- make_shock_detailed_fixture()
  res <- run_shock_scenario(det, make_base_scenario(), coverage = make_shock_coverage())
  elapsed <- res$diagnostics[metric == "elapsed_ms"]$value
  expect_true(is.na(as.numeric(elapsed)) || identical(elapsed, "NA"))
  k1 <- shock_baseline_cache_key(2024L, 2024L, "uv_262deb46e00d2f216a5a", "partial")
  k2 <- shock_baseline_cache_key(2024L, 2024L, "uv_262deb46e00d2f216a5a", "partial")
  expect_identical(k1, k2)
  k3 <- shock_baseline_cache_key(2024L, 2024L, "uv_other", "partial")
  expect_false(identical(k1, k3))
})

test_that("scenario diagnostics keys are unique", {
  det <- make_shock_detailed_fixture()
  cov <- make_shock_coverage()
  res <- run_shock_scenario(det, make_base_scenario(), coverage = cov)
  expect_true(isTRUE(res$ok))
  diag <- res$diagnostics
  expect_true(is.data.frame(diag) || data.table::is.data.table(diag))
  expect_true("metric" %in% names(diag))
  expect_equal(anyDuplicated(diag$metric), 0L)
  expect_equal(sum(diag$metric == "represented_reporter_count"), 1L)
  expect_equal(sum(diag$metric == "selected_reporter_count"), 1L)
  expect_equal(
    diag[metric == "represented_reporter_count"]$value,
    as.character(cov$represented_reporter_count)
  )
  expect_equal(
    diag[metric == "selected_reporter_count"]$value,
    as.character(cov$selected_reporter_count)
  )
  expect_true("baseline_reporter_count" %in% diag$metric)
})

test_that("dependency sparse inputs remain unpadded under 200 nodes", {
  det <- make_shock_detailed_fixture()
  built <- construct_dependency_table(det, year_min = 2024L, year_max = 2024L)
  expect_true(nrow(built$shares) > 0)
  sp <- build_country_commodity_sparse_matrix(built$shares, max_nodes = 200L)
  expect_true(is.list(sp))
  expect_true(sp$n_nodes > 0L)
  expect_true(sp$n_nodes < 200L)
  expect_false(identical(sp$n_nodes, 200L))
})

test_that("minimal shiny backend status module", {
  snap <- shiny::reactiveVal(list(
    detailed_coverage = make_shock_coverage(),
    trade_detailed_enriched = make_shock_detailed_fixture(),
    map_geometry = NULL
  ))
  cfg <- shiny::reactive(list())
  shiny::testServer(mod_shock_simulator_server, args = list(snap = snap, cfg = cfg), {
    expect_equal(coverage()$universe_checksum, "uv_262deb46e00d2f216a5a")
    expect_equal(SHOCK_ENGINE_VERSION, "1.0.0-phase10")
    expect_true(nrow(detailed()) > 0)
  })
})
