.perf_meta <- function(cfg, env) list(cfg = cfg, env = env)

benchmark_snapshot_load <- function(cfg, env, app_cfg, cold = TRUE) {
  timed <- run_timed_iterations(
    function() {
      inc_perf_counter("snapshot_load_count")
      load_processed_snapshot(app_cfg)
    },
    iterations = cfg$iterations,
    warmup = if (isTRUE(cold)) 0L else cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  snap <- timed$result
  make_benchmark_row(
    timed, .perf_meta(cfg, env),
    operation = if (isTRUE(cold)) "snapshot_load_cold" else "snapshot_load_warm",
    module = "data_access",
    dataset_tier = "actual",
    dataset_mode = "actual_processed",
    input_rows = if (!is.null(snap$trade_detailed_enriched)) nrow(snap$trade_detailed_enriched) else NA_integer_,
    cold_or_warm = if (isTRUE(cold)) "cold" else "warm",
    cache_state = "filesystem",
    result_for_checksum = list(
      n_detailed = nrow(snap$trade_detailed_enriched %||% data.table::data.table()),
      n_cy = nrow(snap$country_year_analytics %||% data.table::data.table())
    )
  )
}

benchmark_geometry_load <- function(cfg, env, app_cfg, rebuild = FALSE) {
  timed <- run_timed_iterations(
    function() load_map_geometry(app_cfg, rebuild = rebuild),
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  g <- timed$result
  make_benchmark_row(
    timed, .perf_meta(cfg, env),
    operation = if (isTRUE(rebuild)) "geometry_build_uncached" else "geometry_load_cached",
    module = "map",
    dataset_tier = "actual",
    dataset_mode = "geometry_cache",
    input_rows = if (!is.null(g)) nrow(g) else NA_integer_,
    cold_or_warm = if (isTRUE(rebuild)) "cold" else "warm",
    cache_state = if (isTRUE(rebuild)) "uncached" else "cached",
    result_for_checksum = list(n = if (!is.null(g)) nrow(g) else 0L)
  )
}

benchmark_overview_ops <- function(cfg, env, snap) {
  cy <- snap$country_year_analytics %||% snap$map_analytics
  if (is.null(cy) || !nrow(cy)) {
    return(make_benchmark_row(
      list(iterations = 0, warmup_iterations = 0, minimum_ms = NA, median_ms = NA,
           mean_ms = NA, p95_ms = NA, maximum_ms = NA, memory_bytes = NA,
           benchmark_status = "skipped", warning = "no_country_year", result = NULL),
      .perf_meta(cfg, env), "overview_kpis", "overview", input_rows = 0L
    ))
  }
  cy <- data.table::as.data.table(cy)
  y <- max(cy$year, na.rm = TRUE)
  timed <- run_timed_iterations(
    function() {
      list(
        kpis = overview_kpi_global(cy, y),
        ranks = overview_top_economies(cy, y, top_n = 10L),
        trend = overview_trend_series(cy, reporter = "__GLOBAL__")
      )
    },
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  make_benchmark_row(
    timed, .perf_meta(cfg, env), "overview_year_aggregations", "overview",
    dataset_tier = "actual", input_rows = nrow(cy), cold_or_warm = "warm",
    result_for_checksum = timed$result
  )
}

benchmark_trade_flow_ops <- function(cfg, env, detailed, tier = "actual", mode = "actual_processed") {
  det <- data.table::as.data.table(detailed)
  if (!nrow(det)) {
    return(make_benchmark_row(
      list(iterations = 0, warmup_iterations = 0, minimum_ms = NA, median_ms = NA,
           mean_ms = NA, p95_ms = NA, maximum_ms = NA, memory_bytes = NA,
           benchmark_status = "skipped", warning = "no_detailed", result = NULL),
      .perf_meta(cfg, env), "trade_flow_filter_sankey", "trade_flows",
      dataset_tier = tier, input_rows = 0L
    ))
  }
  if (!"prepared" %in% names(attributes(det))) {
    det <- tryCatch(prepare_detailed_trade(det), error = function(e) det)
  }
  y <- max(det$year, na.rm = TRUE)
  timed <- run_timed_iterations(
    function() {
      inc_perf_counter("detailed_filter_count")
      filt <- filter_detailed_trade(det, year_min = y, year_max = y, flows = "M")
      paths <- trade_flow_path_aggregates(filt, grouping = "reporter_partner_commodity")
      top <- select_top_n_paths(paths, top_n = 25L)
      sank <- build_sankey_data(top$visible)
      list(
        n = nrow(filt),
        sankey = sank,
        total = sum(filt$trade_value_usd, na.rm = TRUE),
        coverage_pct = top$coverage_pct
      )
    },
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  make_benchmark_row(
    timed, .perf_meta(cfg, env), "trade_flow_filter_sankey", "trade_flows",
    dataset_tier = tier, dataset_mode = mode, input_rows = nrow(det),
    cold_or_warm = "warm", result_for_checksum = timed$result
  )
}

benchmark_map_ops <- function(cfg, env, snap) {
  cy <- snap$map_analytics %||% snap$country_year_analytics
  if (is.null(cy) || !nrow(cy)) {
    return(make_benchmark_row(
      list(iterations = 0, warmup_iterations = 0, minimum_ms = NA, median_ms = NA,
           mean_ms = NA, p95_ms = NA, maximum_ms = NA, memory_bytes = NA,
           benchmark_status = "skipped", warning = "no_map_analytics", result = NULL),
      .perf_meta(cfg, env), "map_year_prepare", "map", input_rows = 0L
    ))
  }
  cy <- prepare_map_analytics(data.table::as.data.table(cy))
  y <- max(cy$year, na.rm = TRUE)
  timed <- run_timed_iterations(
    function() {
      yr <- filter_map_year(cy, y)
      list(n = nrow(yr), total = sum(yr$total_trade_usd %||% 0, na.rm = TRUE))
    },
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  make_benchmark_row(
    timed, .perf_meta(cfg, env), "map_year_prepare", "map",
    dataset_tier = "actual", input_rows = nrow(cy), cold_or_warm = "warm",
    cache_state = "geometry_separate", result_for_checksum = timed$result
  )
}

benchmark_timeseries_ops <- function(cfg, env, snap) {
  cy <- snap$country_year_analytics
  if (is.null(cy) || !nrow(cy)) {
    return(make_benchmark_row(
      list(iterations = 0, warmup_iterations = 0, minimum_ms = NA, median_ms = NA,
           mean_ms = NA, p95_ms = NA, maximum_ms = NA, memory_bytes = NA,
           benchmark_status = "skipped", warning = "no_cy", result = NULL),
      .perf_meta(cfg, env), "timeseries_index_yoy", "time_series", input_rows = 0L
    ))
  }
  cy <- prepare_ts_global(data.table::as.data.table(cy))
  iso <- cy$reporter_iso3[1]
  timed <- run_timed_iterations(
    function() {
      s <- economy_metric_series(cy, iso, "total_trade")
      list(n = nrow(s), last = utils::tail(s$value %||% s[[ncol(s)]], 1))
    },
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  make_benchmark_row(
    timed, .perf_meta(cfg, env), "timeseries_economy_series", "time_series",
    dataset_tier = "actual", input_rows = nrow(cy), cold_or_warm = "warm",
    result_for_checksum = timed$result
  )
}

benchmark_network_ops <- function(cfg, env, detailed, tier = "actual", mode = "actual_processed") {
  det <- data.table::as.data.table(detailed)
  if (!nrow(det)) {
    return(make_benchmark_row(
      list(iterations = 0, warmup_iterations = 0, minimum_ms = NA, median_ms = NA,
           mean_ms = NA, p95_ms = NA, maximum_ms = NA, memory_bytes = NA,
           benchmark_status = "skipped", warning = "no_detailed", result = NULL),
      .perf_meta(cfg, env), "network_build_centrality", "network",
      dataset_tier = tier, input_rows = 0L
    ))
  }
  y <- max(det$year, na.rm = TRUE)
  timed <- run_timed_iterations(
    function() {
      inc_perf_counter("graph_build_count")
      net <- construct_network_edges(det, mode = "exports", year_min = y, year_max = y)
      nodes <- build_network_nodes(net$edges)
      g <- create_trade_igraph(net$edges, nodes = nodes, directed = TRUE)
      pr <- igraph::page_rank(g)$vector
      list(
        nodes = igraph::vcount(g),
        edges = igraph::ecount(g),
        pr_sum = sum(pr)
      )
    },
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  make_benchmark_row(
    timed, .perf_meta(cfg, env), "network_build_pagerank", "network",
    dataset_tier = tier, dataset_mode = mode, input_rows = nrow(det),
    nodes = timed$result$nodes, edges = timed$result$edges,
    cold_or_warm = "warm", result_for_checksum = timed$result
  )
}

benchmark_dependency_ops <- function(cfg, env, detailed, tier = "actual", mode = "actual_processed",
                                       max_nodes = NULL) {
  det <- data.table::as.data.table(detailed)
  if (!nrow(det)) {
    return(make_benchmark_row(
      list(iterations = 0, warmup_iterations = 0, minimum_ms = NA, median_ms = NA,
           mean_ms = NA, p95_ms = NA, maximum_ms = NA, memory_bytes = NA,
           benchmark_status = "skipped", warning = "no_detailed", result = NULL),
      .perf_meta(cfg, env), "dependency_build", "dependency",
      dataset_tier = tier, input_rows = 0L
    ))
  }
  y <- max(det$year, na.rm = TRUE)
  timed <- run_timed_iterations(
    function() {
      inc_perf_counter("dependency_build_count")
      dep <- construct_dependency_table(det, year_min = y, year_max = y)
      nodes <- data.table::uniqueN(paste(dep$shares$reporter_iso3, dep$shares$hs_code))
      list(nodes = nodes, n_shares = nrow(dep$shares))
    },
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  make_benchmark_row(
    timed, .perf_meta(cfg, env), "dependency_build", "dependency",
    dataset_tier = tier, dataset_mode = mode, input_rows = nrow(det),
    nodes = timed$result$nodes, cold_or_warm = "warm",
    result_for_checksum = timed$result
  )
}

benchmark_shock_engine <- function(cfg, env, detailed, coverage,
                                     label = "shock_capacity_constrained",
                                     include_propagation = TRUE,
                                     include_persistence = FALSE,
                                     scenario_builder = NULL) {
  det <- data.table::as.data.table(detailed)
  if (!nrow(det) || !exists("run_shock_scenario", mode = "function")) {
    return(make_benchmark_row(
      list(iterations = 0, warmup_iterations = 0, minimum_ms = NA, median_ms = NA,
           mean_ms = NA, p95_ms = NA, maximum_ms = NA, memory_bytes = NA,
           benchmark_status = "skipped", warning = "shock_unavailable", result = NULL),
      .perf_meta(cfg, env), label, "shock_engine", input_rows = nrow(det)
    ))
  }
  y <- max(det$year, na.rm = TRUE)
  partners <- unique(det[flow_code == "M" & year == y]$partner_iso3)
  partner <- if ("CHN" %in% partners) "CHN" else as.character(partners[1])
  hs <- unique(det[flow_code == "M" & year == y & partner_iso3 == partner]$hs_code)
  hs <- utils::head(as.character(hs), 3L)
  scen <- if (is.function(scenario_builder)) {
    scenario_builder(det, y, partner, hs)
  } else {
    list(
      scenario_name = paste("perf", label),
      baseline_year_start = as.integer(y),
      baseline_year_end = as.integer(y),
      target_supplier_iso3 = partner,
      target_hs_codes = hs,
      shock_size_pct = 30,
      substitution_mode = "capacity_constrained",
      substitution_capacity_pct = 25,
      maximum_substitute_supplier_share = 1,
      propagation_mode = if (isTRUE(include_propagation)) "first_order" else "direct_only",
      maximum_propagation_steps = if (isTRUE(include_propagation)) 2L else 1L,
      acknowledge_partial_coverage = TRUE,
      universe_version = coverage$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM,
      engine_version = if (exists("SHOCK_ENGINE_VERSION")) SHOCK_ENGINE_VERSION else "1.0.0-phase10"
    )
  }

  timed <- run_timed_iterations(
    function() {
      inc_perf_counter("shock_execution_count")
      out <- run_shock_scenario(det, scen, coverage = coverage)
      if (isTRUE(include_persistence) && isTRUE(out$ok) &&
          exists("persist_shock_scenario_result", mode = "function")) {

        tryCatch(persist_shock_scenario_result(out), error = function(e) NULL)
      }
      out
    },
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  res <- timed$result
  edge_n <- if (!is.null(res$edge_impacts)) nrow(res$edge_impacts) else NA_integer_
  make_benchmark_row(
    timed, .perf_meta(cfg, env), label, "shock_engine",
    dataset_tier = if (isTRUE(any(det$is_synthetic))) "synthetic" else "actual",
    dataset_mode = if (isTRUE(any(det$is_synthetic))) "synthetic_scaled" else "actual_processed",
    input_rows = nrow(det),
    nodes = NA_integer_,
    edges = edge_n,
    cold_or_warm = "warm",
    cache_state = if (include_persistence) "persist_included" else "persist_excluded",
    result_for_checksum = list(
      ok = isTRUE(res$ok),
      hash = res$scenario$scenario_hash %||% res$manifest$scenario_hash %||% NA_character_,
      residual = if (!is.null(res$edge_impacts) && "residual_unmet_usd" %in% names(res$edge_impacts)) {
        sum(res$edge_impacts$residual_unmet_usd, na.rm = TRUE)
      } else {
        NA_real_
      }
    )
  )
}

benchmark_forecast_ui_helpers <- function(cfg, env, app_cfg) {
  timed <- run_timed_iterations(
    function() {
      fs <- load_forecast_ui_snapshot(app_cfg)
      sid <- if (!is.null(fs$selected) && nrow(fs$selected)) fs$selected$series_id[1] else NA_character_
      inc_perf_counter("forecast_filter_count")
      series <- if (!is.na(sid)) fs$monthly_long[series_id == sid] else data.table::data.table()
      mets <- if (!is.na(sid) && !is.null(fs$metrics)) fs$metrics[series_id == sid] else data.table::data.table()
      list(
        n_series_rows = nrow(series),
        n_metric_rows = nrow(mets),
        data_mode = fs$profile$data_mode %||% "unknown",
        production = isTRUE(fs$profile$production_forecast_available)
      )
    },
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = cfg$profile_memory
  )
  row <- make_benchmark_row(
    timed, .perf_meta(cfg, env), "forecast_ui_filter_prepare", "forecasting",
    dataset_tier = "fixture",
    dataset_mode = "fixture_synthetic_non_production",
    input_rows = timed$result$n_series_rows,
    cold_or_warm = "warm",
    result_for_checksum = timed$result
  )
  row$warning <- "Fixture-labelled forecasting UI helpers only; not production monthly forecasting performance."
  row
}

benchmark_app_source <- function(cfg, env) {
  timed <- run_timed_iterations(
    function() {

      cfg_app <- load_config()
      list(name = cfg_app$app$name %||% "ok")
    },
    iterations = cfg$iterations,
    warmup = cfg$warmup_iterations,
    profile_memory = FALSE
  )
  make_benchmark_row(
    timed, .perf_meta(cfg, env), "config_load", "application",
    dataset_tier = "actual", cold_or_warm = "warm",
    cache_state = "n/a", result_for_checksum = timed$result
  )
}

run_analytics_benchmark_suite <- function(cfg = normalise_performance_config(),
                                            app_cfg = load_config()) {
  env <- capture_benchmark_environment(cfg)
  snap <- load_processed_snapshot(app_cfg)
  coverage <- snap$detailed_coverage %||% list(
    universe_checksum = EXPECTED_UNIVERSE_CHECKSUM,
    production_status = "partial",
    represented_reporter_count = 6L,
    selected_reporter_count = 20L
  )
  fx <- load_performance_fixtures(cfg)
  rows <- list(
    benchmark_snapshot_load(cfg, env, app_cfg, cold = TRUE),
    benchmark_snapshot_load(cfg, env, app_cfg, cold = FALSE),
    benchmark_geometry_load(cfg, env, app_cfg, rebuild = FALSE),
    benchmark_overview_ops(cfg, env, snap),
    benchmark_trade_flow_ops(cfg, env, snap$trade_detailed_enriched, "actual", "actual_processed"),
    benchmark_map_ops(cfg, env, snap),
    benchmark_timeseries_ops(cfg, env, snap),
    benchmark_network_ops(cfg, env, snap$trade_detailed_enriched, "actual", "actual_processed"),
    benchmark_dependency_ops(cfg, env, snap$trade_detailed_enriched, "actual", "actual_processed")
  )
  if (!is.null(fx$detailed) && nrow(fx$detailed)) {
    rows <- c(rows, list(
      benchmark_trade_flow_ops(cfg, env, fx$detailed, "synthetic", "synthetic_scaled"),
      benchmark_network_ops(cfg, env, fx$detailed, "synthetic", "synthetic_scaled"),
      benchmark_dependency_ops(cfg, env, fx$detailed, "synthetic", "synthetic_scaled")
    ))
  }

  rows[[length(rows) + 1L]] <- make_benchmark_row(
    list(iterations = 0, warmup_iterations = 0, minimum_ms = NA, median_ms = NA,
         mean_ms = NA, p95_ms = NA, maximum_ms = NA, memory_bytes = NA,
         benchmark_status = "unavailable",
         warning = tier3_unavailable_notice(coverage), result = NULL),
    .perf_meta(cfg, env), "tier3_complete_coverage", "tier3",
    dataset_tier = "future_complete", dataset_mode = "unavailable"
  )
  data.table::rbindlist(rows, fill = TRUE)
}

run_shock_benchmark_suite <- function(cfg = normalise_performance_config(),
                                        app_cfg = load_config()) {
  env <- capture_benchmark_environment(cfg)
  snap <- load_processed_snapshot(app_cfg)
  coverage <- snap$detailed_coverage %||% list(universe_checksum = EXPECTED_UNIVERSE_CHECKSUM)
  det <- snap$trade_detailed_enriched
  fx <- load_performance_fixtures(cfg)
  rows <- list(
    benchmark_shock_engine(cfg, env, det, coverage,
                           label = "shock_actual_capacity_no_persist",
                           include_propagation = FALSE, include_persistence = FALSE),
    benchmark_shock_engine(cfg, env, det, coverage,
                           label = "shock_actual_capacity_propagation",
                           include_propagation = cfg$include_propagation,
                           include_persistence = FALSE)
  )
  if (isTRUE(cfg$include_persistence)) {
    rows[[length(rows) + 1L]] <- benchmark_shock_engine(
      cfg, env, det, coverage,
      label = "shock_actual_end_to_end_persist",
      include_propagation = FALSE, include_persistence = TRUE
    )
  }
  if (!is.null(fx$detailed) && nrow(fx$detailed)) {
    fx_cov <- coverage
    fx_cov$production_status <- "synthetic_benchmark"
    rows[[length(rows) + 1L]] <- benchmark_shock_engine(
      cfg, env, fx$detailed, fx_cov,
      label = "shock_synthetic_200node_capacity",
      include_propagation = FALSE, include_persistence = FALSE
    )
  }
  data.table::rbindlist(rows, fill = TRUE)
}

run_shiny_helper_benchmark_suite <- function(cfg = normalise_performance_config(),
                                               app_cfg = load_config()) {
  env <- capture_benchmark_environment(cfg)
  rows <- list(
    benchmark_app_source(cfg, env),
    benchmark_forecast_ui_helpers(cfg, env, app_cfg)
  )
  data.table::rbindlist(rows, fill = TRUE)
}
