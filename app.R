options(shiny.autoload.r = FALSE)

root <- getwd()
if (file.exists("renv/activate.R") &&
    !identical(tolower(Sys.getenv("GTSC_SKIP_RENV_ACTIVATE", "")), "true")) {
  source("renv/activate.R")
}

r_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)

priority <- c(
  "utilities.R", "config.R", "validation.R", "reference_data.R",
  "calculations.R", "app_theme.R", "overview_formatters.R",
  "overview_calculations.R", "overview_charts.R",
  "trade_flow_formatters.R", "trade_flow_calculations.R",
  "trade_flow_charts.R", "trade_flow_downloads.R",
  "map_formatters.R", "map_palettes.R", "map_geometry.R",
  "map_calculations.R", "map_downloads.R",
  "time_series_formatters.R", "time_series_calculations.R",
  "commodity_analysis.R", "time_series_charts.R", "time_series_downloads.R",
  "network_formatters.R", "network_construction.R", "network_centrality.R",
  "network_calculations.R", "network_charts.R", "network_downloads.R",
  "dependency_formatters.R", "dependency_construction.R", "dependency_metrics.R",
  "dependency_rankings.R", "dependency_matrix.R", "dependency_charts.R",
  "dependency_downloads.R",
  "shock_formatters.R", "shock_scenario.R", "shock_validation.R",
  "shock_direct.R", "shock_substitution.R", "shock_propagation.R",
  "shock_aggregation.R", "shock_comparison.R", "shock_diagnostics.R",
  "shock_downloads.R",
  "shock_ui_state.R", "shock_ui_validation.R", "shock_ui_calculations.R",
  "shock_ui_charts.R", "shock_ui_maps.R", "shock_ui_comparison.R",
  "shock_ui_downloads.R", "shock_ui_persistence.R",
  "pipeline_state.R", "comtrade_request_planner.R", "production_trade_pipeline.R",
  "forecast_formatters.R", "forecast_series_selection.R",
  "monthly_trade_request_planner.R", "monthly_trade_pipeline.R",
  "monthly_series_construction.R", "forecast_preprocessing.R",
  "forecast_models.R", "forecast_metrics.R", "forecast_backtesting.R",
  "forecast_selection.R", "forecast_diagnostics.R", "forecast_charts.R",
  "forecast_downloads.R", "forecast_pipeline_build.R",
  "performance_config.R", "performance_instrumentation.R",
  "performance_fixtures.R", "performance_memory.R", "performance_reactive.R",
  "performance_benchmarks.R", "performance_validation.R", "performance_reporting.R",
  "runtime_profile.R", "runtime_logging.R", "runtime_health.R",
  "release_config.R", "release_security.R", "release_bundle.R",
  "release_manifest.R", "release_validation.R", "final_audit.R",
  "data_access.R",
  "comtrade_client.R", "wdi_client.R",
  "mod_overview.R", "mod_trade_flows.R", "mod_trade_balance_map.R",
  "mod_time_series.R", "mod_network.R", "mod_dependency.R",
  "mod_shock_simulator.R", "mod_forecasting.R", "mod_data_quality.R",
  "app_ui.R", "app_server.R"
)
ordered <- file.path("R", priority)
ordered <- ordered[file.exists(ordered)]
rest <- setdiff(normalizePath(r_files, winslash = "/"),
                normalizePath(ordered, winslash = "/"))
for (f in c(ordered, rest)) source(f, local = FALSE)

runtime_cfg <- normalise_runtime_config()
set_runtime_config(runtime_cfg)
cfg <- load_config()
cfg <- apply_runtime_paths_to_config(cfg, runtime_cfg)

if (identical(runtime_cfg$runtime_profile, "demo") ||
    isTRUE(runtime_cfg$read_only_mode)) {

  if (isTRUE(runtime_cfg$allow_scenario_writes)) {
    scen <- runtime_cfg$scenario_root
    scen <- resolve_project_path(scen, cfg$project_root)
    dir.create(file.path(scen, "results"), recursive = TRUE, showWarnings = FALSE)
  }
} else {
  ensure_data_dirs(cfg)
}
write_health_www_resource(cfg$project_root %||% find_project_root(), runtime_cfg)
log_startup_metadata(runtime_cfg)
install_runtime_error_logger(public_mode = isTRUE(runtime_cfg$public_mode))

snapshot <- app_snapshot(cfg)
log_runtime_data_diagnostics(cfg, runtime_cfg, snapshot, app_snapshot_load_seconds())
assert_mandatory_snapshot_tables(snapshot)
runtime_log("INFO", "startup_snapshot_ready", list(
  runtime_profile = runtime_cfg$runtime_profile,
  load_seconds = round(app_snapshot_load_seconds(), 2)
))

ui <- app_ui(cfg)

materialise_web_dependencies <- identical(
  tolower(Sys.getenv("GTSC_MATERIALIZE_WEB_DEPS", "false")),
  "true"
)

static_dependency_root <- if (materialise_web_dependencies) {
  file.path(cfg$project_root %||% find_project_root(), "www")
} else {
  NULL
}

register_app_web_dependencies(
  ui,
  static_root = static_dependency_root
)

shiny::shinyApp(ui = ui, server = app_server(cfg, snapshot))
