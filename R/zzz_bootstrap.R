source_project_r <- function(root = getwd()) {
  if (file.exists(file.path(root, "renv/activate.R"))) {
    source(file.path(root, "renv/activate.R"), local = FALSE)
  }
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
    "comtrade_client.R", "wdi_client.R"
  )
  for (f in priority) {
    path <- file.path(root, "R", f)
    if (file.exists(path)) source(path, local = FALSE)
  }
  invisible(TRUE)
}

set_project_wd <- function() {

  root <- find_project_root(getwd())
  setwd(root)
  root
}
