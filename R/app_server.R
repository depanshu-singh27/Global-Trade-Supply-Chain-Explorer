GTSC_MODULE_IDS <- c(
  "overview", "trade_flows", "trade_balance", "time_series", "network",
  "dependency", "shock", "forecast", "data_quality"
)

app_server <- function(cfg, snapshot = NULL) {
  shared_snapshot <- snapshot %||% app_snapshot(cfg)

  function(input, output, session) {
    cfg_reactive <- shiny::reactive(cfg)
    active_nav <- shiny::reactive(input$main_nav)

    snap <- shiny::reactiveVal(shared_snapshot)

    mod_overview_server("overview", snap, cfg_reactive)
    mod_trade_flows_server("trade_flows", snap, cfg_reactive)
    mod_trade_balance_map_server(
      "trade_balance", snap, cfg_reactive, active_nav = active_nav
    )
    mod_time_series_server("time_series", snap, cfg_reactive)
    mod_network_server("network", snap, cfg_reactive)
    mod_dependency_server("dependency", snap, cfg_reactive)
    mod_shock_simulator_server("shock", snap, cfg_reactive)
    mod_forecasting_server("forecast", snap, cfg_reactive)
    mod_data_quality_server("data_quality", snap)

    session$userData$gtsc_registered_modules <- GTSC_MODULE_IDS
    log_module_registration(GTSC_MODULE_IDS)
  }
}
