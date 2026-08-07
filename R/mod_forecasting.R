mod_forecasting_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "forecast-page",
      shiny::div(
        class = "hero-panel",
        shiny::h2("Forecasting"),
        shiny::p(
          "Monthly statistical extrapolations of historical reported trade for selected ",
          "reporter–partner–HS4–flow series. Not predictions of policy, disruptions or realised outcomes."
        )
      ),
      shiny::uiOutput(ns("notices")),
      shiny::uiOutput(ns("filter_bar")),
      shiny::uiOutput(ns("kpi_strip")),
      shiny::div(
        class = "forecast-grid",
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Historical series and forecast"),
          plotly::plotlyOutput(ns("history_plot"), height = "420px"),
          shiny::uiOutput(ns("history_text"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Model leaderboard"),
          DT::DTOutput(ns("leaderboard"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Backtest performance"),
          DT::DTOutput(ns("backtest_table")),
          shiny::p(class = "muted", "Backtests use historical holdouts, not future observations.")
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Residual diagnostics"),
          DT::DTOutput(ns("residual_table"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Series quality"),
          shiny::uiOutput(ns("quality_panel"))
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Forecast table"),
          DT::DTOutput(ns("forecast_table"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Coverage & methodology"),
          shiny::uiOutput(ns("coverage_panel"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Downloads"),
          shiny::p(class = "muted", "Exports respect the active series filter. No secrets or paths."),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(ns("dl_series"), "Monthly series CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_fc"), "Forecast CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_bt"), "Backtest CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_met"), "Metrics CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_res"), "Residuals CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_qual"), "Quality CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_board"), "Leaderboard CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_meta"), "Metadata JSON", class = "btn-sm btn-outline-primary")
          )
        )
      )
    )
  )
}

mod_forecasting_server <- function(id, snap, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    forecast_loaded <- shiny::reactiveVal(NULL)
    forecast_snap <- shiny::reactive({
      cached <- forecast_loaded()
      if (!is.null(cached)) return(cached)
      cfg_now <- cfg()
      loaded <- load_forecast_ui_snapshot(cfg_now)
      loaded$detailed_coverage <- snap()$detailed_coverage
      forecast_loaded(loaded)
      loaded
    })

    series_choices <- shiny::reactive({
      sel <- forecast_snap()$selected
      if (is.null(sel) || !nrow(sel)) return(character())
      stats::setNames(sel$series_id, sel$series_id)
    })

    selected_series_id <- shiny::reactive({
      sid <- input$series_id

      if (is.null(sid) || !length(sid)) {
        return("")
      }

      sid <- as.character(sid[[1L]])

      if (is.na(sid)) "" else sid
    })

    output$notices <- shiny::renderUI({
      cov <- forecast_snap()$detailed_coverage %||% list()
      prof <- forecast_snap()$profile %||% list()
      prop <- prophet_availability()
      fixture_mode <- isTRUE(prof$is_fixture) || identical(prof$data_mode, FORECAST_DATA_MODE_FIXTURE)
      shiny::tagList(
        if (isTRUE(fixture_mode)) {
          shiny::div(
            class = "partial-data-notice forecast-fixture-banner",
            role = "alert",
            shiny::tags$strong("Synthetic fixture results"),
            shiny::p(forecast_fixture_notice())
          )
        },
        shiny::div(
          class = "status-badge-row",
          status_badge("Forecast engine", "complete", label_override = FORECAST_ENGINE_VERSION),
          status_badge(
            "Data mode",
            if (isTRUE(fixture_mode)) "warning" else "complete",
            label_override = if (isTRUE(fixture_mode)) "synthetic fixtures" else "live monthly"
          ),
          status_badge(
            "Production forecasts",
            if (isTRUE(prof$production_forecast_available)) "complete" else "unavailable",
            label_override = if (isTRUE(prof$production_forecast_available)) "available" else "unavailable"
          ),
          status_badge(
            "Annual detailed",
            cov$production_status %||% "partial",
            label_override = sprintf(
              "%d/%d",
              cov$represented_reporter_count %||% 0L,
              cov$selected_reporter_count %||% 0L
            )
          ),
          status_badge(
            "Prophet",
            if (isTRUE(prop$available)) "complete" else "unavailable",
            label_override = if (isTRUE(prop$available)) "available" else "unavailable"
          )
        ),
        shiny::div(
          class = "partial-data-notice forecast-partial-notice",
          role = "status",
          shiny::tags$strong(forecast_partial_notice(
            cov$represented_reporter_count, cov$selected_reporter_count, coverage = cov
          ))
        ),
        shiny::div(
          class = "partial-data-notice forecast-method-notice",
          role = "note",
          shiny::tags$strong(forecast_methodology_notice())
        )
      )
    })

    output$filter_bar <- shiny::renderUI({
      ch <- series_choices()
      shiny::div(
        class = "filter-toolbar forecast-toolbar",
        shiny::selectInput(ns("series_id"), "Series", choices = ch, selected = if (length(ch)) ch[[1]] else NULL),
        shiny::selectInput(
          ns("model_mode"), "Model mode",
          c("Selected model" = "selected", "Manual model" = "manual")
        ),
        shiny::selectInput(ns("model_id"), "Manual model", forecast_model_choices()),
        shiny::selectInput(ns("horizon_view"), "Horizon focus", c("1" = 1, "3" = 3, "6" = 6, "12" = 12), selected = 1),
        shiny::selectInput(ns("interval_level"), "Interval", c("80%" = "80", "95%" = "95"), selected = "80"),
        shiny::checkboxInput(ns("show_imputed"), "Show imputed model-input months", TRUE)
      )
    })

    active_series <- shiny::reactive({
      sid <- selected_series_id()
      long <- forecast_snap()$monthly_long
      if (is.null(long) || !nrow(long) || !nzchar(sid %||% "")) return(data.table::data.table())
      data.table::as.data.table(long)[series_id == sid][order(date)]
    })

    manual_fc_cache <- shiny::reactiveValues(store = list())

    active_model <- shiny::reactive({
      sid <- selected_series_id()
      if (identical(input$model_mode, "manual")) {
        mid <- normalise_forecast_model_id(input$model_id %||% "seasonal_naive")
        if (is.na(mid)) mid <- "seasonal_naive"
        return(mid)
      }
      sm <- forecast_snap()$selected_models
      if (is.null(sm) || !nrow(sm)) return("seasonal_naive")
      row <- sm[series_id == sid]
      if (!nrow(row) || is.na(row$selected_model_id[1])) return("seasonal_naive")
      normalise_forecast_model_id(row$selected_model_id[1]) %||% "seasonal_naive"
    })

    active_forecast <- shiny::reactive({
      sid <- as.character(selected_series_id() %||% "")[1]
      mid <- active_model()
      if (!nzchar(sid) || is.na(mid) || !nzchar(mid)) return(data.table::data.table())
      fc <- forecast_snap()$forecasts
      out <- data.table::data.table()
      if (!is.null(fc) && nrow(fc)) {
        out <- data.table::as.data.table(fc)[series_id == sid]
        if ("model_id" %in% names(out)) {
          out[, model_id := vapply(model_id, normalise_forecast_model_id, character(1))]
          out <- out[model_id == mid]
        }
      }

      if ((!nrow(out) || !any(is.finite(out$predicted_value_usd))) &&
          identical(input$model_mode, "manual")) {
        key <- paste(sid, mid, sep = "::")
        cached <- manual_fc_cache$store[[key]]
        if (is.null(cached)) {
          long <- forecast_snap()$monthly_long
          cached <- generate_series_model_forecast(long, sid, mid, horizon = 12L)
          store <- manual_fc_cache$store
          store[[key]] <- cached
          manual_fc_cache$store <- store
        }
        out <- cached
      }
      if (nrow(out) && !("date" %in% names(out)) && "forecast_date" %in% names(out)) {
        out[, date := as.Date(forecast_date)]
      }
      if (nrow(out)) data.table::setorderv(out, "horizon")
      out
    })

    output$kpi_strip <- shiny::renderUI({
      hist <- active_series()
      if (!nrow(hist)) {
        return(shiny::div(class = "empty-state", shiny::p(
          "No monthly forecast series loaded. Run the Phase 12 pipeline or fixture build."
        )))
      }
      obs <- hist[value_observed == TRUE]
      last <- obs[.N]
      fc <- active_forecast()
      met <- forecast_snap()$metrics
      mid <- active_model()
      mode_label <- if (identical(input$model_mode, "manual")) "Manual model" else "Selected model"
      mrow <- if (!is.null(met) && nrow(met)) {
        mm <- data.table::as.data.table(met)
        if ("model_id" %in% names(mm)) {
          mm[, model_id := vapply(model_id, normalise_forecast_model_id, character(1))]
        }
        mm[series_id == selected_series_id() & model_id == mid & horizon == as.integer(input$horizon_view %||% 1)]
      } else {
        data.table::data.table()
      }
      mape_txt <- if (nrow(mrow) && is.finite(mrow$mape[1])) {
        format_forecast_pct(mrow$mape[1])
      } else if (identical(mid, "prophet") && identical(input$model_mode, "manual") && nrow(fc)) {
        "Unavailable — optional full Prophet rolling-origin backtests were not run for this series."
      } else {
        mape_unavailable_message()
      }
      mase_txt <- if (nrow(mrow) && is.finite(mrow$mase[1])) {
        format_forecast_metric(mrow$mase[1])
      } else if (identical(mid, "prophet") && identical(input$model_mode, "manual") && nrow(fc)) {
        "Unavailable — optional full Prophet backtests not run"
      } else {
        format_forecast_metric(NA)
      }
      smape_txt <- if (nrow(mrow) && is.finite(mrow$smape[1])) {
        format_forecast_pct(mrow$smape[1])
      } else {
        format_forecast_pct(NA)
      }
      prof <- forecast_snap()$profile %||% list()
      fixture_mode <- isTRUE(prof$is_fixture) || identical(prof$data_mode, FORECAST_DATA_MODE_FIXTURE)
      mape_label <- if (isTRUE(fixture_mode)) {
        "Fixture MAPE (not production accuracy)"
      } else {
        "Backtest MAPE"
      }
      next_fc <- if (nrow(fc) && is.finite(fc$predicted_value_usd[1])) fc$predicted_value_usd[1] else NA_real_
      total_fc <- safe_forecast_total(fc$predicted_value_usd)
      shiny::div(
        class = "forecast-kpi-strip",
        shiny::div(class = "kpi-card", shiny::tags$span("Latest observed"), shiny::tags$strong(format_forecast_usd(last$trade_value_usd))),
        shiny::div(class = "kpi-card", shiny::tags$span(mode_label), shiny::tags$strong(forecast_model_labels()[[mid]] %||% mid)),
        shiny::div(class = "kpi-card", shiny::tags$span("Next-month forecast"), shiny::tags$strong(format_forecast_usd(next_fc))),
        shiny::div(class = "kpi-card", shiny::tags$span("Forecast-period total"), shiny::tags$strong(format_forecast_usd(total_fc))),
        shiny::div(class = "kpi-card", shiny::tags$span(mape_label), shiny::tags$strong(mape_txt)),
        shiny::div(class = "kpi-card", shiny::tags$span("MASE"), shiny::tags$strong(mase_txt)),
        shiny::div(class = "kpi-card", shiny::tags$span("sMAPE"), shiny::tags$strong(smape_txt)),
        shiny::div(class = "kpi-card", shiny::tags$span("Last observation"), shiny::tags$strong(as.character(last$date)))
      )
    })

    output$history_plot <- plotly::renderPlotly({
      forecast_history_chart(active_series(), active_forecast(), show_imputed = isTRUE(input$show_imputed))
    })
    output$history_text <- shiny::renderUI({
      shiny::p(class = "muted", forecast_chart_text_summary(active_series(), active_forecast(), active_model()))
    })

    output$leaderboard <- DT::renderDT({
      met <- forecast_snap()$metrics
      if (is.null(met) || !nrow(met)) return(DT::datatable(data.table::data.table()))
      board <- met[series_id == selected_series_id() & horizon == as.integer(input$horizon_view %||% 1)]
      board <- forecast_leaderboard_order(board, selected_model_id = active_model())
      DT::datatable(board, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })

    output$residual_table <- DT::renderDT({
      rs <- forecast_snap()$residuals
      if (is.null(rs) || !nrow(rs)) return(DT::datatable(data.table::data.table()))
      dt <- data.table::as.data.table(rs)[series_id == selected_series_id()]
      mid <- active_model()
      if ("model_id" %in% names(dt)) {
        dt[, model_id := vapply(model_id, normalise_forecast_model_id, character(1))]
        dt <- dt[model_id == mid | is.na(model_id)]
      }
      if (!nrow(dt) && identical(input$model_mode, "manual")) {
        return(DT::datatable(
          data.frame(Note = paste(
            "Residual diagnostics are unavailable for manual model",
            forecast_model_labels()[[mid]] %||% mid,
            "— artefacts store selected-model residuals only."
          )),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }
      DT::datatable(dt, options = list(dom = "t"), rownames = FALSE)
    })

    output$backtest_table <- DT::renderDT({
      bt <- forecast_snap()$backtests
      if (is.null(bt) || !nrow(bt)) return(DT::datatable(data.table::data.table()))
      dt <- data.table::as.data.table(bt)
      if ("model_id" %in% names(dt)) {
        dt[, model_id := vapply(model_id, normalise_forecast_model_id, character(1))]
      }
      dt <- dt[series_id == selected_series_id() & model_id == active_model()]
      if (!nrow(dt) && identical(active_model(), "prophet") && nrow(active_forecast())) {
        return(DT::datatable(
          data.frame(Note = "Prophet forecast is shown; optional full rolling-origin backtests were not run for this series."),
          options = list(dom = "t"),
          rownames = FALSE
        ))
      }
      DT::datatable(dt, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })

    output$quality_panel <- shiny::renderUI({
      q <- forecast_snap()$quality
      if (is.null(q) || !nrow(q)) return(shiny::p(class = "muted", "No quality diagnostics."))
      row <- q[series_id == selected_series_id()]
      if (!nrow(row)) return(shiny::p(class = "muted", "Series not in quality table."))
      shiny::tags$ul(
        shiny::tags$li(paste("Expected months:", row$expected_months)),
        shiny::tags$li(paste("Observed months:", row$observed_months)),
        shiny::tags$li(paste("Missing months:", row$missing_months)),
        shiny::tags$li(paste("Zero months:", row$zero_months)),
        shiny::tags$li(paste("Completeness %:", format_forecast_pct(row$completeness_pct))),
        shiny::tags$li(paste("Longest missing run:", row$longest_missing_run)),
        shiny::tags$li(paste("Recent-12 availability:", row$recent_12_month_availability)),
        shiny::tags$li(paste("Stability:", row$stability_class)),
        shiny::tags$li(paste("Reason:", row$selection_or_rejection_reason)),
        shiny::tags$li(paste("Imputation count:", row$imputation_count))
      )
    })

    output$forecast_table <- DT::renderDT({
      DT::datatable(active_forecast(), options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
    })

    output$coverage_panel <- shiny::renderUI({
      cov <- forecast_snap()$detailed_coverage %||% list()
      prof <- forecast_snap()$profile %||% list()
      prop <- prophet_availability()
      shiny::tags$ul(
        shiny::tags$li(paste("Data mode:", prof$data_mode %||% "unknown")),
        shiny::tags$li(paste("Data source:", prof$data_source %||% "unknown")),
        shiny::tags$li(paste(
          "Production forecast available:",
          isTRUE(prof$production_forecast_available)
        )),
        shiny::tags$li(paste(
          "Live monthly successful requests:",
          prof$live_monthly_successful_requests %||% 0L
        )),
        shiny::tags$li(paste("Universe:", cov$universe_checksum %||% prof$universe_version %||% "n/a")),
        shiny::tags$li(paste("Annual production status:", cov$production_status %||% "unknown")),
        shiny::tags$li(paste("Candidates:", prof$candidate_count %||% NA)),
        shiny::tags$li(paste("Stable selected:", prof$stable_selected_count %||% NA)),
        shiny::tags$li(paste("Prophet available:", isTRUE(prop$available))),
        if (!isTRUE(prop$available)) shiny::tags$li(paste("Prophet reason:", prop$reason %||% "unavailable")),
        if (isTRUE(prof$is_fixture)) {
          shiny::tags$li(prof$fixture_accuracy_disclaimer %||% forecast_fixture_notice())
        },
        shiny::tags$li("No unverified MAPE threshold claim is displayed."),
        shiny::tags$li("Performance latency claims belong to Phase 13.")
      )
    })

    meta <- shiny::reactive({
      c(
        list(
          engine_version = FORECAST_ENGINE_VERSION,
          series_id = selected_series_id() %||% NA_character_,
          model_id = active_model(),
          units = "current_usd"
        ),
        forecast_download_provenance_meta(forecast_snap()$profile)
      )
    })

    make_dl <- function(getter) {
      shiny::downloadHandler(
        filename = function() forecast_download_filename("forecast_export"),
        content = function(file) {
          data.table::fwrite(forecast_download_table(getter(), meta()), file, bom = TRUE)
        }
      )
    }
    output$dl_series <- make_dl(active_series)
    output$dl_fc <- make_dl(active_forecast)
    output$dl_bt <- make_dl(function() {
      bt <- forecast_snap()$backtests
      if (is.null(bt)) data.table::data.table() else bt[series_id == selected_series_id()]
    })
    output$dl_met <- make_dl(function() {
      m <- forecast_snap()$metrics
      if (is.null(m)) data.table::data.table() else m[series_id == selected_series_id()]
    })
    output$dl_res <- make_dl(function() {
      r <- forecast_snap()$residuals
      if (is.null(r)) data.table::data.table() else r[series_id == selected_series_id()]
    })
    output$dl_qual <- make_dl(function() {
      q <- forecast_snap()$quality
      if (is.null(q)) data.table::data.table() else q[series_id == selected_series_id()]
    })
    output$dl_board <- make_dl(function() {
      m <- forecast_snap()$metrics
      if (is.null(m)) data.table::data.table() else m[series_id == selected_series_id()]
    })
    output$dl_meta <- shiny::downloadHandler(
      filename = function() forecast_download_filename("forecast_metadata", ext = "json"),
      content = function(file) {
        jsonlite::write_json(forecast_snap()$profile %||% meta(), file, auto_unbox = TRUE, pretty = TRUE)
      }
    )
  })
}
