mod_trade_flows_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "trade-flows-page",
      shiny::div(
        class = "hero-panel trade-flows-hero",
        shiny::h2("Trade Flows Explorer"),
        shiny::p(
          "Bilateral HS4 flows among the analytical top-reporter universe. ",
          "Values are current US dollars from the reporting economy’s perspective."
        )
      ),
      shiny::uiOutput(ns("partial_notice")),
      shiny::uiOutput(ns("filter_bar")),
      shiny::uiOutput(ns("kpi_strip")),
      shiny::div(
        class = "chart-card sankey-card",
        shiny::h3(class = "chart-title", "Reporter–partner–commodity Sankey"),
        shiny::uiOutput(ns("sankey_coverage_note")),
        networkD3::sankeyNetworkOutput(ns("sankey"), height = "480px"),
        shiny::uiOutput(ns("sankey_summary"))
      ),
      shiny::div(
        class = "trade-flow-grid",
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Bilateral time series"),
          plotly::plotlyOutput(ns("timeseries"), height = "320px"),
          shiny::uiOutput(ns("timeseries_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Commodity composition"),
          plotly::plotlyOutput(ns("composition"), height = "320px"),
          shiny::uiOutput(ns("composition_summary"))
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Reporter–partner matrix"),
          shiny::selectInput(
            ns("matrix_measure"), "Matrix measure",
            choices = c(
              "Trade value" = "trade_value",
              "Imports" = "imports",
              "Exports" = "exports",
              "Observation count" = "observation_count"
            ),
            selected = "trade_value",
            width = "240px"
          ),
          plotly::plotlyOutput(ns("matrix"), height = "380px"),
          shiny::uiOutput(ns("matrix_summary"))
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Detailed observations"),
          shiny::checkboxInput(ns("table_technical"), "Show technical columns", FALSE),
          DT::DTOutput(ns("detail_table")),
          shiny::tags$caption(
            class = "sr-only",
            "Searchable table of filtered bilateral trade observations."
          )
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Coverage and methodology"),
          shiny::uiOutput(ns("coverage_panel"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Downloads"),
          shiny::p(class = "muted", "Exports respect active filters. No secrets or filesystem paths."),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(ns("dl_obs"), "Observations CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_sankey"), "Sankey links CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_ts"), "Time series CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_comp"), "Composition CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_matrix"), "Matrix long CSV", class = "btn-sm btn-outline-primary")
          )
        )
      ),
      shiny::uiOutput(ns("empty_fallback"))
    )
  )
}

mod_trade_flows_server <- function(id, snap, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    detailed <- shiny::reactive({
      s <- snap()
      dt <- s$trade_detailed_enriched
      if (is.null(dt) || !nrow(dt)) return(NULL)
      dt
    })

    coverage <- shiny::reactive({
      s <- snap()
      if (!is.null(s$detailed_coverage) && length(s$detailed_coverage)) {
        return(s$detailed_coverage)
      }
      trade_flow_coverage_status(s)
    })

    choices <- shiny::reactive({
      dt <- detailed()
      shiny::req(dt)
      trade_flow_filter_choices(dt, snap()$analytical_universe)
    })

    output$empty_fallback <- shiny::renderUI({
      if (!is.null(detailed())) return(NULL)
      shiny::div(
        class = "empty-state",
        role = "status",
        shiny::h3("Detailed bilateral data unavailable"),
        shiny::p("Processed detailed trade files were not found or could not be read."),
        shiny::p(class = "muted", "This page does not call external APIs. Resume Phase 2 detailed ingestion when quota allows, then reload.")
      )
    })

    output$partial_notice <- shiny::renderUI({
      c <- coverage()
      shiny::div(
        class = "partial-data-notice trade-flow-notice",
        role = "status",
        shiny::tags$strong(detailed_coverage_notice(c, context = "bilateral"))
      )
    })

    output$filter_bar <- shiny::renderUI({
      ch <- choices()
      shiny::req(ch)
      years <- ch$years
      def_y <- ch$default_year
      shiny::div(
        class = "filter-toolbar trade-flow-filters",
        role = "search",
        `aria-label` = "Trade flow filters",
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year_mode"), "Year mode"),
          shiny::selectInput(
            ns("year_mode"), NULL,
            choices = c("Single year" = "single", "Full range" = "full", "Custom range" = "range"),
            selected = "single"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year"), "Year"),
          shiny::selectInput(
            ns("year"), NULL,
            choices = setNames(as.character(years), as.character(years)),
            selected = as.character(def_y)
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year_min"), "From"),
          shiny::selectInput(ns("year_min"), NULL, choices = years, selected = min(years))
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year_max"), "To"),
          shiny::selectInput(ns("year_max"), NULL, choices = years, selected = max(years))
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("reporter"), "Reporter"),
          shiny::selectizeInput(
            ns("reporter"), NULL,
            choices = c("All represented reporters" = "__ALL__", ch$reporter_labels),
            selected = "__ALL__"
          )
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("partner"), "Partner"),
          shiny::selectizeInput(
            ns("partner"), NULL,
            choices = c("All partners in data" = "__ALL__", ch$partner_labels),
            selected = "__ALL__"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("flow"), "Trade flow"),
          shiny::selectInput(
            ns("flow"), NULL,
            choices = c("Both" = "both", "Imports" = "imports", "Exports" = "exports"),
            selected = "both"
          )
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("hs4"), "HS4 commodity"),
          shiny::selectizeInput(
            ns("hs4"), NULL,
            choices = c("All HS4 in data" = "__ALL__", ch$hs_labels),
            selected = "__ALL__"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("top_n"), "Sankey top-N paths"),
          shiny::selectInput(
            ns("top_n"), NULL,
            choices = c(10L, 20L, 30L, 50L),
            selected = 20L
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("grouping"), "Sankey grouping"),
          shiny::selectInput(
            ns("grouping"), NULL,
            choices = c(
              "Reporter → Partner → Commodity" = "reporter_partner_commodity",
              "Reporter → Commodity → Partner" = "reporter_commodity_partner",
              "Reporter → Partner" = "reporter_partner",
              "Reporter → Commodity" = "reporter_commodity"
            ),
            selected = "reporter_partner_commodity"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("scale"), "Display scale"),
          shiny::selectInput(
            ns("scale"), NULL,
            choices = c("Automatic" = "auto", "Millions" = "millions", "Billions" = "billions"),
            selected = "auto"
          )
        ),
        shiny::uiOutput(ns("reporter_year_hint"))
      )
    })

    year_bounds <- shiny::reactive({
      ch <- choices()
      shiny::req(ch)
      mode <- input$year_mode %||% "single"
      if (identical(mode, "full")) {
        return(list(min = min(ch$years), max = max(ch$years)))
      }
      if (identical(mode, "range")) {
        ymin <- as.integer(input$year_min %||% min(ch$years))
        ymax <- as.integer(input$year_max %||% max(ch$years))
        if (ymin > ymax) {
          tmp <- ymin; ymin <- ymax; ymax <- tmp
        }
        return(list(min = ymin, max = ymax))
      }
      y <- as.integer(input$year %||% ch$default_year)
      list(min = y, max = y)
    })

    output$reporter_year_hint <- shiny::renderUI({
      dt <- detailed()
      shiny::req(dt)
      r <- input$reporter
      if (is.null(r) || identical(r, "__ALL__")) {
        return(shiny::span(class = "context-pill", paste0("Available years: ", paste(choices()$years, collapse = ", "))))
      }
      ys <- sort(unique(dt[reporter_iso3 == r]$year))
      shiny::span(
        class = "context-pill",
        paste0("Years for ", r, ": ", if (length(ys)) paste(ys, collapse = ", ") else "none")
      )
    })

    filtered <- shiny::reactive({
      dt <- detailed()
      shiny::req(dt)
      yb <- year_bounds()
      filter_detailed_trade(
        dt,
        year_min = yb$min,
        year_max = yb$max,
        reporters = input$reporter %||% "__ALL__",
        partners = input$partner %||% "__ALL__",
        flows = normalize_flow_codes(input$flow %||% "both"),
        hs_codes = input$hs4 %||% "__ALL__"
      )
    })

    path_sel <- shiny::reactive({
      paths <- trade_flow_path_aggregates(filtered(), grouping = input$grouping %||% "reporter_partner_commodity")
      select_top_n_paths(paths, top_n = as.integer(input$top_n %||% 20L))
    })

    sankey_obj <- shiny::reactive({
      sel <- path_sel()
      both <- length(normalize_flow_codes(input$flow %||% "both")) > 1L
      build_sankey_data(
        sel$visible,
        grouping = input$grouping %||% "reporter_partner_commodity",
        include_other = FALSE,
        other_value = sel$other_value,
        both_flows = both
      )
    })

    kpis <- shiny::reactive(trade_flow_kpis(filtered(), path_sel()))

    ts_prep <- shiny::reactive(prepare_trade_flow_timeseries(filtered(), max_series = 6L))
    comp_prep <- shiny::reactive(prepare_commodity_composition(filtered(), top_n = 12L))
    mat_prep <- shiny::reactive({
      prepare_reporter_partner_matrix(filtered(), measure = input$matrix_measure %||% "trade_value")
    })

    output$kpi_strip <- shiny::renderUI({
      if (is.null(detailed())) return(NULL)
      k <- kpis()
      shiny::div(
        class = "metric-grid overview-kpi-grid trade-flow-kpis",
        role = "group",
        `aria-label` = "Detailed trade KPIs",
        overview_metric_card("Filtered trade value", format_usd_compact(k$filtered_trade_value),
                             "Current US$ · detailed only", k$scope_note),
        overview_metric_card("Observations", format_count(k$n_observations), "Rows", NULL),
        overview_metric_card("Reporters", format_count(k$n_reporters), "Represented in filter", NULL),
        overview_metric_card("Partners", format_count(k$n_partners), "Country partners", NULL),
        overview_metric_card("HS4 commodities", format_count(k$n_hs4), "Headings", NULL),
        overview_metric_card("Sankey coverage", format_pct(k$sankey_coverage_pct),
                             "Share of filtered value", "Visible top-N paths only")
      )
    })

    output$sankey_coverage_note <- shiny::renderUI({
      sel <- path_sel()
      shiny::p(
        class = "chart-summary",
        paste0(
          "Showing ", format_count(sel$n_visible), " of ", format_count(sel$n_total),
          " aggregated paths (", format_pct(sel$coverage_pct),
          " of filtered detailed trade value: ",
          format_usd_compact(sel$total_value - sel$other_value), " of ",
          format_usd_compact(sel$total_value),
          "). Remainder is excluded from the diagram, not labelled as a complete total."
        )
      )
    })

    output$sankey <- networkD3::renderSankeyNetwork({
      sk <- sankey_obj()
      if (is.null(sk) || !nrow(sk$nodes) || !nrow(sk$links)) {
        return(NULL)
      }
      render_trade_flow_sankey(sk)
    })

    output$sankey_summary <- shiny::renderUI({
      shiny::p(class = "chart-summary", sankey_text_summary(sankey_obj(), path_sel()))
    })

    output$timeseries <- plotly::renderPlotly({
      trade_flow_timeseries_plotly(
        ts_prep(),
        title = "Detailed bilateral trade over time",
        scale = input$scale %||% "auto"
      )
    })

    output$timeseries_summary <- shiny::renderUI({
      shiny::p(class = "chart-summary", ts_prep()$note)
    })

    output$composition <- plotly::renderPlotly({
      trade_flow_composition_plotly(
        comp_prep(),
        title = "HS4 composition (detailed filters)",
        scale = input$scale %||% "auto"
      )
    })

    output$composition_summary <- shiny::renderUI({
      c <- comp_prep()
      shiny::p(
        class = "chart-summary",
        paste0(
          "Composition of filtered detailed trade (", format_usd_compact(c$total),
          "). Top headings shown; OTHER aggregates remainder when used. ",
          "Not a complete global HS-85 market composition."
        )
      )
    })

    output$matrix <- plotly::renderPlotly({
      trade_flow_matrix_plotly(
        mat_prep(),
        title = "Reporter–partner matrix (unobserved cells remain blank)",
        scale = input$scale %||% "auto"
      )
    })

    output$matrix_summary <- shiny::renderUI({
      m <- mat_prep()
      shiny::p(
        class = "chart-summary",
        paste0(
          "Matrix uses represented reporters and partners in the filtered detailed data. ",
          "Unobserved combinations are not filled with zero. Observed total: ",
          format_usd_compact(m$observed_total), "."
        )
      )
    })

    output$detail_table <- DT::renderDT({
      dt <- trade_flow_table_display(filtered(), technical = isTRUE(input$table_technical))
      if (!nrow(dt)) {
        return(DT::datatable(
          data.frame(Message = "No observations match these filters. Try expanding the year range or selecting All Partners."),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        ))
      }

      max_rows <- 5000L
      if (nrow(dt) > max_rows) dt <- dt[seq_len(max_rows)]
      DT::datatable(
        dt,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          deferRender = TRUE
        ),
        caption = "Filtered detailed bilateral observations (current US$)."
      ) |>
        DT::formatCurrency("trade_value_usd", currency = "$", digits = 0)
    }, server = TRUE)

    output$coverage_panel <- shiny::renderUI({
      c <- coverage()
      chk <- c$universe_checksum %||% "—"
      stale <- if (isTRUE(c$checksum_stale)) {
        shiny::span(class = "warn-text", " Checksum differs from expected project universe.")
      } else NULL
      shiny::tagList(
        shiny::div(
          class = "status-badge-row",
          status_badge("Production", c$production_status),
          status_badge(
            "Coverage",
            if (identical(c$production_status, "complete")) "complete" else "partial",
            label_override = sprintf("%d/%d", c$represented_reporter_count, c$selected_reporter_count)
          )
        ),
        shiny::tags$ul(
          class = "coverage-list",
          shiny::tags$li(paste0("Selected universe reporters: ", format_count(c$selected_reporter_count))),
          shiny::tags$li(paste0("Represented: ", paste(c$represented_reporters, collapse = ", "))),
          shiny::tags$li(paste0("Missing: ", if (length(c$missing_reporters)) paste(c$missing_reporters, collapse = ", ") else "none")),
          shiny::tags$li(paste0("Detailed years: ", paste(c$year_range, collapse = "–"))),
          shiny::tags$li(paste0("Latest detailed ingestion: ", c$latest_ingested_at %||% "Unavailable")),
          shiny::tags$li(paste0("Validation warnings: ", format_count(c$validation_warnings))),
          shiny::tags$li(paste0(
            "Request summary — planned: ", c$request_summary$planned,
            ", active: ", c$request_summary$active,
            ", succeeded: ", c$request_summary$succeeded,
            ", quota blocked: ", c$request_summary$quota_blocked
          )),
          shiny::tags$li(list("Universe checksum: ", chk, stale))
        ),
        shiny::p(
          class = "method-note",
          "Results use available detailed observations. The completed global country-level dataset powers the Executive Overview. ",
          "Detailed coverage expands after the API quota permits remaining fetches. ",
          shiny::tags$a(
            href = "#", class = "dq-link",
            onclick = "document.querySelector('[data-value=\"Data Quality\"]')?.click(); return false;",
            "Open Data Quality"
          ), "."
        ),
        shiny::p(
          class = "method-note",
          "Flow semantics: the explorer keeps the reporting-economy perspective. Import rows are reporter imports from partners; ",
          "they are not treated as partner exports unless mirror validation is introduced in a later phase."
        )
      )
    })

    fname_ctx <- shiny::reactive({
      yb <- year_bounds()
      yl <- if (identical(yb$min, yb$max)) as.character(yb$min) else paste0(yb$min, "-", yb$max)
      list(
        year = yl,
        reporter = if (identical(input$reporter, "__ALL__")) "all_reporters" else input$reporter,
        partner = if (identical(input$partner, "__ALL__")) "all_partners" else input$partner,
        flow = input$flow %||% "both"
      )
    })

    output$dl_obs <- shiny::downloadHandler(
      filename = function() {
        ctx <- fname_ctx()
        trade_flow_filename("trade_flows", ctx$year, ctx$reporter, ctx$partner, ctx$flow)
      },
      content = function(file) write_trade_flow_csv(trade_flow_table_display(filtered(), TRUE), file)
    )
    output$dl_sankey <- shiny::downloadHandler(
      filename = function() {
        ctx <- fname_ctx()
        trade_flow_filename("trade_flows_sankey", ctx$year, ctx$reporter, ctx$partner, ctx$flow)
      },
      content = function(file) write_trade_flow_csv(sankey_obj()$edge_table, file)
    )
    output$dl_ts <- shiny::downloadHandler(
      filename = function() {
        ctx <- fname_ctx()
        trade_flow_filename("trade_flows_timeseries", ctx$year, ctx$reporter, ctx$partner, ctx$flow)
      },
      content = function(file) write_trade_flow_csv(ts_prep()$data, file)
    )
    output$dl_comp <- shiny::downloadHandler(
      filename = function() {
        ctx <- fname_ctx()
        trade_flow_filename("trade_flows_composition", ctx$year, ctx$reporter, ctx$partner, ctx$flow)
      },
      content = function(file) write_trade_flow_csv(comp_prep()$data, file)
    )
    output$dl_matrix <- shiny::downloadHandler(
      filename = function() {
        ctx <- fname_ctx()
        trade_flow_filename("trade_flows_matrix", ctx$year, ctx$reporter, ctx$partner, ctx$flow)
      },
      content = function(file) write_trade_flow_csv(mat_prep()$long, file)
    )
  })
}
