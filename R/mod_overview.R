mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "overview-page",
      shiny::uiOutput(ns("partial_notice")),
      shiny::uiOutput(ns("filter_bar")),
      shiny::uiOutput(ns("kpi_cards")),
      shiny::div(
        class = "overview-grid",
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Global trade trend"),
          plotly::plotlyOutput(ns("trend_chart"), height = "320px"),
          shiny::uiOutput(ns("trend_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Leading reporting economies"),
          shiny::div(
            class = "chart-controls",
            shiny::selectInput(
              ns("rank_measure"), "Measure",
              choices = c(
                "Total trade" = "total",
                "Imports" = "imports",
                "Exports" = "exports",
                "Trade balance" = "balance"
              ),
              selected = "total",
              width = "100%"
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'balance'", ns("rank_measure")),
              shiny::radioButtons(
                ns("balance_rank_mode"), NULL,
                choices = c(
                  "Highest balances (surplus first)" = "highest",
                  "Lowest balances (deficit first)" = "lowest"
                ),
                selected = "highest",
                inline = TRUE
              )
            )
          ),
          plotly::plotlyOutput(ns("ranking_chart"), height = "360px"),
          shiny::uiOutput(ns("ranking_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Import / export composition"),
          plotly::plotlyOutput(ns("composition_chart"), height = "220px"),
          shiny::uiOutput(ns("composition_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Trade-balance analysis"),
          shiny::uiOutput(ns("balance_stats")),
          plotly::plotlyOutput(ns("balance_chart"), height = "300px"),
          shiny::p(
            class = "method-note",
            title = "Trade balance = exports minus imports in current US dollars for HS Chapter 85. A surplus or deficit is descriptive, not a normative judgement.",
            "Methodology: balance = exports − imports (current US$). Surplus/deficit wording is descriptive only."
          )
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Macroeconomic context"),
          shiny::uiOutput(ns("macro_panel")),
          plotly::plotlyOutput(ns("macro_chart"), height = "340px"),
          shiny::p(
            class = "method-note",
            "Scatter relates GDP to HS-85 trade for the selected year. Bubble size reflects population where available. Correlation is not causation. Values are current US dollars; CPI is not used to deflate trade."
          )
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Coverage and freshness"),
          shiny::uiOutput(ns("coverage_panel"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Downloads"),
          shiny::p(class = "muted", "Exports respect active filters. No secrets or filesystem paths are included."),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(ns("dl_analytics"), "Country-year CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_ranking"), "Ranking CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_kpi"), "KPI summary CSV", class = "btn-sm btn-outline-primary")
          )
        )
      ),
      shiny::uiOutput(ns("empty_fallback"))
    )
  )
}

mod_overview_server <- function(id, snap, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cy_data <- shiny::reactive({
      s <- snap()
      cy <- s$country_year_analytics
      if (is.null(cy) || !nrow(cy)) return(NULL)
      overview_exclude_aggregates(cy)
    })

    output$empty_fallback <- shiny::renderUI({
      if (!is.null(cy_data())) return(NULL)
      shiny::div(
        class = "empty-state",
        role = "status",
        shiny::h3("Executive Overview data unavailable"),
        shiny::p("Processed country-year analytics were not found or could not be read."),
        shiny::p(class = "muted", "Generate Phase 2–3 outputs locally, then reload the application. No live API calls are made from this page.")
      )
    })

    output$partial_notice <- shiny::renderUI({
      s <- snap()
      c <- if (!is.null(s$detailed_coverage) && length(s$detailed_coverage)) {
        s$detailed_coverage
      } else {
        trade_flow_coverage_status(s)
      }
      shiny::div(
        class = "partial-data-notice",
        role = "note",
        shiny::tags$strong(detailed_coverage_notice(c, context = "overview"))
      )
    })

    output$filter_bar <- shiny::renderUI({
      cy <- cy_data()
      if (is.null(cy)) return(NULL)
      years <- sort(unique(as.integer(cy$year)))
      def <- choose_overview_default_year(cy)
      if (is.na(def)) def <- max(years)
      shiny::div(
        class = "filter-toolbar",
        role = "search",
        `aria-label` = "Executive Overview filters",
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year"), "Year"),
          shiny::selectInput(
            ns("year"), NULL,
            choices = c("Full period (trend only)" = "all", setNames(as.character(years), as.character(years))),
            selected = as.character(def),
            width = "100%"
          )
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("reporter"), "Reporter / economy"),
          shiny::selectizeInput(
            ns("reporter"), NULL,
            choices = overview_reporter_choices(cy),
            selected = "__GLOBAL__",
            options = list(placeholder = "Search economies…")
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("flow_scope"), "Trade-flow scope"),
          shiny::selectInput(
            ns("flow_scope"), NULL,
            choices = c(
              "Total trade" = "total",
              "Imports" = "imports",
              "Exports" = "exports",
              "Trade balance" = "balance"
            ),
            selected = "total",
            width = "100%"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("compare_mode"), "Comparison"),
          shiny::selectInput(
            ns("compare_mode"), NULL,
            choices = c(
              "Year-over-year where available" = "yoy",
              "Selected year only" = "none"
            ),
            selected = "yoy",
            width = "100%"
          )
        ),
        shiny::div(
          class = "filter-context",
          shiny::uiOutput(ns("filter_context_text"))
        )
      )
    })

    selected_year <- shiny::reactive({
      y <- input$year
      if (is.null(y) || identical(y, "all")) return(NA_integer_)
      as.integer(y)
    })

    selected_reporter <- shiny::reactive({
      r <- input$reporter
      if (is.null(r) || !nzchar(r)) return("__GLOBAL__")
      as.character(r)
    })

    is_global <- shiny::reactive(identical(selected_reporter(), "__GLOBAL__"))

    analysis_year <- shiny::reactive({
      cy <- cy_data()
      shiny::req(cy)
      y <- selected_year()
      if (is.na(y)) {
        def <- choose_overview_default_year(cy)
        if (is.na(def)) def <- max(as.integer(cy$year), na.rm = TRUE)
        return(def)
      }
      y
    })

    output$filter_context_text <- shiny::renderUI({
      cy <- cy_data()
      shiny::req(cy)
      y <- analysis_year()
      r <- selected_reporter()
      scope <- if (is_global()) "Global overview" else {
        nm <- cy[reporter_iso3 == r, reporter_name][1]
        paste0(nm %||% r, " (", r, ")")
      }
      period <- if (identical(input$year, "all")) {
        paste0("Trend period ", min(cy$year), "–", max(cy$year), " (KPIs use ", y, ")")
      } else {
        paste("Selected year", y)
      }
      shiny::tagList(
        shiny::span(class = "context-pill", scope),
        shiny::span(class = "context-pill", period),
        shiny::span(class = "context-pill", "Current US$")
      )
    })

    kpi <- shiny::reactive({
      cy <- cy_data()
      shiny::req(cy)
      y <- analysis_year()
      if (is_global()) overview_kpi_global(cy, y) else overview_kpi_reporter(cy, y, selected_reporter())
    })

    ranked <- shiny::reactive({
      cy <- cy_data()
      shiny::req(cy)
      measure <- input$rank_measure %||% "total"
      lowest <- identical(measure, "balance") && identical(input$balance_rank_mode, "lowest")
      overview_top_economies(cy, analysis_year(), measure = measure, top_n = 10L, lowest_balance = lowest)
    })

    composition <- shiny::reactive({
      cy <- cy_data()
      shiny::req(cy)
      overview_composition(cy, analysis_year(), selected_reporter())
    })

    coverage <- shiny::reactive(overview_coverage_status(snap()))

    output$kpi_cards <- shiny::renderUI({
      cy <- cy_data()
      if (is.null(cy)) return(NULL)
      k <- kpi()
      show_yoy <- identical(input$compare_mode, "yoy")
      if (identical(k$mode, "global")) {
        yoy_sub <- if (show_yoy) format_yoy_delta(k$yoy_total_pct) else "Comparison off"
        shiny::div(
          class = "metric-grid overview-kpi-grid",
          role = "group",
          `aria-label` = "Key performance indicators",
          overview_metric_card("Total HS-85 trade", format_usd_compact(k$total_trade),
                               "Current US$", paste("Sum across reporting economies,", k$year)),
          overview_metric_card("Imports", format_usd_compact(k$imports),
                               "Current US$", "HS Chapter 85 imports"),
          overview_metric_card("Exports", format_usd_compact(k$exports),
                               "Current US$", "HS Chapter 85 exports"),
          overview_metric_card("Trade balance", format_usd_compact(k$trade_balance),
                               "Current US$", "Exports minus imports (neutral interpretation)"),
          overview_metric_card("Reporting economies", format_count(k$n_reporters),
                               "Count", "Valid ISO3 reporters only"),
          overview_metric_card("YoY total trade", if (show_yoy) format_pct(k$yoy_total_pct) else "—",
                               "Change", yoy_sub, tone = "neutral")
        )
      } else {
        shiny::div(
          class = "metric-grid overview-kpi-grid",
          role = "group",
          `aria-label` = "Reporter key performance indicators",
          overview_metric_card("Reporter total trade", format_usd_compact(k$total_trade),
                               "Current US$", k$reporter_name %||% k$reporter_iso3),
          overview_metric_card("Imports", format_usd_compact(k$imports), "Current US$", NULL),
          overview_metric_card("Exports", format_usd_compact(k$exports), "Current US$", NULL),
          overview_metric_card("Trade balance", format_usd_compact(k$trade_balance),
                               "Current US$", "Exports minus imports"),
          overview_metric_card("Trade / GDP", format_pct(k$trade_pct_gdp),
                               "Share", "Requires valid GDP; otherwise Unavailable"),
          overview_metric_card("Trade per capita", format_per_person(k$trade_per_capita),
                               "Current US$", "Requires positive population")
        )
      }
    })

    output$trend_chart <- plotly::renderPlotly({
      cy <- cy_data()
      shiny::req(cy)
      series <- overview_trend_series(cy, selected_reporter())
      scope <- if (is_global()) "Global" else selected_reporter()
      show_bal <- identical(input$flow_scope, "balance")
      overview_trend_plotly(
        series,
        title = paste0("HS-85 trade trend — ", scope),
        show_balance = show_bal
      )
    })

    output$trend_summary <- shiny::renderUI({
      cy <- cy_data()
      shiny::req(cy)
      series <- overview_trend_series(cy, selected_reporter())
      scope <- if (is_global()) "global reporting economies" else selected_reporter()
      shiny::p(class = "chart-summary", trend_text_summary(series, scope))
    })

    output$ranking_chart <- plotly::renderPlotly({
      r <- ranked()
      measure <- input$rank_measure %||% "total"
      overview_ranking_plotly(
        r,
        title = paste0("Top economies — ", analysis_year(), " (", measure, ")"),
        measure = measure
      )
    })

    output$ranking_summary <- shiny::renderUI({
      r <- ranked()
      if (is.null(r) || !nrow(r)) {
        return(shiny::p(class = "chart-summary", "No ranking available for the selected year."))
      }
      shiny::p(
        class = "chart-summary",
        paste0(
          "Top ", nrow(r), " reporting economies for ", analysis_year(),
          ". Ranking uses signed values for trade balance (not absolute values). ",
          "Aggregates such as EUR/WLD are excluded. This is not a supply-chain dependency ranking."
        )
      )
    })

    output$composition_chart <- plotly::renderPlotly({
      overview_composition_plotly(
        composition(),
        title = paste0("Import vs export composition — ", analysis_year())
      )
    })

    output$composition_summary <- shiny::renderUI({
      c <- composition()
      shiny::p(
        class = "chart-summary",
        paste0(
          "Imports ", format_usd_compact(c$imports), " (", format_pct(c$imports_share_pct),
          "); exports ", format_usd_compact(c$exports), " (", format_pct(c$exports_share_pct),
          "); total ", format_usd_compact(c$total), ". Current US$."
        )
      )
    })

    output$balance_stats <- shiny::renderUI({
      cy <- cy_data()
      shiny::req(cy)
      y <- analysis_year()
      if (is_global()) {
        d <- overview_balance_distribution(cy, y)
        shiny::div(
          class = "stat-strip",
          shiny::span(paste0("Economies: ", format_count(d$n))),
          shiny::span(class = "badge-surplus", paste0("Surplus: ", format_count(d$n_surplus))),
          shiny::span(class = "badge-deficit", paste0("Deficit: ", format_count(d$n_deficit))),
          shiny::span(paste0("Median balance: ", format_usd_compact(d$median_balance)))
        )
      } else {
        k <- overview_kpi_reporter(cy, y, selected_reporter())
        shiny::div(
          class = "stat-strip",
          shiny::span(paste0("Latest balance: ", format_usd_compact(k$trade_balance))),
          shiny::span(paste0("Change vs prior year: ", format_usd_compact(k$balance_change))),
          shiny::span(paste0("Balance / GDP: ", format_pct(k$balance_pct_gdp)))
        )
      }
    })

    output$balance_chart <- plotly::renderPlotly({
      cy <- cy_data()
      shiny::req(cy)
      y <- analysis_year()
      if (is_global()) {
        d <- overview_balance_distribution(cy, y)
        overview_balance_bars_plotly(d, paste0("Surplus and deficit leaders — ", y))
      } else {
        series <- overview_trend_series(cy, selected_reporter())
        p <- gte_palette()
        if (!nrow(series)) return(overview_empty_plotly())
        plotly::plot_ly(
          as.data.frame(series), x = ~year, y = ~balance,
          type = "scatter", mode = "lines+markers",
          line = list(color = p$accent, width = 2),
          hovertemplate = "Year %{x}<br>Balance: %{customdata}<extra></extra>",
          customdata = format_usd_compact(series$balance),
          name = "Trade balance"
        ) |>
          plotly::layout(
            title = list(text = paste0("Trade balance over time — ", selected_reporter()),
                         font = list(size = 15, color = p$ink)),
            yaxis = list(title = "Balance (current US$)", zeroline = TRUE, zerolinecolor = p$zero, gridcolor = p$grid),
            xaxis = list(title = "Year", gridcolor = p$grid),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            showlegend = FALSE
          ) |>
          plotly::config(displayModeBar = FALSE, responsive = TRUE)
      }
    })

    output$macro_panel <- shiny::renderUI({
      cy <- cy_data()
      shiny::req(cy)
      if (is_global()) {
        prep <- overview_macro_scatter(cy, analysis_year(), use_log = TRUE)
        note <- if (prep$excluded_log > 0) {
          paste0(prep$excluded_log, " economies excluded from log axes (missing, zero, or negative GDP/trade).")
        } else {
          "All plotted points have positive GDP and total trade."
        }
        shiny::p(class = "chart-summary", note)
      } else {
        k <- overview_kpi_reporter(cy, analysis_year(), selected_reporter())
        shiny::div(
          class = "macro-reporter-grid",
          overview_metric_card("GDP", format_usd_compact(k$gdp), "Current US$", NULL),
          overview_metric_card("Population", format_count(k$population), "Persons", NULL),
          overview_metric_card("GDP per capita", format_usd_compact(k$gdp_per_capita), "Current US$", NULL),
          overview_metric_card("Inflation (annual)", format_pct(k$inflation), "Percent", "WDI CPI inflation; not used to deflate trade"),
          overview_metric_card("HS-85 trade / GDP", format_pct(k$trade_pct_gdp), "Share", NULL),
          overview_metric_card("HS-85 trade per capita", format_per_person(k$trade_per_capita), "Current US$", NULL)
        )
      }
    })

    output$macro_chart <- plotly::renderPlotly({
      cy <- cy_data()
      shiny::req(cy)
      if (!is_global()) {
        return(overview_empty_plotly("Reporter macro metrics are shown above; scatter is global-mode only."))
      }
      prep <- overview_macro_scatter(cy, analysis_year(), use_log = TRUE)
      overview_macro_scatter_plotly(
        prep,
        title = paste0("GDP vs HS-85 total trade — ", analysis_year())
      )
    })

    output$coverage_panel <- shiny::renderUI({
      c <- coverage()
      chk <- c$universe_checksum
      chk_short <- if (!is.na(chk) && nchar(chk) > 18) paste0(substr(chk, 1, 18), "…") else as.character(chk %||% "—")
      dq_link <- shiny::tags$a(
        href = "#",
        class = "dq-link",
        onclick = "document.querySelector('[data-value=\"Data Quality\"]')?.click(); return false;",
        "Open Data Quality"
      )
      shiny::tagList(
        shiny::div(
          class = "status-badge-row",
          status_badge("Global trade", c$global_trade_status),
          status_badge("Macro pipeline", c$macro_status),
          status_badge("Detailed trade", c$detailed_status),
          status_badge(
            "Detailed coverage",
            if (identical(c$detailed_status, "complete")) "complete" else "partial",
            label_override = c$detailed_coverage_label
          )
        ),
        shiny::tags$ul(
          class = "coverage-list",
          shiny::tags$li(paste0("Trade year range: ", paste(c$trade_year_range, collapse = "–"))),
          shiny::tags$li(paste0("Macro year range: ", paste(c$macro_year_range, collapse = "–"))),
          shiny::tags$li(paste0("Latest trade ingestion: ", c$trade_ingested_at %||% "Unavailable")),
          shiny::tags$li(paste0("Latest WDI ingestion: ", c$wdi_ingested_at %||% "Unavailable")),
          shiny::tags$li(paste0("Validation warnings: ", format_count(c$validation_warnings))),
          shiny::tags$li(
            shiny::tags$details(
              shiny::tags$summary("Technical details"),
              shiny::p(paste0("Universe checksum: ", chk_short))
            )
          )
        ),
        if ((c$validation_warnings %||% 0) > 0) {
          shiny::p(class = "warn-text", list("Validation warnings detected. ", dq_link, "."))
        } else {
          shiny::p(class = "muted", list("Validation summary available on ", dq_link, "."))
        }
      )
    })

    safe_filename <- function(prefix) {
      y <- analysis_year()
      stamp <- format(Sys.Date(), "%Y%m%d")
      r <- selected_reporter()
      scope <- if (identical(r, "__GLOBAL__")) "global" else r
      sprintf("%s_%s_y%s_%s.csv", prefix, scope, y, stamp)
    }

    output$dl_analytics <- shiny::downloadHandler(
      filename = function() safe_filename("overview_country_year"),
      content = function(file) {
        cy <- cy_data()
        shiny::req(cy)
        y <- analysis_year()
        r <- selected_reporter()
        out <- if (identical(r, "__GLOBAL__")) {
          filter_overview_data(cy, year = y, reporter = "__GLOBAL__")
        } else {
          filter_overview_data(cy, year = NULL, reporter = r, years_all = TRUE)
        }

        drop <- intersect(names(out), c("raw_file", "raw_path", "file_path"))
        if (length(drop)) out[, (drop) := NULL]
        data.table::fwrite(out, file, bom = TRUE)
      }
    )

    output$dl_ranking <- shiny::downloadHandler(
      filename = function() safe_filename("overview_ranking"),
      content = function(file) {
        data.table::fwrite(ranked(), file, bom = TRUE)
      }
    )

    output$dl_kpi <- shiny::downloadHandler(
      filename = function() safe_filename("overview_kpi"),
      content = function(file) {
        data.table::fwrite(kpi_summary_table(kpi()), file, bom = TRUE)
      }
    )
  })
}

overview_metric_card <- function(title, value, unit = NULL, subtitle = NULL, tone = NULL) {
  shiny::div(
    class = paste("metric-card", if (!is.null(tone)) paste0("tone-", tone)),
    shiny::div(class = "metric-label", title),
    shiny::div(
      class = "metric-value",
      `aria-label` = paste(title, value, unit %||% ""),
      value
    ),
    if (!is.null(unit)) shiny::div(class = "metric-unit", unit),
    if (!is.null(subtitle) && nzchar(subtitle)) {
      shiny::div(class = "metric-subtitle", title = subtitle, subtitle)
    }
  )
}

metric_card <- function(title, value) {
  overview_metric_card(title, value)
}

planned_module_ui <- function(id, title, blurb) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "empty-state planned",
    shiny::h3(title),
    shiny::p(blurb),
    shiny::p(class = "muted", "Planned for a later phase.")
  )
}

planned_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
