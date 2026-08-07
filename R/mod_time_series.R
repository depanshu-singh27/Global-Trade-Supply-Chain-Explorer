mod_time_series_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "time-series-page",
      shiny::div(
        class = "hero-panel",
        shiny::h2("Time-Series & Commodity Analysis"),
        shiny::p(
          "Annual HS-85 trends for 2019–2024 using completed World-partner country totals, ",
          "plus bilateral/HS4 views from the selected analytical-universe detailed dataset. ",
          "Indexed and CAGR figures are historical analytical transformations — not forecasts."
        )
      ),
      shiny::uiOutput(ns("scope_status")),
      shiny::uiOutput(ns("filter_bar")),
      shiny::uiOutput(ns("kpi_strip")),
      shiny::div(
        class = "ts-grid",
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Main trade time series"),
          plotly::plotlyOutput(ns("main_chart"), height = "340px"),
          shiny::uiOutput(ns("main_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Economy comparison"),
          plotly::plotlyOutput(ns("compare_chart"), height = "300px"),
          shiny::uiOutput(ns("compare_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Growth & indexed performance"),
          plotly::plotlyOutput(ns("growth_chart"), height = "300px"),
          shiny::uiOutput(ns("growth_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Import / export decomposition"),
          plotly::plotlyOutput(ns("decomp_chart"), height = "300px"),
          shiny::uiOutput(ns("decomp_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Bilateral / commodity trends (detailed)"),
          shiny::uiOutput(ns("detailed_badge")),
          plotly::plotlyOutput(ns("detailed_chart"), height = "300px"),
          shiny::uiOutput(ns("detailed_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Commodity treemap (detailed)"),
          plotly::plotlyOutput(ns("treemap"), height = "320px"),
          shiny::uiOutput(ns("treemap_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Commodity movers — absolute increase"),
          plotly::plotlyOutput(ns("movers_up"), height = "280px")
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Commodity movers — absolute decrease"),
          plotly::plotlyOutput(ns("movers_down"), height = "280px")
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Analytical table"),
          shiny::checkboxInput(ns("table_technical"), "Show technical columns", FALSE),
          DT::DTOutput(ns("ts_table"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Coverage & methodology"),
          shiny::uiOutput(ns("coverage_panel"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Downloads"),
          shiny::p(class = "muted", "Exports respect active filters. No secrets or filesystem paths."),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(ns("dl_series"), "Time series CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_compare"), "Comparison CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_index"), "Indexed growth CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_treemap"), "Treemap CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_movers"), "Movers CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_table"), "Table CSV", class = "btn-sm btn-outline-primary")
          )
        )
      ),
      shiny::uiOutput(ns("empty_fallback"))
    )
  )
}

mod_time_series_server <- function(id, snap, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cy_data <- shiny::reactive({
      s <- snap()
      s$map_analytics %||% s$country_year_analytics
    })

    detailed_data <- shiny::reactive({
      s <- snap()
      s$trade_detailed_enriched
    })

    coverage <- shiny::reactive({
      s <- snap()
      if (!is.null(s$detailed_coverage) && length(s$detailed_coverage)) s$detailed_coverage
      else trade_flow_coverage_status(s)
    })

    output$empty_fallback <- shiny::renderUI({
      if (!is.null(cy_data()) && nrow(cy_data()) > 0) return(NULL)
      shiny::div(
        class = "empty-state", role = "status",
        shiny::h3("Time-series analytics unavailable"),
        shiny::p("Country-year analytics were not found. This page does not call external APIs.")
      )
    })

    output$scope_status <- shiny::renderUI({
      c <- coverage()
      status_label <- if (coverage_is_selected_universe_complete(c)) "complete" else
        (c$production_status %||% "partial")
      shiny::tagList(
        shiny::div(
          class = "status-badge-row",
          status_badge("Global country-year", snap()$pipeline_status$global_trade %||% "complete"),
          status_badge(
            "Detailed bilateral",
            status_label,
            label_override = sprintf("%d/%d", c$represented_reporter_count %||% 0L,
                                     c$selected_reporter_count %||% 0L)
          )
        ),
        shiny::div(
          class = "partial-data-notice ts-partial-notice",
          role = "status",
          shiny::tags$strong(detailed_coverage_notice(c, context = "time_series"))
        )
      )
    })

    output$filter_bar <- shiny::renderUI({
      cy <- cy_data()
      shiny::req(cy)
      yrs <- ts_year_choices(cy)
      rng <- ts_default_year_range(cy)
      labs <- unique(prepare_ts_global(cy)[, .(reporter_iso3, reporter_name)])
      labs <- labs[order(reporter_name)]
      eco <- c(setNames(labs$reporter_iso3, paste0(labs$reporter_name, " (", labs$reporter_iso3, ")")))
      det <- detailed_data()
      det_reps <- if (!is.null(det) && nrow(det)) {
        sort(unique(prepare_detailed_trade(det)$reporter_iso3))
      } else character()
      det_labs <- eco[unname(eco) %in% det_reps]
      partners <- c("All partners" = "__ALL__")
      hs <- c("All HS4" = "__ALL__")
      if (!is.null(det) && nrow(det)) {
        ch <- trade_flow_filter_choices(det, snap()$analytical_universe)
        partners <- c(partners, ch$partner_labels)
        hs <- c(hs, ch$hs_labels)
      }
      shiny::div(
        class = "filter-toolbar ts-toolbar",
        role = "search",
        `aria-label` = "Time-series analysis filters",
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("scope"), "Analysis scope"),
          shiny::selectInput(
            ns("scope"), NULL,
            choices = c(
              "Global aggregate" = "global",
              "Single economy" = "single",
              "Compare economies" = "compare",
              "Bilateral / commodity detail" = "detailed"
            ),
            selected = "single"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year_min"), "From year"),
          shiny::selectInput(ns("year_min"), NULL, choices = yrs, selected = rng[1])
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year_max"), "To year"),
          shiny::selectInput(ns("year_max"), NULL, choices = yrs, selected = rng[2])
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("economy"), "Primary economy"),
          shiny::selectizeInput(ns("economy"), NULL, choices = eco, selected = default_comparison_economies(cy, n = 1L)[1])
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("compare"), "Comparison economies (max 5)"),
          shiny::selectizeInput(
            ns("compare"), NULL,
            choices = eco,
            selected = default_comparison_economies(cy, n = 4L),
            multiple = TRUE,
            options = list(maxItems = 5)
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("metric"), "Metric"),
          shiny::selectInput(
            ns("metric"), NULL,
            choices = c(
              "Total trade" = "total_trade",
              "Imports" = "imports",
              "Exports" = "exports",
              "Trade balance" = "trade_balance",
              "Total trade / GDP" = "total_trade_pct_gdp",
              "Balance / GDP" = "balance_pct_gdp",
              "Trade per capita" = "trade_per_capita"
            ),
            selected = "total_trade"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("transform"), "Display transformation"),
          shiny::selectInput(
            ns("transform"), NULL,
            choices = c(
              "Absolute value" = "absolute",
              "Year-over-year %" = "yoy",
              "Indexed growth" = "index",
              "Share of selected total" = "share"
            ),
            selected = "absolute"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("flow"), "Flow (detailed)"),
          shiny::selectInput(
            ns("flow"), NULL,
            choices = c("Both" = "both", "Imports" = "imports", "Exports" = "exports"),
            selected = "both"
          )
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("partner"), "Partner (detailed)"),
          shiny::selectizeInput(ns("partner"), NULL, choices = partners, selected = "__ALL__")
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("hs4"), "HS4 commodity (detailed)"),
          shiny::selectizeInput(ns("hs4"), NULL, choices = hs, selected = "__ALL__")
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("top_n"), "Top-N"),
          shiny::selectInput(ns("top_n"), NULL, choices = c(5L, 10L, 15L, 20L), selected = 10L)
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("det_reporter"), "Detailed reporter"),
          shiny::selectizeInput(
            ns("det_reporter"), NULL,
            choices = if (length(det_labs)) det_labs else c("None available" = ""),
            selected = if (length(det_labs)) unname(det_labs)[1] else ""
          )
        )
      )
    })

    year_min <- shiny::reactive({
      v <- suppressWarnings(as.integer(input$year_min))
      shiny::req(length(v) == 1L, is.finite(v))
      v
    })
    year_max <- shiny::reactive({
      v <- suppressWarnings(as.integer(input$year_max))
      shiny::req(length(v) == 1L, is.finite(v))
      v
    })
    metric <- shiny::reactive(input$metric %||% "total_trade")
    transform <- shiny::reactive(input$transform %||% "absolute")
    scope <- shiny::reactive(input$scope %||% "single")
    primary_economy <- shiny::reactive({
      value <- input$economy
      shiny::req(length(value) == 1L)
      shiny::req(!is.na(value))
      shiny::req(nzchar(as.character(value)))
      as.character(value)
    })

    main_series <- shiny::reactive({
      cy <- cy_data()
      shiny::req(cy)
      sc <- scope()
      if (identical(sc, "global")) {
        g <- aggregate_global_series(cy, year_min(), year_max())
        col <- switch(metric(),
                      "imports" = "imports", "exports" = "exports",
                      "trade_balance" = "balance", "total")
        out <- g[, .(year, value = get(col), reporter_iso3 = "GLOBAL",
                     reporter_name = "Global aggregate", series = "Global aggregate")]
      } else if (identical(sc, "compare")) {
        isos <- validate_comparison_economies(input$compare, max_n = 5L, primary = primary_economy())
        shiny::req(length(isos) >= 1L)
        out <- comparison_metric_series(cy, isos, metric(), year_min(), year_max(), max_n = 5L)
      } else {
        out <- economy_metric_series(cy, primary_economy(), metric(), year_min(), year_max())
      }
      apply_series_transform(out, metric(), transform())
    })

    compare_series <- shiny::reactive({
      cy <- cy_data()
      shiny::req(cy)
      isos <- validate_comparison_economies(input$compare, 5L, primary_economy())
      shiny::req(length(isos) >= 1L)
      apply_series_transform(
        comparison_metric_series(cy, isos, metric(), year_min(), year_max(), 5L),
        metric(), transform()
      )
    })

    index_series <- shiny::reactive({
      cy <- cy_data()
      shiny::req(cy)
      sc <- scope()
      raw <- if (identical(sc, "compare")) {
        isos <- validate_comparison_economies(input$compare, 5L, primary_economy())
        shiny::req(length(isos) >= 1L)
        comparison_metric_series(cy, isos, metric(), year_min(), year_max(), 5L)
      } else if (identical(sc, "global")) {
        g <- aggregate_global_series(cy, year_min(), year_max())
        col <- switch(metric(), "imports" = "imports", "exports" = "exports",
                      "trade_balance" = "balance", "total")
        g[, .(year, value = get(col), reporter_iso3 = "GLOBAL",
              reporter_name = "Global", series = "Global")]
      } else {
        economy_metric_series(cy, primary_economy(), metric(), year_min(), year_max())
      }
      apply_series_transform(raw, metric(), "index")
    })

    decomp <- shiny::reactive({
      cy <- cy_data()
      shiny::req(cy)
      if (identical(scope(), "global")) {
        import_export_decomposition(cy, "global", NULL, year_min(), year_max())
      } else {
        import_export_decomposition(cy, "single", primary_economy(), year_min(), year_max())
      }
    })

    detailed_filtered <- shiny::reactive({
      det <- detailed_data()
      if (is.null(det) || !nrow(det)) return(data.table::data.table())
      filter_detailed_trade(
        det,
        year_min = year_min(), year_max = year_max(),
        reporters = if (nzchar(input$det_reporter %||% "")) input$det_reporter else "__ALL__",
        partners = input$partner %||% "__ALL__",
        flows = normalize_flow_codes(input$flow %||% "both"),
        hs_codes = input$hs4 %||% "__ALL__"
      )
    })

    detailed_trend <- shiny::reactive({
      det <- detailed_data()
      if (is.null(det) || !nrow(det)) return(list(visible = data.table::data.table(), coverage_pct = NA_real_))
      tr <- aggregate_detailed_trend(
        det,
        year_min = year_min(), year_max = year_max(),
        reporters = if (nzchar(input$det_reporter %||% "")) input$det_reporter else NULL,
        partners = if (!identical(input$partner, "__ALL__")) input$partner else NULL,
        flows = normalize_flow_codes(input$flow %||% "both"),
        hs_codes = if (!identical(input$hs4, "__ALL__")) input$hs4 else NULL,
        group_by = c("year", "flow_code")
      )
      if (!nrow(tr)) return(list(visible = tr, coverage_pct = NA_real_))

      if ("series" %in% names(tr)) {
        select_top_detailed_series(tr, "series", top_n = as.integer(input$top_n %||% 10L))
      } else {
        tr[, series := "Detailed"]
        list(visible = tr, coverage_pct = 100, other_value = 0)
      }
    })

    treemap_prep <- shiny::reactive({
      det <- detailed_data()
      if (is.null(det) || !nrow(det)) {
        return(list(data = data.table::data.table(), total = 0, other_value = 0))
      }
      prepare_commodity_treemap(
        det,
        year_min = year_min(), year_max = year_max(),
        reporters = if (nzchar(input$det_reporter %||% "")) input$det_reporter else NULL,
        partners = if (!identical(input$partner, "__ALL__")) input$partner else NULL,
        flows = normalize_flow_codes(input$flow %||% "both"),
        top_n = as.integer(input$top_n %||% 10L)
      )
    })

    movers <- shiny::reactive({
      det <- detailed_data()
      if (is.null(det) || !nrow(det)) {
        return(commodity_movers(data.table::data.table(), year_min(), year_max()))
      }
      commodity_movers(
        det,
        start_year = year_min(), end_year = year_max(),
        reporters = if (nzchar(input$det_reporter %||% "")) input$det_reporter else NULL,
        partners = if (!identical(input$partner, "__ALL__")) input$partner else NULL,
        flows = normalize_flow_codes(input$flow %||% "both"),
        top_n = as.integer(input$top_n %||% 10L)
      )
    })

    output$kpi_strip <- shiny::renderUI({
      cy <- cy_data()
      if (is.null(cy) || !nrow(cy)) return(NULL)
      sc <- scope()
      if (identical(sc, "detailed")) {
        k <- ts_detailed_kpis(detailed_filtered(), coverage())
        shiny::div(
          class = "metric-grid overview-kpi-grid",
          overview_metric_card("Filtered detailed trade", format_usd_compact(k$filtered_trade_value),
                               "Current US$", k$scope_note),
          overview_metric_card("Observations", format_count(k$n_observations), "Rows", NULL),
          overview_metric_card("Years", format_count(k$n_years), "In filter", NULL),
          overview_metric_card("Partners", format_count(k$n_partners), "Country partners", NULL),
          overview_metric_card("HS4", format_count(k$n_hs4), "Headings", NULL),
          overview_metric_card("Detailed coverage", k$coverage_label, "Represented / selected", NULL)
        )
      } else if (identical(sc, "compare")) {
        k <- ts_kpi_comparison(compare_series(), metric())
        shiny::div(
          class = "metric-grid overview-kpi-grid",
          overview_metric_card("Economies compared", format_count(k$n_economies), "Max 5", NULL),
          overview_metric_card("Highest latest", format_ts_value(k$highest_latest, metric()),
                               paste("Year", k$latest_year), k$highest_latest_iso),
          overview_metric_card("Fastest CAGR", format_pct(k$fastest_cagr),
                               "Period CAGR", k$fastest_cagr_iso),
          overview_metric_card("Largest decline (CAGR)", format_pct(k$largest_decline_cagr),
                               "Period CAGR", k$largest_decline_iso),
          overview_metric_card("Median latest", format_ts_value(k$median_latest, metric()),
                               "Across comparison", NULL),
          overview_metric_card("Period", paste(year_min(), year_max(), sep = "–"), "Selected years", NULL)
        )
      } else {
        k <- ts_kpi_single_or_global(main_series(), metric())
        shiny::div(
          class = "metric-grid overview-kpi-grid",
          overview_metric_card("Latest value", format_ts_value(k$latest_value, metric()),
                               paste("Year", k$latest_year), "Completed global scope"),
          overview_metric_card(
            if (ts_metric_allows_pct_growth(metric())) "YoY change" else "Balance change",
            if (ts_metric_allows_pct_growth(metric())) format_pct(k$yoy) else format_usd_compact(k$yoy),
            "Consecutive year only", NULL
          ),
          overview_metric_card("Period CAGR", format_pct(k$cagr), "Historical, not a forecast", NULL),
          overview_metric_card("Period high", format_ts_value(k$period_high, metric()), NULL, NULL),
          overview_metric_card("Period low", format_ts_value(k$period_low, metric()), NULL, NULL),
          overview_metric_card("Years in series", format_count(k$n_years), NULL, NULL)
        )
      }
    })

    output$main_chart <- plotly::renderPlotly({
      shiny::req(cy_data())
      if (is.null(input$scope) || is.null(input$year_min) || is.null(input$year_max)) {
        return(ts_empty_plotly("Initialising time-series filters…"))
      }
      if (identical(scope(), "single") &&
          (is.null(input$economy) || !length(input$economy) || !nzchar(as.character(input$economy)[1]))) {
        return(ts_empty_plotly("Select a primary economy to render the single-economy series."))
      }
      ms <- main_series()
      if (is.null(ms) || !nrow(ms)) {
        return(ts_empty_plotly("No observations for the current single-economy filters."))
      }
      ts_main_plotly(
        ms,
        title = paste0("Main series — ", ts_metric_label(metric()), " (", scope(), ")"),
        metric = metric(),
        transform = transform(),
        show_zero = ts_metric_is_signed(metric())
      )
    })
    output$main_summary <- shiny::renderUI({
      shiny::p(class = "chart-summary", ts_accessibility_summary(main_series(), metric(), scope()))
    })

    output$compare_chart <- plotly::renderPlotly({
      ts_main_plotly(compare_series(), "Economy comparison", metric(), transform())
    })
    output$compare_summary <- shiny::renderUI({
      k <- ts_kpi_comparison(compare_series(), metric())
      shiny::p(
        class = "chart-summary",
        paste0(
          "Comparing up to five economies. Latest-year leader: ", k$highest_latest_iso %||% "n/a",
          ". Aggregate entities excluded. Missing observations are not interpolated."
        )
      )
    })

    output$growth_chart <- plotly::renderPlotly({
      shiny::req(cy_data())
      if (is.null(input$scope) || is.null(input$year_min) || is.null(input$year_max)) {
        return(ts_empty_plotly("Initialising growth / indexed chart…"))
      }
      if (identical(scope(), "single") &&
          (is.null(input$economy) || !length(input$economy) || !nzchar(as.character(input$economy)[1]))) {
        return(ts_empty_plotly("Select a primary economy to render indexed performance."))
      }
      ix <- index_series()
      if (is.null(ix) || !nrow(ix)) {
        return(ts_empty_plotly("No indexed series for the current filters."))
      }
      ts_main_plotly(ix, "Indexed performance (own baseline = 100)", metric(), "index")
    })
    output$growth_summary <- shiny::renderUI({
      ix <- index_series()
      bl <- if (nrow(ix) && "baseline_year" %in% names(ix)) {
        unique(stats::na.omit(ix$baseline_year))[1]
      } else NA
      shiny::p(
        class = "chart-summary",
        paste0(
          "Index uses each series’ first positive valid year as baseline (= 100)",
          if (!is.na(bl)) paste0("; example baseline year visible: ", bl) else "",
          ". YoY % is not applied to signed trade balance (absolute change used instead). ",
          "CAGR summarises historical change only — not a forecast."
        )
      )
    })

    output$decomp_chart <- plotly::renderPlotly({
      ts_decomposition_plotly(decomp(), "Imports and exports over time")
    })
    output$decomp_summary <- shiny::renderUI({
      d <- decomp()
      ok <- nrow(d) && all(abs(d$imports + d$exports - d$total) < 1e-3 * pmax(1, abs(d$total)), na.rm = TRUE)
      shiny::p(
        class = "chart-summary",
        paste0(
          "Grouped bars show imports and exports; balance on a secondary axis. ",
          if (isTRUE(ok)) "Totals reconcile to imports + exports. " else "",
          "Current US dollars."
        )
      )
    })

    output$detailed_badge <- shiny::renderUI({
      c <- coverage()
      shiny::span(
        class = paste("status-badge", status_badge_class(c$production_status %||% "partial")),
        paste0("Detailed ", c$represented_reporter_count %||% 0, "/", c$selected_reporter_count %||% 0)
      )
    })

    output$detailed_chart <- plotly::renderPlotly({
      sel <- detailed_trend()
      vis <- sel$visible
      if (!nrow(vis)) return(ts_empty_plotly("No detailed observations for these filters."))
      vis <- data.table::as.data.table(vis)
      if (!"display_value" %in% names(vis)) vis[, display_value := value]
      if (!"series" %in% names(vis)) vis[, series := "Detailed"]
      ts_main_plotly(vis, "Detailed bilateral/commodity trend", "total_trade", "absolute")
    })
    output$detailed_summary <- shiny::renderUI({
      sel <- detailed_trend()
      shiny::p(
        class = "chart-summary",
        paste0(
          "Detailed series coverage of filtered value shown: ", format_pct(sel$coverage_pct),
          ". Not a complete top-20 or global HS-85 commodity market."
        )
      )
    })

    output$treemap <- plotly::renderPlotly({
      ts_treemap_plotly(
        treemap_prep(),
        paste0("HS4 composition — detailed filters (", year_min(), "–", year_max(), ")")
      )
    })
    output$treemap_summary <- shiny::renderUI({
      t <- treemap_prep()
      cov_txt <- if (coverage_is_selected_universe_complete(coverage())) {
        "Based on completed selected-universe detailed observations."
      } else {
        "Based on currently available detailed observations (selected universe may be partial)."
      }
      shiny::p(
        class = "chart-summary",
        paste0(
          "Treemap total ", format_usd_compact(t$total),
          "; OTHER ", format_usd_compact(t$other_value),
          " (", format_pct(if (t$total > 0) 100 * t$other_value / t$total else NA_real_),
          "). ", cov_txt
        )
      )
    })

    output$movers_up <- plotly::renderPlotly({
      m <- movers()
      if (is.null(m$absolute_increase) || !nrow(m$absolute_increase)) {
        msg <- commodity_movers_empty_message(
          m,
          detailed = detailed_data(),
          reporters = if (nzchar(input$det_reporter %||% "")) input$det_reporter else NULL,
          partners = if (!identical(input$partner, "__ALL__")) input$partner else NULL,
          represented_reporters = coverage()$represented_reporters,
          start_year = year_min(),
          end_year = year_max()
        )
        return(ts_empty_plotly(msg))
      }
      ts_movers_plotly(m$absolute_increase, "abs_change",
                       paste0("Increase ", m$start_year, "→", m$end_year),
                       gte_palette()$surplus)
    })
    output$movers_down <- plotly::renderPlotly({
      m <- movers()
      if (is.null(m$absolute_decrease) || !nrow(m$absolute_decrease)) {
        msg <- commodity_movers_empty_message(
          m,
          detailed = detailed_data(),
          reporters = if (nzchar(input$det_reporter %||% "")) input$det_reporter else NULL,
          partners = if (!identical(input$partner, "__ALL__")) input$partner else NULL,
          represented_reporters = coverage()$represented_reporters,
          start_year = year_min(),
          end_year = year_max()
        )
        return(ts_empty_plotly(msg))
      }
      ts_movers_plotly(m$absolute_decrease, "abs_change",
                       paste0("Decrease ", m$start_year, "→", m$end_year),
                       gte_palette()$deficit)
    })

    table_data <- shiny::reactive({
      if (identical(scope(), "detailed")) {
        dt <- detailed_filtered()
        if (!nrow(dt)) return(dt)
        cols <- c("year", "reporter_iso3", "reporter_name", "partner_iso3", "partner_name",
                  "flow_code", "hs_code", "commodity_description", "trade_value_usd")
        if (isTRUE(input$table_technical)) {
          cols <- c(cols, intersect(c("ingested_at", "request_id", "universe_checksum"), names(dt)))
        }
        dt[, intersect(cols, names(dt)), with = FALSE]
      } else {
        cy <- prepare_ts_global(cy_data())
        cy <- filter_ts_years(cy, year_min(), year_max())
        if (identical(scope(), "single")) {
          iso <- normalize_primary_economy(primary_economy())
          shiny::req(!is.na(iso))
          cy <- cy[reporter_iso3 == iso]
        }
        if (identical(scope(), "compare")) {
          isos <- validate_comparison_economies(input$compare, 5L, primary_economy())
          cy <- cy[reporter_iso3 %chin% isos]
        }
        cols <- c("year", "reporter_iso3", "reporter_name", "imports_value_usd", "exports_value_usd",
                  "total_trade_value_usd", "trade_balance_usd", "gdp_current_usd",
                  "total_trade_pct_gdp", "total_trade_per_capita_usd")
        cy[, intersect(cols, names(cy)), with = FALSE]
      }
    })

    output$ts_table <- DT::renderDT({
      dt <- table_data()
      if (!nrow(dt)) {
        return(DT::datatable(
          data.frame(Message = "No rows for the current filters."),
          rownames = FALSE, options = list(dom = "t")
        ))
      }
      if (nrow(dt) > 5000L) dt <- dt[seq_len(5000L)]
      DT::datatable(
        dt, rownames = FALSE, filter = "top",
        options = list(pageLength = 20, scrollX = TRUE),
        caption = "Analytical table for the active time-series scope."
      )
    }, server = TRUE)

    output$coverage_panel <- shiny::renderUI({
      c <- coverage()
      s <- snap()
      shiny::tagList(
        shiny::tags$ul(
          class = "coverage-list",
          shiny::tags$li(paste0("Global trade status: ", s$pipeline_status$global_trade %||% "complete")),
          shiny::tags$li(paste0("Detailed status: ", c$production_status %||% "partial")),
          shiny::tags$li(paste0("Selected universe reporters: ", format_count(c$selected_reporter_count))),
          shiny::tags$li(paste0("Represented detailed: ", paste(c$represented_reporters, collapse = ", "))),
          shiny::tags$li(paste0("Missing detailed: ", if (length(c$missing_reporters)) paste(c$missing_reporters, collapse = ", ") else "none")),
          shiny::tags$li(paste0("Selected year range: ", year_min(), "–", year_max())),
          shiny::tags$li(paste0("Universe checksum: ", c$universe_checksum %||% "—",
                                if (isTRUE(c$checksum_stale)) " (differs from expected)" else "")),
          shiny::tags$li(paste0("Latest detailed ingestion: ", c$latest_ingested_at %||% "Unavailable")),
          shiny::tags$li(paste0("Validation warnings: ", format_count(c$validation_warnings)))
        ),
        shiny::p(
          class = "method-note",
          list(
            if (coverage_is_selected_universe_complete(c)) {
              "Global trends use completed World-partner country totals. Bilateral/HS4 analysis uses the completed selected-universe detailed dataset. "
            } else {
              "Global trends use completed World-partner country totals. Bilateral/HS4 analysis uses the currently available selected-universe detailed dataset. "
            },
            selected_universe_disclaimer(), " ",
            "Values are current US dollars (nominal). CPI is context only. Indexed values and CAGR are not forecasts. ",
            "Missing observations are not imputed. ",
            shiny::tags$a(
              href = "#", class = "dq-link",
              onclick = "document.querySelector('[data-value=\"Data Quality\"]')?.click(); return false;",
              "Open Data Quality"
            ), "."
          )
        )
      )
    })

    output$dl_series <- shiny::downloadHandler(
      filename = function() ts_download_filename(
        "trade_timeseries",
        tryCatch(primary_economy(), error = function(e) "global"),
        paste0(year_min(), "_", year_max())
      ),
      content = function(file) write_ts_csv(main_series(), file)
    )
    output$dl_compare <- shiny::downloadHandler(
      filename = function() ts_download_filename("trade_comparison", metric(), year_max()),
      content = function(file) write_ts_csv(compare_series(), file)
    )
    output$dl_index <- shiny::downloadHandler(
      filename = function() ts_download_filename("trade_index", metric(), paste0(year_min(), "_", year_max())),
      content = function(file) write_ts_csv(index_series(), file)
    )
    output$dl_treemap <- shiny::downloadHandler(
      filename = function() ts_download_filename("commodity_treemap", input$det_reporter %||% "all", year_max()),
      content = function(file) write_ts_csv(treemap_prep()$data, file)
    )
    output$dl_movers <- shiny::downloadHandler(
      filename = function() ts_download_filename("commodity_movers", year_min(), year_max()),
      content = function(file) write_ts_csv(movers()$absolute_increase, file)
    )
    output$dl_table <- shiny::downloadHandler(
      filename = function() ts_download_filename("timeseries_table", scope(), year_max()),
      content = function(file) write_ts_csv(table_data(), file)
    )
  })
}
