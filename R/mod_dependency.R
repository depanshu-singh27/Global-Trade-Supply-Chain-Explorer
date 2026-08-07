mod_dependency_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "dependency-page",
      shiny::div(
        class = "hero-panel",
        shiny::h2("Dependency Explorer"),
        shiny::p(
          "Direct reported-import concentration for HS-85 commodities in the currently available ",
          "detailed dataset. Not causal dependency, domestic production exposure, firm-level ",
          "substitutability, or indirect supply-chain effects. Shock propagation belongs to Phase 10."
        )
      ),
      shiny::uiOutput(ns("limitation_notice")),
      shiny::uiOutput(ns("filter_bar")),
      shiny::uiOutput(ns("kpi_strip")),
      shiny::div(
        class = "dependency-grid",
        shiny::div(
          class = "chart-card chart-card-wide dependency-matrix-card",
          shiny::h3(class = "chart-title", "Dependency matrix"),
          shiny::selectInput(
            ns("matrix_mode"), NULL,
            choices = dependency_matrix_mode_choices(),
            selected = "reporter_supplier"
          ),
          shiny::uiOutput(ns("matrix_dim_text")),
          plotly::plotlyOutput(ns("heatmap"), height = "420px"),
          shiny::uiOutput(ns("matrix_summary")),
          DT::DTOutput(ns("matrix_table"))
        ),
        shiny::div(
          class = "chart-card dependency-profile-card",
          shiny::h3(class = "chart-title", "Selected economy profile"),
          shiny::uiOutput(ns("reporter_profile"))
        ),
        shiny::div(
          class = "chart-card dependency-commodity-card",
          shiny::h3(class = "chart-title", "Selected commodity profile"),
          shiny::uiOutput(ns("commodity_profile"))
        ),
        shiny::div(
          class = "chart-card dependency-concentration-card",
          shiny::h3(class = "chart-title", "Concentration & diversification"),
          plotly::plotlyOutput(ns("scatter"), height = "320px")
        ),
        shiny::div(
          class = "chart-card dependency-ranking-card",
          shiny::h3(class = "chart-title", "Highest-dependency rankings"),
          shiny::selectInput(
            ns("rank_metric"), NULL,
            choices = c(
              "Top-supplier share" = "top_1_share",
              "Supplier HHI" = "supplier_hhi",
              "Concentrated import exposure" = "concentrated_import_value"
            ),
            selected = "top_1_share"
          ),
          plotly::plotlyOutput(ns("rank_plot"), height = "320px"),
          shiny::uiOutput(ns("rank_summary"))
        ),
        shiny::div(
          class = "chart-card dependency-supplier-card",
          shiny::h3(class = "chart-title", "Supplier exposure ranking"),
          DT::DTOutput(ns("supplier_table"))
        ),
        shiny::div(
          class = "chart-card dependency-trend-card",
          shiny::h3(class = "chart-title", "Concentration trends"),
          plotly::plotlyOutput(ns("trend_plot"), height = "280px")
        ),
        shiny::div(
          class = "chart-card chart-card-wide dependency-table-card",
          shiny::h3(class = "chart-title", "Detailed dependency table"),
          shiny::checkboxInput(ns("table_technical"), "Show technical columns", FALSE),
          DT::DTOutput(ns("detail_table"))
        ),
        shiny::div(
          class = "chart-card dependency-coverage-card",
          shiny::h3(class = "chart-title", "Coverage & methodology"),
          shiny::uiOutput(ns("coverage_panel"))
        ),
        shiny::div(
          class = "chart-card dependency-download-card",
          shiny::h3(class = "chart-title", "Downloads"),
          shiny::p(class = "muted", "Exports respect active filters. No secrets or filesystem paths."),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(ns("dl_rs_matrix"), "Reporter–supplier CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_cc_sparse"), "Country–commodity sparse CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_cc_mtx"), "Matrix Market .mtx", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_detail"), "Detailed dependencies CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_reporter"), "Reporter concentration CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_commodity"), "Commodity concentration CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_supplier"), "Supplier exposure CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_diag"), "Diagnostics CSV", class = "btn-sm btn-outline-primary")
          )
        )
      ),
      shiny::uiOutput(ns("empty_fallback"))
    )
  )
}

mod_dependency_server <- function(id, snap, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    detailed_data <- shiny::reactive({
      s <- snap()
      s$trade_detailed_enriched %||% s$trade_detailed
    })

    coverage <- shiny::reactive({
      s <- snap()
      if (!is.null(s$detailed_coverage) && length(s$detailed_coverage)) s$detailed_coverage
      else trade_flow_coverage_status(s)
    })

    output$empty_fallback <- shiny::renderUI({
      d <- detailed_data()
      if (!is.null(d) && nrow(d) > 0) return(NULL)
      shiny::div(
        class = "empty-state", role = "status",
        shiny::h3("Dependency analytics unavailable"),
        shiny::p("No detailed bilateral observations were found. This page does not call external APIs.")
      )
    })

    output$limitation_notice <- shiny::renderUI({
      c <- coverage()
      status_label <- if (coverage_is_selected_universe_complete(c)) "complete" else
        (c$production_status %||% "partial")
      shiny::tagList(
        shiny::div(
          class = "status-badge-row",
          status_badge(
            "Detailed imports",
            status_label,
            label_override = sprintf(
              "%d/%d",
              c$represented_reporter_count %||% 0L,
              c$selected_reporter_count %||% 0L
            )
          )
        ),
        shiny::div(
          class = "partial-data-notice dependency-partial-notice",
          role = "status",
          shiny::tags$strong(detailed_coverage_notice(c, context = "dependency"))
        ),
        shiny::div(
          class = "partial-data-notice dependency-method-notice",
          role = "note",
          shiny::tags$strong(
            "Dependency measures describe direct reported import concentration in the selected-universe detailed dataset. "
          ),
          "They do not measure domestic production, inventory buffers, firm-level substitutability or indirect supply-chain effects."
        )
      )
    })

    output$filter_bar <- shiny::renderUI({
      d <- detailed_data()
      shiny::req(d)
      ch <- trade_flow_filter_choices(d, snap()$analytical_universe)
      yrs <- ch$years
      def_year <- ch$default_year
      shiny::div(
        class = "filter-toolbar dependency-toolbar",
        role = "search",
        `aria-label` = "Dependency filters",
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year_mode"), "Year mode"),
          shiny::selectInput(
            ns("year_mode"), NULL,
            choices = c("Latest year" = "latest", "Full range" = "full", "Custom range" = "custom"),
            selected = "latest"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year_min"), "From year"),
          shiny::selectInput(ns("year_min"), NULL, choices = yrs, selected = def_year)
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year_max"), "To year"),
          shiny::selectInput(ns("year_max"), NULL, choices = yrs, selected = def_year)
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("reporter"), "Reporting economy"),
          shiny::selectizeInput(
            ns("reporter"), NULL,
            choices = c("All represented reporters" = "__ALL__", ch$reporter_labels),
            selected = "__ALL__"
          )
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("partner"), "Partner / supplier"),
          shiny::selectizeInput(
            ns("partner"), NULL,
            choices = c("All eligible partners" = "__ALL__", ch$partner_labels),
            selected = "__ALL__"
          )
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("hs"), "HS4 commodity"),
          shiny::selectizeInput(
            ns("hs"), NULL,
            choices = c("All selected HS4" = "__ALL__", ch$hs_labels),
            selected = "__ALL__"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("metric"), "Dependency metric"),
          shiny::selectInput(ns("metric"), NULL, choices = dependency_metric_choices(),
                             selected = DEP_DEFAULT_METRIC)
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("agg_mode"), "Aggregation mode"),
          shiny::selectInput(ns("agg_mode"), NULL, choices = dependency_aggregation_choices(),
                             selected = "commodity_specific")
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("top_n"), "Matrix top-N"),
          shiny::selectInput(
            ns("top_n"), NULL,
            choices = c("10" = "10", "20" = "20", "50" = "50", "100" = "100", "200" = "200"),
            selected = "20"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("min_value"), "Min link value (US$)"),
          shiny::numericInput(ns("min_value"), NULL, value = 0, min = 0, step = 1000)
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("scale"), "Value scale"),
          shiny::selectInput(
            ns("scale"), NULL,
            choices = c("Automatic" = "auto", "Millions" = "millions", "Billions" = "billions"),
            selected = "auto"
          )
        )
      )
    })

    year_bounds <- shiny::reactive({
      d <- detailed_data()
      shiny::req(d)
      ch <- trade_flow_filter_choices(d, snap()$analytical_universe)
      mode <- input$year_mode %||% "latest"
      if (identical(mode, "full")) return(c(min(ch$years), max(ch$years)))
      if (identical(mode, "custom")) {
        ymin <- as.integer(input$year_min %||% ch$default_year)
        ymax <- as.integer(input$year_max %||% ch$default_year)
        if (ymin > ymax) { tmp <- ymin; ymin <- ymax; ymax <- tmp }
        return(c(ymin, ymax))
      }
      y <- as.integer(input$year_max %||% ch$default_year)
      c(y, y)
    })

    reporter_filter <- shiny::reactive({
      r <- input$reporter %||% "__ALL__"
      if (identical(r, "__ALL__") || !nzchar(r)) return(NULL)
      r
    })

    partner_filter <- shiny::reactive({
      p <- input$partner %||% "__ALL__"
      if (identical(p, "__ALL__") || !nzchar(p)) return(NULL)
      p
    })

    hs_filter <- shiny::reactive({
      h <- input$hs %||% "__ALL__"
      if (identical(h, "__ALL__") || !nzchar(h)) return(NULL)
      h
    })

    dependency_built <- shiny::reactive({
      d <- detailed_data()
      shiny::req(d)
      inc_perf_counter("dependency_build_count")
      yb <- year_bounds()
      construct_dependency_table(
        d,
        year_min = yb[1],
        year_max = yb[2],
        reporters = reporter_filter(),
        partners = partner_filter(),
        hs_codes = hs_filter(),
        min_link_value = input$min_value %||% 0,
        exclude_self = TRUE
      )
    })

    group_conc <- shiny::reactive({
      add_commodity_importance(supplier_concentration_by_group(dependency_built()$shares))
    })

    reporter_conc <- shiny::reactive({
      reporter_weighted_concentration(group_conc())
    })

    commodity_conc <- shiny::reactive({
      commodity_concentration_summary(group_conc())
    })

    supplier_exp <- shiny::reactive({
      supplier_exposure_summary(dependency_built()$shares)
    })

    rs_matrix <- shiny::reactive({
      top_n <- as.integer(input$top_n %||% 20L)
      build_reporter_supplier_matrix(
        dependency_built()$shares,
        metric = if (identical(input$metric, "import_value")) "import_value" else "partner_share",
        top_rows = top_n,
        top_cols = top_n
      )
    })

    cc_sparse <- shiny::reactive({
      build_country_commodity_sparse_matrix(
        dependency_built()$shares,
        max_nodes = min(as.integer(input$top_n %||% 20L), DEP_NODE_CAP),
        weight = if (identical(input$metric, "import_value")) "import_value" else "partner_share"
      )
    })

    output$kpi_strip <- shiny::renderUI({
      built <- dependency_built()
      shiny::req(built)
      gc <- group_conc()
      rc <- reporter_conc()
      c <- coverage()
      sp <- cc_sparse()
      shiny::div(
        class = "metric-grid dependency-kpi-strip",
        overview_metric_card(
          "Observed imports",
          format_trade_value_scaled(built$diagnostics$eligible_value %||% 0, input$scale %||% "auto")
        ),
        overview_metric_card(
          "Represented reporters",
          sprintf("%d / %d", c$represented_reporter_count %||% 0, c$selected_reporter_count %||% 0)
        ),
        overview_metric_card(
          "Eligible suppliers",
          format(data.table::uniqueN(built$shares$partner_iso3), big.mark = ",")
        ),
        overview_metric_card(
          "HS4 commodities",
          format(data.table::uniqueN(built$shares$hs_code), big.mark = ",")
        ),
        overview_metric_card(
          "Median weighted HHI",
          if (nrow(rc)) format_dependency_hhi(stats::median(rc$weighted_hhi, na.rm = TRUE)) else "Unavailable"
        ),
        overview_metric_card(
          "Matrix nodes",
          sprintf("%d (cap %d)", sp$n_nodes %||% 0L, DEP_NODE_CAP)
        )
      )
    })

    output$matrix_dim_text <- shiny::renderUI({
      mode <- input$matrix_mode %||% "reporter_supplier"
      if (identical(mode, "country_commodity")) {
        sp <- cc_sparse()
        shiny::p(
          class = "muted",
          sprintf(
            "Matrix dimension reflects active observed country-commodity nodes under the current filters: %d × %d (%d non-zero edges%s).",
            sp$n_nodes, sp$n_nodes, sp$n_edges,
            if (isTRUE(sp$capped)) paste0("; capped from ", sp$n_eligible_nodes_before_cap) else ""
          )
        )
      } else {
        m <- rs_matrix()
        shiny::p(
          class = "muted",
          sprintf(
            "Reporter × supplier matrix: %d × %d observed cells (not padded).",
            m$n_rows, m$n_cols
          )
        )
      }
    })

    output$heatmap <- plotly::renderPlotly({
      mode <- input$matrix_mode %||% "reporter_supplier"
      if (identical(mode, "country_commodity")) {
        country_commodity_heatmap(cc_sparse(), max_display = min(40L, as.integer(input$top_n %||% 20L)))
      } else {
        reporter_supplier_heatmap(rs_matrix())
      }
    })

    output$matrix_summary <- shiny::renderUI({
      shiny::p(
        class = "chart-summary",
        dependency_accessibility_summary(
          dependency_built(), group_conc(), reporter_conc(), coverage()
        )
      )
    })

    output$matrix_table <- DT::renderDT({
      mode <- input$matrix_mode %||% "reporter_supplier"
      if (identical(mode, "country_commodity")) {
        ed <- cc_sparse()$edges
        if (!nrow(ed)) return(DT::datatable(data.frame(Note = "No edges")))
        DT::datatable(
          ed[, .(from_node, to_node, hs_code, weight, partner_import_value)],
          rownames = FALSE,
          options = list(pageLength = 8, scrollX = TRUE),
          caption = "Sparse country-commodity dependency edges (table alternative)."
        )
      } else {
        long <- rs_matrix()$long
        if (!nrow(long)) return(DT::datatable(data.frame(Note = "No cells")))
        DT::datatable(
          long[, .(reporter_iso3, partner_iso3, value, partner_import_value, partner_share)],
          rownames = FALSE,
          options = list(pageLength = 8, scrollX = TRUE),
          caption = "Reporter × supplier matrix (table alternative)."
        )
      }
    })

    output$reporter_profile <- shiny::renderUI({
      built <- dependency_built()
      shiny::req(built)
      reps <- sort(unique(built$shares$reporter_iso3))
      if (!length(reps)) return(shiny::p(class = "muted", "No reporters in filter."))
      sel <- reporter_filter() %||% reps[1]
      if (!sel %in% reps) sel <- reps[1]
      shiny::tagList(
        shiny::selectInput(ns("profile_reporter"), "Reporter", choices = reps, selected = sel),
        shiny::uiOutput(ns("reporter_profile_body"))
      )
    })

    output$reporter_profile_body <- shiny::renderUI({
      iso <- input$profile_reporter %||% reporter_filter()
      shiny::req(iso)
      prof <- selected_reporter_profile(dependency_built()$shares, group_conc(), iso)
      if (is.null(prof)) return(shiny::p(class = "muted", "Profile unavailable."))
      w <- prof$weighted
      shiny::div(
        class = "dependency-profile-body",
        shiny::tags$h4(paste0(prof$reporter_name, " (", prof$reporter_iso3, ")")),
        shiny::tags$ul(
          shiny::tags$li(paste("Observed imports:", format_trade_value_scaled(prof$total_imports, input$scale %||% "auto"))),
          shiny::tags$li(paste("Partners:", prof$partner_count)),
          shiny::tags$li(paste("Commodities:", prof$commodity_count)),
          if (!is.null(w)) shiny::tagList(
            shiny::tags$li(paste("Weighted HHI:", format_dependency_hhi(w$weighted_hhi), "—", classify_hhi_band(w$weighted_hhi))),
            shiny::tags$li(paste("Weighted top-1 share:", format_dependency_share(w$weighted_top_1_share))),
            shiny::tags$li(paste("Weighted top-3 share:", format_dependency_share(w$weighted_top_3_share))),
            shiny::tags$li(paste("Effective supplier count:", format_dependency_hhi(w$effective_supplier_count, 2))),
            shiny::tags$li(paste("Most concentrated HS4:", w$most_concentrated_hs %||% "Unavailable")),
            shiny::tags$li(paste("Largest HS4:", w$largest_hs %||% "Unavailable"))
          ) else NULL
        ),
        shiny::p(class = "muted", "Does not imply firm-level substitutability or complete bilateral coverage.")
      )
    })

    output$commodity_profile <- shiny::renderUI({
      built <- dependency_built()
      shiny::req(built)
      hs <- sort(unique(built$shares$hs_code))
      if (!length(hs)) return(shiny::p(class = "muted", "No commodities in filter."))
      sel <- hs_filter() %||% hs[1]
      if (!sel %in% hs) sel <- hs[1]
      shiny::tagList(
        shiny::selectInput(ns("profile_hs"), "HS4", choices = hs, selected = sel),
        shiny::uiOutput(ns("commodity_profile_body"))
      )
    })

    output$commodity_profile_body <- shiny::renderUI({
      hs <- input$profile_hs %||% hs_filter()
      shiny::req(hs)
      prof <- selected_commodity_profile(dependency_built()$shares, group_conc(), hs)
      if (is.null(prof)) return(shiny::p(class = "muted", "Profile unavailable."))
      shiny::div(
        class = "dependency-commodity-body",
        shiny::tags$h4(paste0(prof$hs_code, " — ", prof$commodity_description %||% "")),
        shiny::tags$ul(
          shiny::tags$li(paste("Observed imports:", format_trade_value_scaled(prof$total_imports, input$scale %||% "auto"))),
          shiny::tags$li(paste("Represented reporters:", prof$reporter_count)),
          shiny::tags$li(paste("Supplier economies:", prof$partner_count))
        ),
        shiny::p(
          class = "muted",
          sprintf(
            "Partial coverage: %d of %d selected reporting economies.",
            coverage()$represented_reporter_count %||% 0,
            coverage()$selected_reporter_count %||% 0
          )
        ),
        shiny::p(class = "muted", "Not the complete global market structure for this HS4 heading.")
      )
    })

    output$scatter <- plotly::renderPlotly({
      concentration_scatter_plotly(group_conc(), scale = input$scale %||% "auto")
    })

    output$rank_plot <- plotly::renderPlotly({
      metric <- input$rank_metric %||% "top_1_share"
      ranked <- rank_dependency_groups(group_conc(), metric = metric, top_n = 15L)
      dependency_ranking_plotly(ranked, metric, paste("Top groups by", metric))
    })

    output$rank_summary <- shiny::renderUI({
      ranked <- rank_dependency_groups(group_conc(), input$rank_metric %||% "top_1_share", 1L)
      if (!nrow(ranked)) return(NULL)
      shiny::p(
        class = "chart-summary",
        sprintf(
          "Highest ranked group: %s · %s (%s).",
          ranked$reporter_iso3[1], ranked$hs_code[1],
          format_dependency_hhi(ranked[[input$rank_metric %||% "top_1_share"]][1])
        )
      )
    })

    output$supplier_table <- DT::renderDT({
      dt <- supplier_exp()
      if (!nrow(dt)) return(DT::datatable(data.frame(Note = "No suppliers")))
      DT::datatable(
        head(dt, 20L),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE),
        caption = "Supplier exposure within the represented import-reporting dataset (not supplier vulnerability)."
      )
    })

    output$trend_plot <- plotly::renderPlotly({
      d <- detailed_data()
      shiny::req(d)
      tr <- dependency_trend_by_year(
        d,
        reporters = reporter_filter(),
        hs_codes = hs_filter(),
        year_min = 2019L,
        year_max = 2024L
      )
      dependency_trend_plotly(tr)
    })

    output$detail_table <- DT::renderDT({
      built <- dependency_built()
      shiny::req(built)
      gc <- group_conc()
      dt <- merge(
        built$shares,
        gc[, .(reporter_iso3, hs_code, supplier_count, top_1_share, top_3_share,
               supplier_hhi, effective_supplier_count, commodity_import_share,
               concentrated_import_value, imports_pct_gdp)],
        by = c("reporter_iso3", "hs_code"),
        all.x = TRUE
      )
      keep <- c(
        "year_start", "year_end", "reporter_iso3", "reporter_name",
        "partner_iso3", "partner_name", "hs_code", "commodity_description",
        "partner_import_value", "reporter_commodity_total", "partner_share",
        "supplier_rank", "supplier_count", "top_1_share", "top_3_share",
        "supplier_hhi", "effective_supplier_count", "commodity_import_share"
      )
      if (isTRUE(input$table_technical)) {
        keep <- c(keep, "observation_count", "reporter_gdp_current_usd", "imports_pct_gdp",
                  "concentrated_import_value")
      }
      keep <- intersect(keep, names(dt))
      DT::datatable(
        dt[, keep, with = FALSE],
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE),
        caption = "Reporter–partner–HS4 dependency shares from reported imports only."
      )
    })

    output$coverage_panel <- shiny::renderUI({
      c <- coverage()
      built <- tryCatch(dependency_built(), error = function(e) NULL)
      sp <- tryCatch(cc_sparse(), error = function(e) NULL)
      excl <- built$diagnostics$excluded %||% data.table::data.table()
      shiny::tags$ul(
        class = "coverage-list",
        shiny::tags$li(paste("Production status:", c$production_status %||% "unknown")),
        shiny::tags$li(paste("Selected reporters:", c$selected_reporter_count %||% 0)),
        shiny::tags$li(paste("Represented reporters:", c$represented_reporter_count %||% 0)),
        shiny::tags$li(paste("Missing reporters:", paste(c$missing_reporters %||% character(), collapse = ", "))),
        shiny::tags$li(paste("Universe checksum:", c$universe_checksum %||% "Unavailable")),
        if (isTRUE(c$checksum_stale)) shiny::tags$li("Checksum differs from expected Phase-2 universe.") else NULL,
        shiny::tags$li(paste("Latest ingestion:", c$latest_ingested_at %||% "Unavailable")),
        if (!is.null(built)) {
          shiny::tagList(
            shiny::tags$li(sprintf(
              "Eligible import rows / value: %s / %s",
              format(built$diagnostics$eligible_rows %||% 0, big.mark = ","),
              format_trade_value_scaled(built$diagnostics$eligible_value %||% 0, "auto")
            )),
            shiny::tags$li(sprintf(
              "Share reconciliation: %s (max |error|=%s)",
              if (isTRUE(built$reconciliation$ok)) "PASS" else "FAIL",
              format_dependency_hhi(built$reconciliation$max_abs_error)
            )),
            if (nrow(excl)) {
              shiny::tags$li(paste(
                "Exclusions:",
                paste(sprintf("%s=%d", excl$reason, excl$n_rows), collapse = "; ")
              ))
            } else NULL,
            if (!is.null(sp)) {
              shiny::tags$li(sprintf(
                "Country-commodity sparse matrix: %d nodes, %d edges, density %s",
                sp$n_nodes, sp$n_edges, format_dependency_hhi(sp$density, 5)
              ))
            } else NULL
          )
        } else NULL,
        shiny::tags$li("Dependency uses each reporting economy’s own reported imports only."),
        shiny::tags$li("Exports are not used to fill imports; mirror data are not reconciled."),
        shiny::tags$li("Partner shares use observed eligible suppliers within the filtered detailed dataset."),
        shiny::tags$li("Missing unobserved suppliers may understate diversification."),
        shiny::tags$li("Domestic production, inventories and substitutability are outside scope."),
        shiny::tags$li("No indirect or second-order dependencies; no shock propagation in Phase 9."),
        shiny::tags$li("HHI uses a 0–1 scale. Effective supplier count = 1 / HHI."),
        shiny::tags$li(
          shiny::a("Open Data Quality", href = "#",
                   onclick = "document.querySelector('a[data-value=\"Data Quality\"]')?.click(); return false;")
        )
      )
    })

    year_label <- shiny::reactive({
      yb <- year_bounds()
      if (identical(yb[1], yb[2])) as.character(yb[1]) else paste0(yb[1], "_", yb[2])
    })

    output$dl_rs_matrix <- shiny::downloadHandler(
      filename = function() dependency_download_filename(
        "dependency_matrix_reporter_supplier", year_label()
      ),
      content = function(file) write_dependency_csv(rs_matrix()$long, file)
    )
    output$dl_cc_sparse <- shiny::downloadHandler(
      filename = function() dependency_download_filename(
        "dependency_matrix_country_commodity", year_label()
      ),
      content = function(file) write_dependency_sparse_csv(cc_sparse(), file)
    )
    output$dl_cc_mtx <- shiny::downloadHandler(
      filename = function() dependency_download_filename(
        "dependency_matrix_country_commodity", year_label(), ext = "mtx"
      ),
      content = function(file) write_dependency_mtx(cc_sparse(), file)
    )
    output$dl_detail <- shiny::downloadHandler(
      filename = function() dependency_download_filename("dependency_detail", year_label()),
      content = function(file) write_dependency_csv(dependency_built()$shares, file)
    )
    output$dl_reporter <- shiny::downloadHandler(
      filename = function() dependency_download_filename("dependency_reporter_concentration", year_label()),
      content = function(file) write_dependency_csv(reporter_conc(), file)
    )
    output$dl_commodity <- shiny::downloadHandler(
      filename = function() dependency_download_filename("dependency_commodity_concentration", year_label()),
      content = function(file) write_dependency_csv(commodity_conc(), file)
    )
    output$dl_supplier <- shiny::downloadHandler(
      filename = function() dependency_download_filename("supplier_exposure", year_label()),
      content = function(file) write_dependency_csv(supplier_exp(), file)
    )
    output$dl_diag <- shiny::downloadHandler(
      filename = function() dependency_download_filename("dependency_diagnostics", year_label()),
      content = function(file) write_dependency_csv(
        dependency_diagnostics_download(dependency_built(), cc_sparse(), coverage()),
        file
      )
    )
  })
}
