mod_network_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "network-page",
      shiny::div(
        class = "hero-panel",
        shiny::h2("Trade Network"),
        shiny::p(
          "Directed trade network from available detailed HS-85 observations. ",
          "Centrality and connectivity describe only the filtered currently available network — ",
          "not complete global HS-85 structure, and not supply-chain dependency scores."
        )
      ),
      shiny::uiOutput(ns("partial_notice")),
      shiny::uiOutput(ns("filter_bar")),
      shiny::uiOutput(ns("kpi_strip")),
      shiny::div(
        class = "network-grid",
        shiny::div(
          class = "chart-card chart-card-wide network-graph-card",
          shiny::h3(class = "chart-title", "Interactive network"),
          shiny::p(
            class = "muted centrality-note",
            "Centrality is calculated within the filtered and currently available network. ",
            "Values are not comparable across materially different filters without care."
          ),
          plotly::plotlyOutput(ns("network_plot"), height = "480px"),
          shiny::uiOutput(ns("network_summary"))
        ),
        shiny::div(
          class = "chart-card network-ranking-card",
          shiny::h3(class = "chart-title", "Centrality ranking"),
          shiny::selectInput(
            ns("rank_metric"), NULL,
            choices = network_centrality_rank_choices(),
            selected = "total_strength"
          ),
          plotly::plotlyOutput(ns("rank_plot"), height = "360px")
        ),
        shiny::div(
          class = "chart-card network-profile-card",
          shiny::h3(class = "chart-title", "Selected-node profile"),
          shiny::uiOutput(ns("node_profile"))
        ),
        shiny::div(
          class = "chart-card network-corridor-card",
          shiny::h3(class = "chart-title", "Top trade corridors"),
          shiny::uiOutput(ns("corridor_summary")),
          DT::DTOutput(ns("corridor_table"))
        ),
        shiny::div(
          class = "chart-card network-diagnostics-card",
          shiny::h3(class = "chart-title", "Structural diagnostics"),
          shiny::uiOutput(ns("diag_kpis")),
          plotly::plotlyOutput(ns("diag_plot"), height = "260px")
        ),
        shiny::div(
          class = "chart-card chart-card-wide network-tables-card",
          shiny::h3(class = "chart-title", "Node centrality table"),
          DT::DTOutput(ns("node_table")),
          shiny::h3(class = "chart-title", "Edge table"),
          DT::DTOutput(ns("edge_table"))
        ),
        shiny::div(
          class = "chart-card network-coverage-card",
          shiny::h3(class = "chart-title", "Coverage & methodology"),
          shiny::uiOutput(ns("coverage_panel"))
        ),
        shiny::div(
          class = "chart-card network-download-card",
          shiny::h3(class = "chart-title", "Downloads"),
          shiny::p(class = "muted", "Exports respect active filters. No secrets or filesystem paths."),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(ns("dl_nodes"), "Nodes CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_edges"), "Edges CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_rank"), "Ranking CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_corridors"), "Corridors CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_stats"), "Statistics CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_graphml"), "GraphML", class = "btn-sm btn-outline-primary")
          )
        )
      ),
      shiny::uiOutput(ns("empty_fallback"))
    )
  )
}

mod_network_server <- function(id, snap, cfg) {
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
        shiny::h3("Detailed trade network unavailable"),
        shiny::p("No detailed bilateral observations were found in the session snapshot. This page does not call external APIs.")
      )
    })

    output$partial_notice <- shiny::renderUI({
      c <- coverage()
      status_label <- if (coverage_is_selected_universe_complete(c)) "complete" else
        (c$production_status %||% "partial")
      shiny::tagList(
        shiny::div(
          class = "status-badge-row",
          status_badge(
            "Detailed network",
            status_label,
            label_override = sprintf(
              "%d/%d",
              c$represented_reporter_count %||% 0L,
              c$selected_reporter_count %||% 0L
            )
          )
        ),
        shiny::div(
          class = "partial-data-notice network-partial-notice",
          role = "status",
          shiny::tags$strong(detailed_coverage_notice(c, context = "network"))
        )
      )
    })

    output$filter_bar <- shiny::renderUI({
      d <- detailed_data()
      shiny::req(d)
      ch <- trade_flow_filter_choices(d, snap()$analytical_universe)
      yrs <- ch$years
      def_year <- ch$default_year
      focus_choices <- c("Entire available network" = "__ALL__", ch$reporter_labels)

      partner_focus <- ch$partner_labels
      focus_choices <- c(focus_choices, partner_focus[!partner_focus %in% focus_choices])

      shiny::div(
        class = "filter-toolbar network-toolbar",
        role = "search",
        `aria-label` = "Trade network filters",
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
          class = "filter-item",
          shiny::tags$label(`for` = ns("mode"), "Network mode"),
          shiny::selectInput(
            ns("mode"), NULL,
            choices = c(
              "Reported exports" = "exports",
              "Reported imports" = "imports",
              "Undirected observed trade" = "undirected"
            ),
            selected = NW_DEFAULT_MODE
          )
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("focus"), "Reporter / economy focus"),
          shiny::selectizeInput(ns("focus"), NULL, choices = focus_choices, selected = "__ALL__")
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("ego_order"), "Ego neighbourhood"),
          shiny::selectInput(
            ns("ego_order"), NULL,
            choices = c("Direct neighbours" = "1", "Up to two steps" = "2"),
            selected = "1"
          )
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("partners"), "Partners"),
          shiny::selectizeInput(
            ns("partners"), NULL,
            choices = c("All country partners" = "__ALL__", ch$partner_labels),
            selected = "__ALL__",
            multiple = TRUE,
            options = list(maxItems = 20)
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
          shiny::tags$label(`for` = ns("top_n"), "Top edges"),
          shiny::selectInput(
            ns("top_n"), NULL,
            choices = c("50" = "50", "100" = "100", "200" = "200", "300" = "300"),
            selected = as.character(NW_DEFAULT_TOP_EDGES)
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("size_metric"), "Node size"),
          shiny::selectInput(ns("size_metric"), NULL, choices = network_node_size_choices(),
                             selected = "total_strength")
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("colour_metric"), "Node colour"),
          shiny::selectInput(ns("colour_metric"), NULL, choices = network_node_colour_choices(),
                             selected = "reporting_status")
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("layout"), "Layout"),
          shiny::selectInput(ns("layout"), NULL, choices = network_layout_choices(), selected = "fr")
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
      mode <- as.character(input$year_mode %||% "latest")[1]
      if (identical(mode, "full")) {
        return(c(min(ch$years), max(ch$years)))
      }
      if (identical(mode, "custom")) {
        ymin <- as.integer(input$year_min %||% ch$default_year)[1]
        ymax <- as.integer(input$year_max %||% ch$default_year)[1]
        if (is.na(ymin)) ymin <- ch$default_year
        if (is.na(ymax)) ymax <- ch$default_year
        if (ymin > ymax) {
          tmp <- ymin; ymin <- ymax; ymax <- tmp
        }
        return(c(ymin, ymax))
      }
      y <- as.integer(input$year_max %||% ch$default_year)[1]
      if (is.na(y)) y <- ch$default_year
      c(y, y)
    })

    partner_filter <- shiny::reactive({
      p <- input$partners %||% "__ALL__"
      p <- as.character(p)
      if ("__ALL__" %in% p || !length(p)) return(NULL)
      setdiff(p, "__ALL__")
    })

    hs_filter <- shiny::reactive({
      h <- as.character(input$hs %||% "__ALL__")
      if (!length(h) || "__ALL__" %in% h) return(NULL)
      h <- h[nzchar(h)]
      if (!length(h)) return(NULL)
      h
    })

    network_result <- shiny::reactive({
      d <- detailed_data()
      shiny::req(d)
      inc_perf_counter("graph_build_count")
      c <- coverage()
      yb <- year_bounds()
      focus <- as.character(input$focus %||% "__ALL__")[1]
      uni <- snap()$analytical_universe
      build_full_trade_network(
        d,
        mode = as.character(input$mode %||% NW_DEFAULT_MODE)[1],
        year_min = yb[1],
        year_max = yb[2],
        partners = partner_filter(),
        hs_codes = hs_filter(),
        top_n = as.integer(input$top_n %||% NW_DEFAULT_TOP_EDGES)[1],
        focus_iso3 = if (identical(focus, "__ALL__")) NULL else focus,
        ego_order = as.integer(input$ego_order %||% 1L)[1],
        layout = as.character(input$layout %||% "fr")[1],
        selected_reporters = c$selected_reporters %||% universe_iso3_from_snap(uni),
        selected_partners = partner_iso3_from_universe(uni),
        represented_reporters = c$represented_reporters %||% character()
      )
    })

    selected_iso <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$focus, {
      f <- input$focus
      if (!is.null(f) && !identical(f, "__ALL__")) selected_iso(f)
    }, ignoreInit = TRUE)

    output$kpi_strip <- shiny::renderUI({
      net <- network_result()
      shiny::req(net)
      k <- network_kpi_summary(net$built, net$stats, coverage())
      shiny::div(
        class = "metric-grid network-kpi-strip",
        overview_metric_card("Nodes", format(k$nodes, big.mark = ",")),
        overview_metric_card("Visible edges", format(k$edges, big.mark = ",")),
        overview_metric_card(
          "Visible trade",
          format_trade_value_scaled(k$visible_value, input$scale %||% "auto")
        ),
        overview_metric_card(
          "Value coverage",
          if (is.finite(k$coverage_pct)) sprintf("%.1f%%", k$coverage_pct) else "Unavailable"
        ),
        overview_metric_card(
          "Density",
          if (is.finite(k$density)) sprintf("%.4f", k$density) else "Unavailable"
        ),
        overview_metric_card(
          "Coverage",
          sprintf("%d/%d %s", k$represented %||% 0, k$selected %||% 0, k$status %||% "")
        )
      )
    })

    output$network_plot <- plotly::renderPlotly({
      net <- network_result()
      shiny::req(net)
      network_plotly(
        net,
        size_metric = input$size_metric %||% "total_strength",
        colour_metric = input$colour_metric %||% "reporting_status",
        scale = input$scale %||% "auto",
        selected_iso3 = selected_iso(),
        title = paste0(
          network_mode_label(input$mode %||% NW_DEFAULT_MODE),
          " — available observations"
        )
      )
    })

    output$network_summary <- shiny::renderUI({
      net <- network_result()
      shiny::req(net)
      shiny::tagList(
        shiny::p(
          class = "chart-summary",
          network_accessibility_summary(
            net$built, net$stats, net$nodes, input$mode %||% NW_DEFAULT_MODE
          )
        ),
        shiny::p(
          class = "muted",
          sprintf(
            "Eligible edges: %d · Visible: %d · Self-edges excluded: %d (value %s)",
            net$built$eligible_n %||% 0L,
            net$built$visible_n %||% 0L,
            net$built$self_excluded_count %||% 0L,
            format_trade_value_scaled(net$built$self_excluded_value %||% 0, input$scale %||% "auto")
          )
        )
      )
    })

    output$rank_plot <- plotly::renderPlotly({
      net <- network_result()
      shiny::req(net)
      metric <- input$rank_metric %||% "total_strength"
      ranked <- rank_network_nodes(net$nodes, metric, top_n = 15L)
      centrality_ranking_plotly(
        ranked, metric = metric, scale = input$scale %||% "auto",
        title = paste("Top nodes by", metric)
      )
    })

    output$node_profile <- shiny::renderUI({
      net <- network_result()
      shiny::req(net)
      choices <- setNames(
        net$nodes$iso3,
        paste0(net$nodes$display_name, " (", net$nodes$iso3, ")")
      )
      sel <- selected_iso()
      if (is.null(sel) || !sel %in% net$nodes$iso3) {
        sel <- if (nrow(net$nodes)) {
          rank_network_nodes(net$nodes, "total_strength", 1L)$iso3[1]
        } else NULL
      }
      shiny::tagList(
        shiny::selectInput(ns("profile_iso"), "Node", choices = choices, selected = sel),
        shiny::uiOutput(ns("profile_body"))
      )
    })

    shiny::observeEvent(input$profile_iso, {
      if (!is.null(input$profile_iso)) selected_iso(input$profile_iso)
    })

    output$profile_body <- shiny::renderUI({
      net <- network_result()
      shiny::req(net)
      iso <- input$profile_iso %||% selected_iso()
      shiny::req(iso)
      yb <- year_bounds()
      prof <- selected_node_profile(
        net$nodes, net$edges, iso,
        detailed = detailed_data(),
        year_min = yb[1], year_max = yb[2]
      )
      if (is.null(prof)) {
        return(shiny::p(class = "muted", "Select a node to view its profile."))
      }
      shiny::div(
        class = "node-profile-body",
        shiny::tags$h4(paste0(prof$display_name, " (", prof$iso3, ")")),
        shiny::tags$p(shiny::tags$strong(prof$reporting_status_label)),
        shiny::tags$ul(
          shiny::tags$li(paste("Total strength:", format_trade_value_scaled(prof$total_strength, input$scale %||% "auto"))),
          shiny::tags$li(paste("Inbound:", format_trade_value_scaled(prof$in_strength, input$scale %||% "auto"))),
          shiny::tags$li(paste("Outbound:", format_trade_value_scaled(prof$out_strength, input$scale %||% "auto"))),
          shiny::tags$li(paste("Degree:", prof$degree)),
          shiny::tags$li(paste("PageRank:", format_network_metric(prof$pagerank))),
          shiny::tags$li(paste("Betweenness:", format_network_metric(prof$betweenness))),
          shiny::tags$li(paste("Community:", prof$community %||% "Unavailable")),
          shiny::tags$li(paste("Component:", prof$component_id %||% "Unavailable")),
          shiny::tags$li(paste(
            "GDP:",
            if (is.finite(prof$gdp_current_usd)) format_trade_value_scaled(prof$gdp_current_usd, "billions")
            else "Unavailable"
          ))
        ),
        if (!isTRUE(prof$represented_as_reporter)) {
          shiny::p(
            class = "muted",
            "This node is observed as a partner in the available network and does not imply complete reporter coverage."
          )
        } else NULL
      )
    })

    output$corridor_summary <- shiny::renderUI({
      net <- network_result()
      shiny::req(net)
      top <- rank_network_corridors(net$edges, top_n = 1L)
      if (!nrow(top)) return(shiny::p(class = "muted", "No corridors available."))
      shiny::p(
        class = "chart-summary",
        sprintf(
          "Largest corridor: %s → %s (%s, %.1f%% of visible network value). Mode: %s.",
          top$from_iso3[1], top$to_iso3[1],
          format_trade_value_scaled(top$trade_value_usd[1], input$scale %||% "auto"),
          top$share_pct[1],
          network_mode_label(input$mode %||% NW_DEFAULT_MODE)
        )
      )
    })

    output$corridor_table <- DT::renderDT({
      net <- network_result()
      shiny::req(net)
      dt <- rank_network_corridors(net$edges, top_n = 20L)
      if (!nrow(dt)) return(DT::datatable(data.frame(Note = "No edges")))
      out <- dt[, .(
        from = from_iso3, to = to_iso3,
        trade_value_usd, observation_count, share_pct,
        year_start, year_end, flow_mode, hs_scope
      )]
      DT::datatable(
        out,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE),
        caption = "Top corridors in the filtered available-observation network."
      )
    })

    output$diag_kpis <- shiny::renderUI({
      net <- network_result()
      shiny::req(net)
      s <- net$stats
      shiny::tags$ul(
        class = "coverage-list",
        shiny::tags$li(sprintf("Weak components: %s", s$weak_component_count %||% "Unavailable")),
        shiny::tags$li(sprintf(
          "Largest component share: %s",
          format_network_share(s$largest_weak_component_share_pct)
        )),
        shiny::tags$li(sprintf("Reciprocity: %s", format_network_metric(s$reciprocity))),
        shiny::tags$li(sprintf("Top-5 edge share: %s", format_network_share(s$top5_edge_share_pct))),
        shiny::tags$li(sprintf("Top-5 node-strength share: %s", format_network_share(s$top5_node_strength_share_pct))),
        shiny::tags$li(sprintf("Edge HHI: %s", format_network_metric(s$edge_hhi))),
        shiny::tags$li(sprintf("Node-strength HHI: %s", format_network_metric(s$node_strength_hhi)))
      )
    })

    output$diag_plot <- plotly::renderPlotly({
      net <- network_result()
      shiny::req(net)
      network_diagnostics_plotly(net$nodes, net$edges, which = "edges")
    })

    output$node_table <- DT::renderDT({
      net <- network_result()
      shiny::req(net)
      dt <- data.table::copy(net$nodes)
      keep <- intersect(
        c("iso3", "display_name", "reporting_status", "degree", "in_degree", "out_degree",
          "total_strength", "in_strength", "out_strength", "pagerank", "betweenness",
          "community", "component_id", "gdp_current_usd", "population_total"),
        names(dt)
      )
      DT::datatable(
        dt[, keep, with = FALSE],
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE, serverSide = FALSE),
        caption = "Node metrics for the filtered available-observation network."
      )
    })

    output$edge_table <- DT::renderDT({
      net <- network_result()
      shiny::req(net)
      dt <- data.table::copy(net$edges)
      tot <- sum(dt$trade_value_usd, na.rm = TRUE)
      dt[, share_pct := if (tot > 0) 100 * trade_value_usd / tot else NA_real_]
      keep <- intersect(
        c("from_iso3", "to_iso3", "trade_value_usd", "observation_count", "share_pct",
          "year_start", "year_end", "flow_mode", "hs_scope"),
        names(dt)
      )
      DT::datatable(
        dt[, keep, with = FALSE],
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE),
        caption = "Aggregated edges after threshold / top-N control."
      )
    })

    output$coverage_panel <- shiny::renderUI({
      c <- coverage()
      net <- tryCatch(network_result(), error = function(e) NULL)
      shiny::tags$ul(
        class = "coverage-list",
        shiny::tags$li(paste("Production status:", c$production_status %||% "unknown")),
        shiny::tags$li(paste("Selected reporters:", c$selected_reporter_count %||% 0L)),
        shiny::tags$li(paste("Represented reporters:", c$represented_reporter_count %||% 0L)),
        shiny::tags$li(paste("Missing reporters:", c$missing_reporter_count %||% 0L)),
        shiny::tags$li(paste("Represented:", paste(c$represented_reporters %||% character(), collapse = ", "))),
        shiny::tags$li(paste("Missing:", paste(c$missing_reporters %||% character(), collapse = ", "))),
        shiny::tags$li(paste("Universe checksum:", c$universe_checksum %||% "Unavailable")),
        if (isTRUE(c$checksum_stale)) shiny::tags$li("Checksum differs from expected Phase-2 universe.") else NULL,
        shiny::tags$li(paste("Latest ingestion:", c$latest_ingested_at %||% "Unavailable")),
        shiny::tags$li(paste("Validation warnings:", c$validation_warnings %||% 0L)),
        if (!is.null(net)) {
          shiny::tagList(
            shiny::tags$li(paste("Flow mode:", network_mode_label(input$mode %||% NW_DEFAULT_MODE))),
            shiny::tags$li(paste("HS scope:", net$built$hs_scope %||% "all_hs4")),
            shiny::tags$li(sprintf(
              "Eligible / visible edges: %d / %d",
              net$built$eligible_n %||% 0L, net$built$visible_n %||% 0L
            )),
            shiny::tags$li(sprintf(
              "Visible value coverage: %s",
              format_network_share(net$built$coverage_pct)
            )),
            shiny::tags$li(sprintf(
              "Self-edges excluded: %d (value %s)",
              net$built$self_excluded_count %||% 0L,
              format_trade_value_scaled(net$built$self_excluded_value %||% 0, "auto")
            ))
          )
        } else NULL,
        shiny::tags$li("Reported exports: reporter → partner. Reported imports: partner → reporter."),
        shiny::tags$li("Imports and exports are not merged by default (mirror overlap risk)."),
        shiny::tags$li("Betweenness uses distance = inverse scaled trade value (not physical distance)."),
        shiny::tags$li("PageRank indicates prominence in the observed network only."),
        shiny::tags$li("Concentration metrics are not dependency scores (Phase 9)."),
        shiny::tags$li(
          shiny::a("Open Data Quality", href = "#", onclick = "document.querySelector('a[data-value=\"Data Quality\"]')?.click(); return false;")
        )
      )
    })

    year_label <- shiny::reactive({
      yb <- year_bounds()
      if (identical(yb[1], yb[2])) as.character(yb[1]) else paste0(yb[1], "_", yb[2])
    })

    output$dl_nodes <- shiny::downloadHandler(
      filename = function() network_download_filename(
        "trade_network_nodes", input$mode %||% "exports", year_label()
      ),
      content = function(file) write_network_csv(network_result()$nodes, file)
    )
    output$dl_edges <- shiny::downloadHandler(
      filename = function() network_download_filename(
        "trade_network_edges", input$mode %||% "exports", year_label()
      ),
      content = function(file) write_network_csv(network_result()$edges, file)
    )
    output$dl_rank <- shiny::downloadHandler(
      filename = function() network_download_filename(
        "trade_network_ranking", input$rank_metric %||% "total_strength", year_label()
      ),
      content = function(file) {
        net <- network_result()
        write_network_csv(
          rank_network_nodes(net$nodes, input$rank_metric %||% "total_strength", 50L),
          file
        )
      }
    )
    output$dl_corridors <- shiny::downloadHandler(
      filename = function() network_download_filename(
        "trade_network_corridors", input$mode %||% "exports", year_label()
      ),
      content = function(file) write_network_csv(
        rank_network_corridors(network_result()$edges, 50L), file
      )
    )
    output$dl_stats <- shiny::downloadHandler(
      filename = function() network_download_filename(
        "trade_network_stats", input$mode %||% "exports", year_label()
      ),
      content = function(file) {
        net <- network_result()
        write_network_csv(
          network_stats_download_dt(net$stats, net$built, coverage()),
          file
        )
      }
    )
    output$dl_graphml <- shiny::downloadHandler(
      filename = function() {
        hs <- hs_filter() %||% "all"
        network_download_filename(
          "trade_network", hs, year_label(), ext = "graphml"
        )
      },
      content = function(file) write_network_graphml(network_result()$graph, file)
    )
  })
}
