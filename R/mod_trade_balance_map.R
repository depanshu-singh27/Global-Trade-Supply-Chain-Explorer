mod_trade_balance_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "trade-map-page",
      shiny::div(
        class = "hero-panel",
        shiny::h2("Trade Balance Map"),
        shiny::p(
          "Choropleth of reporter-based HS Chapter 85 World-partner country totals. ",
          "Global country-level trade coverage is complete; macro indicators may be missing; ",
          "detailed bilateral analyses elsewhere use the selected analytical-universe cube."
        )
      ),
      shiny::uiOutput(ns("status_strip")),
      shiny::uiOutput(ns("filter_bar")),
      shiny::uiOutput(ns("kpi_strip")),
      shiny::div(
        class = "trade-map-grid",
        shiny::div(
          class = "chart-card map-card",
          shiny::h3(class = "chart-title", "Geographic choropleth"),
          shiny::div(
            class = "map-actions",
            shiny::actionButton(ns("clear_country"), "Clear selection", class = "btn-sm btn-outline-secondary"),
            shiny::actionButton(ns("reset_view"), "Reset map view", class = "btn-sm btn-outline-secondary")
          ),
          shiny::tags$script(shiny::HTML(
            "Shiny.addCustomMessageHandler('trade-map-selection-style', function(message) {
              function applySelectionStyle() {
                var widget = window.HTMLWidgets && window.HTMLWidgets.find('#' + message.mapId);
                var map = widget && (widget.getMap ? widget.getMap() : widget);
                if (!map || !map._layers) return;
                Object.keys(map._layers).forEach(function(key) {
                  var layer = map._layers[key];
                  if (!layer || !layer.setStyle || !layer.options || !layer.options.layerId) return;
                  var selected = message.iso && layer.options.layerId === message.iso;
                  layer.setStyle({
                    weight: selected ? 2.8 : 0.7,
                    color: selected ? '#1B2A3B' : message.defaultBorder,
                    opacity: 1,
                    fillOpacity: selected ? 0.9 : 0.82
                  });
                  if (selected && layer.bringToFront) layer.bringToFront();
                });
              }
              applySelectionStyle();
              window.setTimeout(applySelectionStyle, 100);
              window.setTimeout(applySelectionStyle, 500);
            });"
          )),
          leaflet::leafletOutput(ns("choropleth"), height = "420px"),
          shiny::uiOutput(ns("map_summary"))
        ),
        shiny::div(
          class = "chart-card country-profile-card",
          shiny::h3(class = "chart-title", "Selected economy profile"),
          shiny::uiOutput(ns("country_profile"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Trend"),
          plotly::plotlyOutput(ns("trend"), height = "300px"),
          shiny::uiOutput(ns("trend_summary"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Largest reported surpluses"),
          plotly::plotlyOutput(ns("surplus_chart"), height = "300px")
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Largest reported deficits"),
          plotly::plotlyOutput(ns("deficit_chart"), height = "300px")
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Accessible country table"),
          shiny::uiOutput(ns("a11y_summary")),
          DT::DTOutput(ns("map_table"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Geographic coverage"),
          shiny::uiOutput(ns("coverage_panel"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Downloads"),
          shiny::p(class = "muted", "Exports respect active filters. No secrets or filesystem paths."),
          shiny::div(
            class = "download-row",
            shiny::downloadButton(ns("dl_map"), "Map dataset CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_trend"), "Country trend CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_surplus"), "Surplus ranking CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_deficit"), "Deficit ranking CSV", class = "btn-sm btn-outline-primary"),
            shiny::downloadButton(ns("dl_geo"), "Geography diagnostics CSV", class = "btn-sm btn-outline-primary")
          )
        )
      ),
      shiny::uiOutput(ns("empty_fallback"))
    )
  )
}

mod_trade_balance_map_server <- function(id, snap, cfg, active_nav = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_iso <- shiny::reactiveVal(NULL)

    analytics <- shiny::reactive({
      s <- snap()
      s$map_analytics %||% s$country_year_analytics
    })

    geometry <- shiny::reactive(snap()$map_geometry)

    output$empty_fallback <- shiny::renderUI({
      if (!is.null(analytics()) && nrow(analytics()) > 0) return(NULL)
      shiny::div(
        class = "empty-state",
        role = "status",
        shiny::h3("Map analytics unavailable"),
        shiny::p("Country-year analytics were not found. Global trade and Phase 3 enrichment must be available locally."),
        shiny::p(class = "muted", "This page does not call external APIs.")
      )
    })

    output$status_strip <- shiny::renderUI({
      s <- snap()
      shiny::div(
        class = "status-badge-row map-status-strip",
        status_badge("Global trade", s$pipeline_status$global_trade %||% "complete"),
        status_badge("Macro", s$pipeline_status$macro %||% "complete"),
        status_badge("Detailed bilateral", s$pipeline_status$detailed_trade %||% "partial"),
        shiny::span(
          class = "method-note",
          "Map uses completed World-partner country totals — not bilateral dependency flows."
        )
      )
    })

    output$filter_bar <- shiny::renderUI({
      cy <- analytics()
      shiny::req(cy)
      years <- map_year_choices(cy)
      def_y <- choose_map_default_year(cy)
      labs <- unique(prepare_map_analytics(cy)[, .(reporter_iso3, reporter_name)])
      labs <- labs[order(reporter_name)]
      country_choices <- c("No selection" = "", setNames(
        labs$reporter_iso3,
        paste0(labs$reporter_name, " (", labs$reporter_iso3, ")")
      ))
      regions <- c("All regions" = "__ALL__")
      g <- geometry()
      if (!is.null(g) && inherits(g, "sf")) {
        rb <- sort(unique(stats::na.omit(as.character(g$region_wb))))
        rb <- rb[nzchar(rb) & tolower(rb) != "antarctica"]
        regions <- c(regions, setNames(rb, rb))
      }
      shiny::div(
        class = "filter-toolbar map-toolbar",
        role = "search",
        `aria-label` = "Trade balance map filters",
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("year"), "Year"),
          shiny::selectInput(ns("year"), NULL, choices = years, selected = def_y)
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("metric"), "Metric"),
          shiny::selectInput(
            ns("metric"), NULL,
            choices = c(
              "Trade balance" = "trade_balance",
              "Imports" = "imports",
              "Exports" = "exports",
              "Total trade" = "total_trade",
              "Total trade / GDP" = "total_trade_pct_gdp",
              "Trade balance / GDP" = "balance_pct_gdp",
              "Total trade per capita" = "trade_per_capita"
            ),
            selected = "trade_balance"
          )
        ),
        shiny::div(
          class = "filter-item",
          shiny::tags$label(`for` = ns("classify"), "Classification"),
          shiny::selectInput(
            ns("classify"), NULL,
            choices = c(
              "Continuous" = "continuous",
              "Quantile" = "quantile",
              "Fixed symmetric (balance)" = "fixed_symmetric"
            ),
            selected = "continuous"
          )
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("country"), "Country / economy"),
          shiny::selectizeInput(ns("country"), NULL, choices = country_choices, selected = "")
        ),
        shiny::div(
          class = "filter-item filter-item-wide",
          shiny::tags$label(`for` = ns("region"), "Region (Natural Earth WB regions)"),
          shiny::selectInput(ns("region"), NULL, choices = regions, selected = "__ALL__")
        )
      )
    })

    shiny::observeEvent(input$country, {
      v <- input$country
      if (is.null(v) || !nzchar(v)) selected_iso(NULL) else selected_iso(as.character(v))
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$clear_country, {
      selected_iso(NULL)

      shiny::updateSelectizeInput(session, "country", selected = "")
    }, ignoreInit = TRUE)

    year_dt <- shiny::reactive({
      cy <- analytics()
      shiny::req(cy)
      filter_map_year(
        cy,
        year = as.integer(input$year %||% choose_map_default_year(cy)),
        region = input$region %||% "__ALL__",
        geometry = geometry()
      )
    })

    joined <- shiny::reactive({
      join_map_data(year_dt(), geometry(), input$metric %||% "trade_balance")
    })

    coverage <- shiny::reactive({
      mapped_value_coverage(year_dt(), joined()$sf, input$metric %||% "trade_balance")
    })

    color_meta <- shiny::reactive({
      build_map_color_meta(
        map_metric_values(year_dt(), input$metric %||% "trade_balance"),
        metric = input$metric %||% "trade_balance",
        method = input$classify %||% "continuous"
      )
    })

    output$kpi_strip <- shiny::renderUI({
      cy <- analytics()
      if (is.null(cy) || !nrow(cy)) return(NULL)
      iso <- selected_iso()
      if (!is.null(iso) && nzchar(iso)) {
        p <- selected_country_profile(cy, iso, as.integer(input$year))
        if (!isTRUE(p$available)) {
          return(shiny::div(class = "empty-state", "No observations for the selected economy and year."))
        }
        shiny::div(
          class = "metric-grid overview-kpi-grid",
          overview_metric_card("Total trade", format_usd_compact(p$total_trade), "Current US$", p$reporter_name),
          overview_metric_card("Imports", format_usd_compact(p$imports), "Current US$", NULL),
          overview_metric_card("Exports", format_usd_compact(p$exports), "Current US$", NULL),
          overview_metric_card("Trade balance", format_usd_compact(p$trade_balance), "Current US$", "Exports − imports"),
          overview_metric_card("Trade / GDP", format_pct(p$trade_pct_gdp), "Share", "Unavailable if GDP missing"),
          overview_metric_card("Trade per capita", format_per_person(p$trade_per_capita), "Current US$", NULL)
        )
      } else {
        k <- map_kpi_summary(year_dt(), coverage())
        shiny::div(
          class = "metric-grid overview-kpi-grid",
          overview_metric_card("Mapped HS-85 trade", format_usd_compact(k$total_trade), "Current US$", k$scope_note),
          overview_metric_card("Imports", format_usd_compact(k$imports), "Current US$", NULL),
          overview_metric_card("Exports", format_usd_compact(k$exports), "Current US$", NULL),
          overview_metric_card("Aggregate balance", format_usd_compact(k$trade_balance), "Current US$", "Neutral interpretation"),
          overview_metric_card("Economies", format_count(k$n_economies), "With observations", NULL),
          overview_metric_card(
            "Surplus / deficit",
            paste0(format_count(k$n_surplus), " / ", format_count(k$n_deficit)),
            "Counts",
            paste0("Mapped-value coverage: ", format_pct(k$mapped_coverage_pct))
          )
        )
      }
    })

    output$choropleth <- leaflet::renderLeaflet({

      if (!is.null(active_nav)) {
        shiny::req(identical(active_nav(), "Trade Balance Map"))
      }
      shiny::req(input$year, input$metric, input$classify)
      j <- joined()
      shiny::req(j$sf)
      meta <- color_meta()
      sfobj <- j$sf
      if (!"map_value" %in% names(sfobj)) sfobj$map_value <- NA_real_
      pal <- leaflet_color_fun(meta)
      sfobj$map_value_plot <- clamp_map_values_to_domain(sfobj$map_value, meta$domain)

      sfobj$polygon_layer_id <- as.character(sfobj$map_iso3)
      sfobj$border_weight <- 0.7
      sfobj$border_color <- map_polygon_border()
      labels <- lapply(seq_len(nrow(sfobj)), function(i) {
        nm <- sfobj$map_name[i] %||% sfobj$reporter_name[i] %||% sfobj$map_iso3[i]
        shiny::HTML(paste0(
          "<strong>", nm, " (", sfobj$map_iso3[i], ")</strong><br/>",
          "Year: ", input$year, "<br/>",
          map_metric_label(input$metric %||% "trade_balance"), ": ",
          format_map_metric_value(sfobj$map_value[i], input$metric %||% "trade_balance"), "<br/>",
          "Imports: ", format_usd_compact(sfobj$imports_value_usd[i]), "<br/>",
          "Exports: ", format_usd_compact(sfobj$exports_value_usd[i])
        ))
      })
      leaflet::leaflet(
        sfobj,
        options = leaflet::leafletOptions(
          worldCopyJump = FALSE,
          minZoom = 1
        )
      ) |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron,
          options = leaflet::providerTileOptions(noWrap = TRUE)
        ) |>
        leaflet::addPolygons(
          layerId = ~polygon_layer_id,
          fillColor = ~pal(map_value_plot),
          weight = ~border_weight,
          color = ~border_color,
          fillOpacity = 0.82,
          opacity = 1,
          label = labels,
          group = "choropleth",
          highlightOptions = leaflet::highlightOptions(
            weight = 3, color = "#1B2A3B", bringToFront = TRUE, fillOpacity = 0.9
          )
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          pal = pal,
          values = ~map_value_plot,
          title = map_metric_label(input$metric %||% "trade_balance"),
          opacity = 0.9,
          na.label = "No value available"
        ) |>
        leaflet::setMaxBounds(-180, -60, 180, 85) |>
        leaflet::fitBounds(-180, -60, 180, 85)
    })

    shiny::observeEvent(input$choropleth_shape_click, {
      click <- input$choropleth_shape_click
      iso <- as.character(click$id %||% "")[1]
      if (!nzchar(iso)) return()
      valid <- year_dt()$reporter_iso3
      if (!iso %in% as.character(valid)) return()
      selected_iso(iso)
      shiny::updateSelectizeInput(session, "country", selected = iso)
    }, ignoreInit = TRUE)

    shiny::observe({
      input$year
      input$metric
      input$classify
      input$region
      iso <- selected_iso() %||% ""
      session$sendCustomMessage(
        "trade-map-selection-style",
        list(
          mapId = ns("choropleth"),
          iso = as.character(iso),
          defaultBorder = map_polygon_border()
        )
      )
    })

    shiny::observeEvent(input$reset_view, {
      leaflet::leafletProxy("choropleth", session) |>
        leaflet::fitBounds(-180, -60, 180, 85)
    }, ignoreInit = TRUE)

    output$map_summary <- shiny::renderUI({
      shiny::p(
        class = "chart-summary",
        paste0(
          map_accessibility_summary(year_dt(), input$metric %||% "trade_balance", selected_iso()),
          " Missing metric values use a neutral fill and are labelled “No value available”, not zero. ",
          "Surplus/deficit colours are descriptive only."
        )
      )
    })

    output$country_profile <- shiny::renderUI({
      iso <- selected_iso()
      if (is.null(iso) || !nzchar(iso)) {
        return(shiny::p(
          class = "muted",
          "Select an economy on the map or from the country control to view the profile. ",
          shiny::tags$span(
            title = "Values are current US dollars. GDP-normalised metrics require positive GDP. Totals are reporter-based World-partner HS-85 trade. Balance = exports − imports.",
            "(Methodology)"
          )
        ))
      }
      p <- selected_country_profile(analytics(), iso, as.integer(input$year))
      if (!isTRUE(p$available)) {
        return(shiny::p("No profile for this selection."))
      }
      shiny::tagList(
        shiny::h4(paste0(p$reporter_name, " (", p$reporter_iso3, ") — ", p$year)),
        shiny::tags$ul(
          class = "coverage-list",
          shiny::tags$li(paste0("GDP: ", format_usd_compact(p$gdp))),
          shiny::tags$li(paste0("Population: ", format_count(p$population))),
          shiny::tags$li(paste0("GDP per capita: ", format_usd_compact(p$gdp_per_capita))),
          shiny::tags$li(paste0("Inflation: ", format_pct(p$inflation))),
          shiny::tags$li(paste0("Trade / GDP: ", format_pct(p$trade_pct_gdp))),
          shiny::tags$li(paste0("Balance / GDP: ", format_pct(p$balance_pct_gdp))),
          shiny::tags$li(paste0("Trade per capita: ", format_per_person(p$trade_per_capita))),
          shiny::tags$li(paste0("YoY total trade: ", format_yoy_delta(p$yoy_total_pct)))
        ),
        shiny::p(
          class = "method-note",
          "Current US$; GDP-normalised only with valid GDP; reporter World-partner totals; balance = exports − imports."
        )
      )
    })

    output$trend <- plotly::renderPlotly({
      iso <- selected_iso()
      p <- gte_palette()
      if (is.null(iso) || !nzchar(iso)) {
        ser <- prepare_global_trend(analytics(), input$region %||% "__ALL__", geometry())
        title <- "Aggregated World-partner totals over time"
      } else {
        ser <- prepare_country_trend(analytics(), iso)
        title <- paste0("Trade trend — ", iso)
      }
      if (!nrow(ser)) return(overview_empty_plotly("No trend data."))
      df <- as.data.frame(ser)
      plotly::plot_ly(df, x = ~year) |>
        plotly::add_trace(y = ~imports, name = "Imports", type = "scatter", mode = "lines+markers",
                          line = list(color = p$imports)) |>
        plotly::add_trace(y = ~exports, name = "Exports", type = "scatter", mode = "lines+markers",
                          line = list(color = p$exports)) |>
        plotly::add_trace(y = ~total, name = "Total", type = "scatter", mode = "lines+markers",
                          line = list(color = p$total, dash = "dot")) |>
        plotly::add_trace(y = ~balance, name = "Balance", type = "scatter", mode = "lines+markers",
                          line = list(color = p$balance_neg), yaxis = "y2") |>
        plotly::layout(
          title = list(text = title, font = list(size = 14, color = p$ink)),
          yaxis = list(title = "Current US$", gridcolor = p$grid),
          yaxis2 = list(overlaying = "y", side = "right", title = "Balance", zeroline = TRUE,
                        zerolinecolor = p$zero, showgrid = FALSE),
          legend = list(orientation = "h", y = -0.2),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    })

    output$trend_summary <- shiny::renderUI({
      shiny::p(
        class = "chart-summary",
        "Missing years are not interpolated. Balance uses a secondary axis with a zero line. Values are current US dollars."
      )
    })

    rank_plot <- function(ranked, title, surplus = TRUE) {
      p <- gte_palette()
      if (!nrow(ranked)) return(overview_empty_plotly("No economies in this ranking."))
      df <- as.data.frame(ranked)
      df$label <- paste0(df$reporter_name, " (", df$reporter_iso3, ")")
      df <- df[order(df$trade_balance_usd, df$reporter_iso3), , drop = FALSE]
      plotly::plot_ly(
        df,
        x = ~trade_balance_usd,
        y = ~factor(label, levels = label),
        type = "bar", orientation = "h",
        marker = list(color = if (surplus) p$surplus else p$deficit),
        hovertemplate = "%{y}<br>%{customdata}<extra></extra>",
        customdata = format_usd_compact(df$trade_balance_usd)
      ) |>
        plotly::layout(
          title = list(text = title, font = list(size = 13, color = p$ink)),
          xaxis = list(title = "Current US$", zeroline = TRUE, zerolinecolor = p$zero),
          yaxis = list(title = "", automargin = TRUE),
          margin = list(l = 120, t = 40, b = 40),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          showlegend = FALSE
        ) |>
        plotly::config(displayModeBar = FALSE, responsive = TRUE)
    }

    output$surplus_chart <- plotly::renderPlotly({
      rank_plot(map_surplus_ranking(year_dt()), "Largest reported surpluses", TRUE)
    })
    output$deficit_chart <- plotly::renderPlotly({
      rank_plot(map_deficit_ranking(year_dt()), "Largest reported deficits", FALSE)
    })

    output$a11y_summary <- shiny::renderUI({
      shiny::p(
        class = "chart-summary",
        map_accessibility_summary(year_dt(), input$metric %||% "trade_balance", selected_iso())
      )
    })

    output$map_table <- DT::renderDT({
      tab <- map_table_data(year_dt(), joined()$crosswalk)
      DT::datatable(
        tab,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE),
        caption = "Non-map alternative: country-year World-partner HS-85 totals."
      )
    }, server = TRUE)

    output$coverage_panel <- shiny::renderUI({
      diag <- map_coverage_diagnostics(
        year_dt(), joined(), coverage(),
        metric = input$metric %||% "trade_balance",
        snap = snap()
      )
      shiny::tagList(
        shiny::tags$ul(
          class = "coverage-list",
          shiny::tags$li(paste0("Source economies (selected year/region): ", format_count(diag$n_source))),
          shiny::tags$li(paste0("Geometry matched: ", format_count(diag$n_matched))),
          shiny::tags$li(paste0("Unmatched: ", format_count(diag$n_unmatched),
                                if (length(diag$unmatched_iso3)) paste0(" (", paste(utils::head(diag$unmatched_iso3, 12), collapse = ", "), ")") else "")),
          shiny::tags$li(paste0("Mapped-value coverage: ", format_pct(diag$coverage_pct),
                                " (method: ", diag$coverage_method, ")")),
          shiny::tags$li(paste0("Missing selected metric: ", format_count(diag$missing_metric))),
          shiny::tags$li(paste0("Missing/invalid GDP: ", format_count(diag$missing_gdp))),
          shiny::tags$li(paste0("Missing/invalid population: ", format_count(diag$missing_population))),
          shiny::tags$li(paste0("Global trade status: ", diag$global_trade_status)),
          shiny::tags$li(paste0("Detailed bilateral status: ", diag$detailed_status)),
          shiny::tags$li(paste0("Latest ingestion: ", diag$ingested_at %||% "Unavailable")),
          shiny::tags$li(paste0("Geometry source: Natural Earth medium (rnaturalearthdata); public-domain terms."))
        ),
        shiny::p(
          class = "method-note",
          list(
            "For signed balance metrics, coverage uses the ratio of absolute mapped values to absolute source values so surpluses and deficits do not cancel. ",
            shiny::tags$a(
              href = "#", class = "dq-link",
              onclick = "document.querySelector('[data-value=\"Data Quality\"]')?.click(); return false;",
              "Open Data Quality"
            ), "."
          )
        )
      )
    })

    output$dl_map <- shiny::downloadHandler(
      filename = function() map_download_filename("trade_balance_map", input$year, input$metric),
      content = function(file) write_map_csv(map_table_data(year_dt(), joined()$crosswalk), file)
    )
    output$dl_trend <- shiny::downloadHandler(
      filename = function() {
        iso <- selected_iso() %||% "aggregate"
        map_download_filename("country_trade", paste0(iso, "_2019_2024"))
      },
      content = function(file) {
        iso <- selected_iso()
        dt <- if (is.null(iso) || !nzchar(iso)) {
          prepare_global_trend(analytics(), input$region %||% "__ALL__", geometry())
        } else {
          prepare_country_trend(analytics(), iso)
        }
        write_map_csv(dt, file)
      }
    )
    output$dl_surplus <- shiny::downloadHandler(
      filename = function() map_download_filename("surplus_ranking", input$year),
      content = function(file) write_map_csv(map_surplus_ranking(year_dt()), file)
    )
    output$dl_deficit <- shiny::downloadHandler(
      filename = function() map_download_filename("deficit_ranking", input$year),
      content = function(file) write_map_csv(map_deficit_ranking(year_dt()), file)
    )
    output$dl_geo <- shiny::downloadHandler(
      filename = function() map_download_filename("geography_unmatched_entities", input$year),
      content = function(file) write_map_csv(joined()$crosswalk, file)
    )
  })
}
