mod_data_quality_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("summary")),
    shiny::h4("Validation results"),
    DT::DTOutput(ns("validation_table"))
  )
}

mod_data_quality_server <- function(id, snap) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary <- shiny::renderUI({
      s <- snap()
      vs <- validation_summary(s)
      has_processed_outputs <- isTRUE(s$available) || any(vapply(
        list(
          s$trade_global,
          s$trade_detailed_enriched,
          s$country_year_analytics,
          s$production_validation,
          s$phase3_validation
        ),
        function(x) !is.null(x) && NROW(x) > 0L,
        logical(1)
      ))

      if (!has_processed_outputs) {
        return(shiny::div(
          class = "empty-state",
          shiny::h3("No processed pipeline outputs"),
          shiny::p("No processed trade or validation datasets were found in the active data bundle.")
        ))
      }

      trade <- s$trade_global %||%
        s$trade_detailed_enriched %||%
        s$trade
      n_missing_value <- if (!is.null(trade) && nrow(trade)) {
        sum(is.na(trade$trade_value_usd))
      } else 0L
      n_dup_msg <- if (!is.null(vs$table) && nrow(vs$table)) {
        d <- vs$table[check_id == "unique_keys" | grepl("duplicate", check_id)]
        if (nrow(d)) sum(d$affected_rows, na.rm = TRUE) else 0L
      } else 0L
      unmatched <- if (!is.null(vs$table) && nrow(vs$table)) {
        u <- vs$table[grepl("^mapping_", check_id) & status != "pass"]
        if (nrow(u)) sum(u$affected_rows, na.rm = TRUE) else 0L
      } else 0L
      ingest_ts <- if (!is.null(trade) && nrow(trade)) {
        max(as.character(trade$ingested_at), na.rm = TRUE)
      } else "—"
      source_ts <- s$manifest$completed_at %||% s$loaded_at %||% "—"

      shiny::tagList(
        shiny::div(
          class = "metric-grid",
          metric_card("Pipeline status", if (vs$n_error == 0) "OK" else "Issues"),
          metric_card("Checks passed", as.character(vs$n_pass)),
          metric_card("Warnings", as.character(vs$n_warning)),
          metric_card("Errors", as.character(vs$n_error)),
          metric_card("Global trade", as.character(s$pipeline_status$global_trade %||% "—")),
          metric_card("Detailed trade", as.character(s$pipeline_status$detailed_trade %||% "—")),
          metric_card("Macro pipeline", as.character(s$pipeline_status$macro %||% "—")),
          metric_card("Missing trade values", as.character(n_missing_value)),
          metric_card("Duplicate findings", as.character(n_dup_msg)),
          metric_card("Unmatched mapping rows", as.character(unmatched)),
          metric_card("Ingestion timestamp", ingest_ts),
          metric_card("Pipeline timestamp", as.character(source_ts))
        ),
        {
          perf <- load_performance_summary_for_ui()
          shiny::div(
            class = "partial-data-notice",
            role = "status",
            shiny::tags$strong("Phase 13 performance"),
            if (isTRUE(perf$available)) {
              shiny::tagList(
                shiny::p(sprintf(
                  "Benchmark available (%s, phase=%s, generated %s). Server-side timings only; browser rendering not measured.",
                  perf$mode, perf$phase, perf$generated_at
                )),
                if (isTRUE(perf$claim_250_supported)) {
                  shiny::p("Headline shock p95 evidence supports an under-250 ms statement for defined scenarios.")
                } else {
                  shiny::p("No public under-250 ms badge is displayed without supporting evidence.")
                },
              )
            } else {
              shiny::p(perf$message %||% "No Phase 13 benchmark results available.")
            }
          )
        },
        if (!is.null(s$phase3_validation) && nrow(s$phase3_validation)) {
          shiny::p(shiny::strong("Phase 3 validation included."),
                   " Detailed Comtrade status is ",
                   as.character(s$pipeline_status$detailed_trade %||% "unknown"),
                   "; missing WDI values are warnings, not imputed.")
        }
      )
    })

    output$validation_table <- DT::renderDT({
      vs <- validation_summary(snap())
      if (!nrow(vs$table)) {
        return(data.frame(message = "No validation results available."))
      }
      DT::datatable(
        as.data.frame(vs$table),
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}
