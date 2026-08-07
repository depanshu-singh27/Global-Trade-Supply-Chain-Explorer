mod_shock_simulator_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "shock-page",
      shiny::div(
        class = "hero-panel",
        shiny::h2("Shock Simulator"),
        shiny::p(
          "Configure → Validate → Run → Analyse → Compare → Export. ",
          "Deterministic scenario sensitivities over observed import dependencies — ",
          "not forecasts of realised economic losses."
        )
      ),
      shiny::uiOutput(ns("notices")),
      shiny::uiOutput(ns("stale_banner")),
      shiny::uiOutput(ns("workflow_bar")),

      bslib::accordion(
        id = ns("shock_workflow_acc"),
        open = "builder",
        class = "shock-workflow-accordion",
        bslib::accordion_panel(
          title = "1. Scenario builder",
          value = "builder",
          shiny::uiOutput(ns("builder_panel"))
        ),
        bslib::accordion_panel(
          title = "2. Validation",
          value = "validation",
          shiny::uiOutput(ns("validation_panel"))
        ),
        bslib::accordion_panel(
          title = "3. Target preview",
          value = "preview",
          shiny::uiOutput(ns("preview_panel"))
        ),
        bslib::accordion_panel(
          title = "4. Run controls",
          value = "run",
          shiny::uiOutput(ns("exec_panel"))
        )
      ),

      shiny::uiOutput(ns("kpi_strip")),

      shiny::div(
        class = "shock-grid",
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Reporter impact ranking"),
          shiny::selectInput(ns("rep_metric"), "Metric", shock_reporter_rank_metric_choices()),
          shiny::numericInput(ns("rep_top_n"), "Top N", 12, min = 5, max = 40),
          shiny::selectInput(ns("rep_select"), "Highlight reporter", choices = c("—" = ""), selected = ""),
          plotly::plotlyOutput(ns("rep_rank_plot"), height = "320px"),
          shiny::uiOutput(ns("rep_rank_text"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Reporter profile"),
          shiny::uiOutput(ns("rep_profile"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Commodity impact ranking"),
          shiny::selectInput(
            ns("com_metric"), "Sort by",
            c(
              "Residual unmet value" = "residual_unmet_value_usd",
              "Residual share of baseline" = "residual_share_of_baseline",
              "Direct disruption" = "direct_disrupted_value_usd"
            )
          ),
          plotly::plotlyOutput(ns("com_rank_plot"), height = "320px"),
          DT::DTOutput(ns("com_table"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Supplier allocation"),
          shiny::p(
            class = "muted",
            "Substitution represents a scenario allocation across observed existing suppliers, ",
            "not verified real-world spare capacity."
          ),
          plotly::plotlyOutput(ns("sup_plot"), height = "300px"),
          DT::DTOutput(ns("sup_table"))
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Geographic impact"),
          shiny::selectInput(ns("map_metric"), "Map metric", shock_map_metric_choices()),
          leaflet::leafletOutput(ns("impact_map"), height = "420px"),
          shiny::uiOutput(ns("map_text")),
          DT::DTOutput(ns("map_table"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Before / after concentration"),
          plotly::plotlyOutput(ns("conc_plot"), height = "300px"),
          DT::DTOutput(ns("conc_table"))
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Propagation paths"),
          shiny::uiOutput(ns("path_empty")),
          plotly::plotlyOutput(ns("path_plot"), height = "260px"),
          DT::DTOutput(ns("path_table"))
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Scenario comparison"),
          shiny::uiOutput(ns("compare_panel")),
          DT::DTOutput(ns("compare_kpi_table")),
          DT::DTOutput(ns("compare_rep_table"))
        ),
        shiny::div(
          class = "chart-card chart-card-wide",
          shiny::h3(class = "chart-title", "Detailed impact tables"),
          DT::DTOutput(ns("edge_table"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Scenario history"),
          DT::DTOutput(ns("history_table")),
          shiny::uiOutput(ns("history_actions")),
          shiny::uiOutput(ns("hist_msg"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Coverage & methodology"),
          shiny::uiOutput(ns("coverage_panel"))
        ),
        shiny::div(
          class = "chart-card",
          shiny::h3(class = "chart-title", "Downloads"),
          shiny::p(class = "muted", "Exports use the active scenario result. No secrets or filesystem paths."),
          shiny::uiOutput(ns("download_panel"))
        )
      )
    )
  )
}

mod_shock_simulator_server <- function(id, snap, cfg) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    root <- find_project_root()

    rv <- shiny::reactiveValues(
      running = FALSE,
      run_started_at = NULL,
      run_elapsed_ms = NULL,
      active_scenario_id = NULL,
      active_definition = NULL,
      active_result = NULL,
      active_manifest = NULL,
      active_result_source = NULL,
      active_result_error = NULL,
      prior_result = NULL,
      last_error = NULL,
      last_success = NULL,
      history_tick = 0L,
      compare_dir = NULL,
      selected_reporter = NULL
    )

    coverage <- shiny::reactive({
      s <- snap()
      s$detailed_coverage %||% trade_flow_coverage_status(s)
    })

    detailed <- shiny::reactive({
      s <- snap()
      d <- s$trade_detailed_enriched %||% s$trade_detailed
      if (is.null(d)) data.table::data.table() else data.table::as.data.table(d)
    })

    builder_inputs <- shiny::reactive({
      list(
        scenario_name = input$scenario_name,
        scenario_description = input$scenario_description,
        baseline_year_start = input$baseline_year_start,
        baseline_year_end = input$baseline_year_end %||% input$baseline_year_start,
        target_supplier_iso3 = input$target_supplier_iso3,
        target_reporter_iso3 = input$target_reporter_iso3,
        target_hs_codes = input$target_hs_codes,
        shock_type = input$shock_type,
        shock_size_pct = input$shock_size_pct,
        substitution_mode = input$substitution_mode,
        substitution_capacity_pct = input$substitution_capacity_pct,
        maximum_substitute_supplier_share = input$maximum_substitute_supplier_share,
        enable_max_share = isTRUE(input$enable_max_share),
        propagation_mode = input$propagation_mode,
        maximum_propagation_steps = input$maximum_propagation_steps,
        propagation_decay = input$propagation_decay,
        minimum_propagated_value_usd = input$minimum_propagated_value_usd,
        minimum_dependency_share = input$minimum_dependency_share,
        include_macro_normalisation = isTRUE(input$include_macro_normalisation),
        acknowledge_partial_coverage = isTRUE(input$acknowledge_partial_coverage),
        universe_version = coverage()$universe_checksum
      )
    })

    builder_scenario <- shiny::reactive({
      shock_ui_to_scenario(builder_inputs(), coverage = coverage())
    })

    builder_baseline <- shiny::reactive({
      d <- detailed()
      sc <- builder_scenario()
      if (!nrow(d)) return(data.table::data.table())
      build_shock_baseline(
        d,
        year_min = sc$baseline_year_start,
        year_max = sc$baseline_year_end,
        coverage = coverage(),
        universe_version = coverage()$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM
      )$baseline
    })

    choices <- shiny::reactive({
      shock_control_choices(builder_baseline(), coverage())
    })

    year_pool <- shiny::reactive({
      d <- detailed()
      if (!nrow(d) || !"year" %in% names(d)) return(integer())
      sort(unique(as.integer(d$year)))
    })

    choice_catalogue <- shiny::reactive({
      d <- detailed()
      cov <- coverage()
      years <- year_pool()
      if (!nrow(d) || !length(years)) {
        return(shock_control_choices(data.table::data.table(), cov))
      }
      bl <- build_shock_baseline(
        d,
        year_min = min(years),
        year_max = max(years),
        coverage = cov,
        universe_version = cov$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM
      )$baseline
      shock_control_choices(bl, cov)
    })

    validation <- shiny::reactive({
      shock_ui_validate_inputs(builder_inputs(), baseline = builder_baseline(), coverage = coverage())
    })

    preview <- shiny::reactive({
      shock_target_preview(builder_baseline(), builder_scenario())
    })

    stale <- shiny::reactive({
      shock_stale_result_state(builder_scenario(), rv$active_result, coverage = coverage())
    })

    history <- shiny::reactive({

      rv$history_tick
      inc_perf_counter("scenario_history_scan_count")
      shock_index_scenario_history(root)
    })

    capabilities <- shiny::reactive({
      rt <- cfg()$runtime %||% get_runtime_config()
      shock_ui_capabilities(rt, history_n = nrow(history()))
    })

    persist_enabled <- shiny::reactive({
      isTRUE(capabilities()$can_persist)
    })

    activate_result <- function(res, source = "run") {
      if (!isTRUE(res$ok)) {
        msg <- paste(res$errors %||% "Unable to activate scenario result.", collapse = "; ")
        rv$active_result_error <- msg
        rv$last_error <- msg

        return(FALSE)
      }
      rv$prior_result <- rv$active_result
      rv$active_scenario_id <- res$scenario$scenario_id %||% res$scenario_hash
      rv$active_definition <- res$scenario
      rv$active_result <- res
      rv$active_manifest <- res$manifest %||% shock_result_download_meta(res)
      rv$active_result_source <- source
      rv$active_result_error <- NULL
      rv$last_error <- NULL
      rv$last_success <- paste("Active result:", res$scenario$scenario_name %||% rv$active_scenario_id)
      if (nrow(res$reporter_impacts %||% data.table::data.table())) {
        rv$selected_reporter <- res$reporter_impacts$reporter_iso3[
          which.max(res$reporter_impacts$residual_unmet_value_usd)
        ]
      }
      TRUE
    }

    output$notices <- shiny::renderUI({
      cov <- coverage()
      caps <- capabilities()
      shiny::tagList(
        shiny::div(
          class = "status-badge-row",
          status_badge("Engine", "complete", label_override = SHOCK_ENGINE_VERSION),
          status_badge(
            "Detailed imports",
            cov$production_status %||% "partial",
            label_override = sprintf(
              "%d/%d",
              cov$represented_reporter_count %||% 0L,
              cov$selected_reporter_count %||% 0L
            )
          ),
          status_badge("Universe", "partial", label_override = cov$universe_checksum %||% "n/a"),
          if (!isTRUE(caps$can_persist)) {
            status_badge("Persistence", "partial", label_override = "read-only")
          }
        ),
        shiny::div(
          class = "partial-data-notice shock-partial-notice",
          role = "status",
          shiny::tags$strong(shock_ui_partial_notice(
            cov$represented_reporter_count, cov$selected_reporter_count, coverage = cov
          ))
        ),
        shiny::div(
          class = "partial-data-notice shock-method-notice",
          role = "note",
          shiny::tags$strong(shock_ui_methodology_notice())
        ),
        if (!isTRUE(caps$can_persist)) {
          shiny::div(
            class = "partial-data-notice shock-readonly-notice",
            role = "status",
            shiny::tags$strong(caps$read_only_notice)
          )
        }
      )
    })

    output$stale_banner <- shiny::renderUI({
      st <- stale()
      if (!isTRUE(st$stale) && !isTRUE(st$inputs_changed)) return(NULL)
      shiny::div(
        class = "partial-data-notice shock-stale-notice",
        role = "alert",
        shiny::tags$strong(
          if (isTRUE(st$inputs_changed)) {
            "Inputs changed since last run. Displayed results are from the previous execution."
          } else {
            st$message
          }
        )
      )
    })

    output$workflow_bar <- shiny::renderUI({
      shiny::div(
        class = "shock-workflow",
        shiny::tags$ol(
          shiny::tags$li("Configure"),
          shiny::tags$li("Validate"),
          shiny::tags$li("Run"),
          shiny::tags$li("Analyse"),
          shiny::tags$li("Compare"),
          shiny::tags$li("Export")
        )
      )
    })

    output$builder_panel <- shiny::renderUI({

      cov <- coverage()
      years <- year_pool()
      defs <- shock_builder_defaults(cov, years)
      ch <- choice_catalogue()
      ex <- shock_example_catalog(root)
      ex_choices <- c("— Select example —" = "")
      if (nrow(ex)) {
        labs <- ifelse(ex$available, ex$label, paste0(ex$label, " (unavailable)"))
        ex_choices <- c(ex_choices, stats::setNames(ex$file, labs))
      }

      shiny::tagList(
        shiny::div(
          class = "shock-builder-grid",
          shiny::textInput(ns("scenario_name"), "Scenario name", defs$scenario_name),
          shiny::textAreaInput(ns("scenario_description"), "Description", "", rows = 2),
          shiny::selectInput(
            ns("baseline_year_start"), "Baseline year start",
            choices = if (length(years)) years else defs$baseline_year_start,
            selected = defs$baseline_year_start
          ),
          shiny::selectInput(
            ns("baseline_year_end"), "Baseline year end",
            choices = if (length(years)) years else defs$baseline_year_end,
            selected = defs$baseline_year_end
          ),
          shiny::selectInput(
            ns("target_supplier_iso3"), "Target supplier",
            choices = ch$suppliers, multiple = TRUE, selected = character()
          ),
          shiny::selectInput(
            ns("target_reporter_iso3"), "Target reporters (optional / required for bilateral)",
            choices = ch$reporters, multiple = TRUE, selected = character()
          ),
          shiny::selectInput(
            ns("target_hs_codes"), "Target HS4",
            choices = ch$hs_codes, multiple = TRUE, selected = character()
          ),
          shiny::selectInput(ns("shock_type"), "Shock type", shock_type_choices()),
          shiny::sliderInput(
            ns("shock_size_pct"), "Shock size (%)",
            min = 0, max = 100, value = 30, step = 1
          ),
          shiny::selectInput(
            ns("substitution_mode"), "Substitution mode",
            shock_substitution_mode_choices(),
            selected = "capacity_constrained"
          ),
          shiny::uiOutput(ns("builder_sub_controls")),
          shiny::selectInput(
            ns("propagation_mode"), "Propagation mode",
            shock_propagation_mode_choices(),
            selected = "direct_only"
          ),
          shiny::uiOutput(ns("builder_prop_controls")),
          shiny::checkboxInput(ns("include_macro_normalisation"), "Include macro normalisation (GDP shares)", TRUE),
          if (isTRUE(shock_partial_ack_required(cov))) {
            shiny::checkboxInput(
              ns("acknowledge_partial_coverage"),
              "I understand that the current scenario uses partial detailed coverage.",
              FALSE
            )
          } else {
            shiny::div(class = "muted", "Partial-coverage acknowledgement not required (production complete).")
          },
          shiny::selectInput(ns("example_file"), "Load example", choices = ex_choices)
        )
      )
    })

    output$builder_sub_controls <- shiny::renderUI({
      mode <- as.character(input$substitution_mode %||% "capacity_constrained")[1]
      stype <- as.character(input$shock_type %||% "commodity_specific_supplier_reduction")[1]
      vis <- shock_control_visibility(mode, "direct_only", stype)

      vals <- shiny::isolate(list(
        capacity = as.numeric(input$substitution_capacity_pct %||% 25)[1],
        enable_max = isTRUE(input$enable_max_share),
        max_share = as.numeric(input$maximum_substitute_supplier_share %||% 100)[1]
      ))
      shiny::tagList(
        if (isTRUE(vis$show_capacity)) {
          shiny::sliderInput(
            ns("substitution_capacity_pct"),
            "Assumed additional observed-supplier capacity (%)",
            min = 0, max = 100, value = vals$capacity, step = 1
          )
        },
        if (isTRUE(vis$show_max_share)) {
          shiny::tagList(
            shiny::checkboxInput(
              ns("enable_max_share"),
              "Constrain maximum substitute supplier share",
              vals$enable_max
            ),
            shiny::sliderInput(
              ns("maximum_substitute_supplier_share"),
              "Maximum substitute supplier share (%)",
              min = 0, max = 100,
              value = vals$max_share,
              step = 1
            )
          )
        }
      )
    })

    output$builder_prop_controls <- shiny::renderUI({
      mode <- as.character(input$propagation_mode %||% "direct_only")[1]
      if (!mode %in% c("first_order", "multi_step")) return(NULL)
      vals <- shiny::isolate(list(
        steps = as.integer(input$maximum_propagation_steps %||% 1L)[1],
        decay = as.numeric(input$propagation_decay %||% 1)[1],
        min_val = as.numeric(input$minimum_propagated_value_usd %||% 0)[1],
        min_share = as.numeric(input$minimum_dependency_share %||% 0)[1]
      ))
      shiny::div(
        class = "shock-advanced",
        shiny::numericInput(
          ns("maximum_propagation_steps"), "Maximum propagation depth",
          value = vals$steps, min = 1, max = 5
        ),
        shiny::sliderInput(
          ns("propagation_decay"), "Propagation decay",
          0, 1, vals$decay, 0.05
        ),
        shiny::numericInput(
          ns("minimum_propagated_value_usd"), "Minimum propagated value (USD)",
          value = vals$min_val, min = 0
        ),
        shiny::sliderInput(
          ns("minimum_dependency_share"), "Minimum dependency share",
          0, 1, vals$min_share, 0.01
        )
      )
    })

    shiny::observeEvent(list(input$baseline_year_start, input$baseline_year_end), {
      ch <- choices()
      shiny::updateSelectInput(
        session, "target_supplier_iso3",
        choices = ch$suppliers,
        selected = intersect(input$target_supplier_iso3 %||% character(), ch$suppliers)
      )
      shiny::updateSelectInput(
        session, "target_reporter_iso3",
        choices = ch$reporters,
        selected = intersect(input$target_reporter_iso3 %||% character(), ch$reporters)
      )
      shiny::updateSelectInput(
        session, "target_hs_codes",
        choices = ch$hs_codes,
        selected = intersect(input$target_hs_codes %||% character(), ch$hs_codes)
      )
    }, ignoreInit = TRUE)

    shiny::outputOptions(output, "builder_panel", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "builder_sub_controls", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "builder_prop_controls", suspendWhenHidden = FALSE)

    output$validation_panel <- shiny::renderUI({
      v <- shock_ui_validation_groups(validation())
      cls <- paste0("shock-validation-", tolower(v$status))
      shiny::div(
        class = paste("shock-validation-summary", cls),
        role = "status",
        `aria-live` = "polite",
        shiny::tags$strong(paste("Status:", v$status)),
        shiny::p(v$accessible_summary),
        if (length(v$errors)) {
          shiny::tags$ul(lapply(v$errors, function(e) shiny::tags$li(class = "shock-error", e)))
        },
        if (length(v$warnings)) {
          shiny::tags$ul(lapply(v$warnings, function(w) shiny::tags$li(class = "shock-warning", w)))
        }
      )
    })

    output$preview_panel <- shiny::renderUI({
      p <- preview()
      shiny::tags$ul(
        shiny::tags$li(paste("Target edges:", p$n_target_edges)),
        shiny::tags$li(paste("Reporters:", p$n_reporters)),
        shiny::tags$li(paste("Commodities:", p$n_commodities)),
        shiny::tags$li(paste("Suppliers:", paste(p$target_suppliers %||% character(), collapse = ", "))),
        shiny::tags$li(paste("Baseline period:", p$baseline_year_start, "–", p$baseline_year_end)),
        shiny::tags$li(paste("Targeted baseline value:", format_shock_usd(p$targeted_baseline_value_usd))),
        shiny::tags$li(paste("Potential direct disruption:", format_shock_usd(p$potential_direct_disruption_usd))),
        shiny::tags$li(paste("Eligible substitute supplier rows:", p$eligible_substitute_supplier_rows)),
        shiny::tags$li(paste("Indicative substitution capacity:", format_shock_usd(p$indicative_substitution_capacity_usd))),
        shiny::tags$li(paste("Propagation:", p$propagation_mode, "depth", p$maximum_propagation_steps)),
        shiny::tags$li(paste("Complexity edges / groups:", p$complexity$edge_count, "/", p$complexity$reporter_commodity_group_count)),
        if (isTRUE(p$neutral_control)) shiny::tags$li(class = "shock-warning", "Neutral 0% control scenario"),
        shiny::tags$li(paste("Universe:", coverage()$universe_checksum %||% "n/a")),
        shiny::tags$li(paste("Coverage:", coverage()$production_status %||% "unknown"))
      )
    })

    output$exec_panel <- shiny::renderUI({
      v <- validation()
      caps <- capabilities()
      can_persist <- isTRUE(caps$can_persist)
      shiny::tagList(
        if (!can_persist) {
          shiny::div(
            class = "partial-data-notice shock-readonly-notice",
            role = "status",
            shiny::tags$strong(caps$read_only_notice)
          )
        },
        shiny::div(
          class = "download-row",
          shiny::actionButton(
            ns("run_scenario"), "Run scenario",
            class = "btn-primary",
            disabled = if (isTRUE(rv$running) || !isTRUE(v$ok)) "disabled" else NULL
          ),
          shiny::actionButton(ns("reset_builder"), "Reset to defaults", class = "btn-outline-secondary"),
          shiny::actionButton(ns("dup_scenario"), "Duplicate definition", class = "btn-outline-secondary"),
          if (can_persist) {
            shiny::actionButton(ns("save_def"), "Save definition", class = "btn-outline-secondary")
          } else {
            shiny::tags$span(
              class = "muted",
              title = "Disabled in public/read-only mode",
              "Save definition (disabled — read-only)"
            )
          },
          shiny::actionButton(ns("load_example"), "Load selected example", class = "btn-outline-secondary")
        ),
        shiny::div(
          class = paste("shock-exec-state", if (isTRUE(rv$running)) "running" else "idle"),
          role = "status",
          if (isTRUE(rv$running)) {
            shiny::tags$strong("Running scenario…")
          } else if (!is.null(rv$last_error)) {
            shiny::tags$strong(paste("Error:", rv$last_error))
          } else if (!is.null(rv$last_success)) {
            shiny::tagList(
              shiny::tags$strong(rv$last_success),
              if (!is.null(rv$run_elapsed_ms)) {
                shiny::tags$span(class = "muted", sprintf(" (%0.0f ms)", rv$run_elapsed_ms))
              }
            )
          } else if (!is.null(rv$active_result) && isTRUE(rv$active_result$ok)) {
            shiny::tags$strong(paste(
              "Active:", rv$active_result$scenario$scenario_name,
              sprintf("(%s)", rv$active_result_source %||% "session")
            ))
          } else {
            shiny::tags$span(class = "muted", "Ready. Run is explicit and does not auto-execute on slider changes.")
          }
        ),
        shiny::p(
          class = "muted",
          if (can_persist) {
            "Persistence: enabled — successful runs may be written under scenario results."
          } else {
            "Persistence: disabled — runs stay in session memory only."
          }
        )
      )
    })

    apply_scenario_to_inputs <- function(sc) {
      sc <- normalize_shock_scenario(sc)
      shiny::updateTextInput(session, "scenario_name", value = sc$scenario_name)
      shiny::updateTextAreaInput(session, "scenario_description", value = sc$scenario_description)
      shiny::updateSelectInput(session, "baseline_year_start", selected = sc$baseline_year_start)
      shiny::updateSelectInput(session, "baseline_year_end", selected = sc$baseline_year_end)
      shiny::updateSelectInput(session, "target_supplier_iso3", selected = sc$target_supplier_iso3)
      shiny::updateSelectInput(session, "target_reporter_iso3", selected = sc$target_reporter_iso3)
      shiny::updateSelectInput(session, "target_hs_codes", selected = sc$target_hs_codes)
      shiny::updateSelectInput(session, "shock_type", selected = sc$shock_type)
      shiny::updateSliderInput(session, "shock_size_pct", value = sc$shock_size_pct)
      shiny::updateSelectInput(session, "substitution_mode", selected = sc$substitution_mode)
      shiny::updateSelectInput(session, "propagation_mode", selected = sc$propagation_mode)
      shiny::updateCheckboxInput(session, "acknowledge_partial_coverage", value = isTRUE(sc$acknowledge_partial_coverage))
      shiny::updateCheckboxInput(session, "include_macro_normalisation", value = isTRUE(sc$include_macro_normalisation))
      if (!is.null(input$substitution_capacity_pct)) {
        shiny::updateSliderInput(session, "substitution_capacity_pct", value = sc$substitution_capacity_pct)
      }
      max_share_pct <- round(100 * as.numeric(sc$maximum_substitute_supplier_share %||% 1))
      if (!is.null(input$maximum_substitute_supplier_share)) {
        shiny::updateSliderInput(session, "maximum_substitute_supplier_share", value = max_share_pct)
      }
      if (!is.null(input$maximum_propagation_steps)) {
        shiny::updateNumericInput(session, "maximum_propagation_steps", value = sc$maximum_propagation_steps)
      }
      if (!is.null(input$propagation_decay)) {
        shiny::updateSliderInput(session, "propagation_decay", value = sc$propagation_decay)
      }
    }

    shiny::observeEvent(input$reset_builder, {
      defs <- shock_builder_defaults(coverage(), year_pool())
      apply_scenario_to_inputs(defs)
    })

    shiny::observeEvent(input$load_example, {
      f <- input$example_file
      if (!nzchar(f %||% "")) return()
      path <- file.path(shock_scenario_dirs(root)$examples, f)
      sc <- tryCatch(read_shock_scenario_file(path), error = function(e) NULL)
      if (is.null(sc)) {
        rv$last_error <- "Unable to load example definition."
        return()
      }
      av <- shock_example_availability(sc, detailed(), coverage())
      if (!isTRUE(av$available)) {
        rv$last_error <- "Selected example has no valid targets in current detailed data."
        return()
      }
      sc$acknowledge_partial_coverage <- TRUE
      sc$universe_version <- coverage()$universe_checksum %||% sc$universe_version
      apply_scenario_to_inputs(sc)
      rv$last_error <- NULL
    })

    shiny::observeEvent(input$dup_scenario, {
      sc <- shock_duplicate_scenario_definition(builder_scenario())
      apply_scenario_to_inputs(sc)
    })

    shiny::observeEvent(input$save_def, {
      if (!persist_enabled()) {
        rv$last_error <- "Scenario persistence is disabled in public/read-only mode."
        return()
      }
      sc <- normalize_shock_scenario(builder_scenario())
      path <- file.path(
        shock_scenario_dirs(root)$definitions,
        paste0(sanitize_shock_token(sc$scenario_id, "scn"), ".json")
      )
      tryCatch(write_shock_scenario_file(sc, path), error = function(e) {
        rv$last_error <- "Could not save definition."
      })
    })

    shiny::observeEvent(input$run_scenario, {
      if (isTRUE(rv$running)) return()
      v <- validation()
      if (!isTRUE(v$ok)) {
        rv$last_error <- paste(v$errors, collapse = "; ")
        rv$last_success <- NULL
        return()
      }
      rv$running <- TRUE
      rv$run_started_at <- Sys.time()
      rv$last_error <- NULL
      rv$last_success <- NULL
      res <- tryCatch(
        run_shock_scenario_orchestrated(
          detailed(),
          v$scenario,
          coverage = coverage(),
          root = root,
          persist = persist_enabled()
        ),
        error = function(e) list(ok = FALSE, errors = "Scenario execution failed.")
      )
      rv$running <- FALSE
      rv$run_elapsed_ms <- as.numeric(difftime(Sys.time(), rv$run_started_at, units = "secs")) * 1000
      if (!isTRUE(res$ok)) {
        activate_result(res, source = "run_failed")
      } else {
        res$loaded_from_disk <- FALSE
        activate_result(res, source = if (persist_enabled()) "run_persisted" else "run_in_memory")
        rv$history_tick <- rv$history_tick + 1L
      }
    })

    active <- shiny::reactive(rv$active_result)

    output$kpi_strip <- shiny::renderUI({
      if (!is.null(rv$active_result_error) && (is.null(active()) || !isTRUE(active()$ok))) {
        return(shiny::div(
          class = "empty-state shock-error-state",
          role = "alert",
          shiny::p(rv$active_result_error)
        ))
      }
      k <- shock_prepare_kpis(active())
      if (!isTRUE(k$available)) {
        return(shiny::div(
          class = "empty-state",
          role = "status",
          shiny::p(shock_no_active_result_message())
        ))
      }
      shiny::div(
        class = "shock-kpi-strip",
        role = "region",
        `aria-label` = "Scenario KPI summary",
        shiny::div(class = "kpi-card", shiny::tags$span("Targeted baseline"), shiny::tags$strong(format_shock_usd(k$targeted_baseline_imports_usd))),
        shiny::div(class = "kpi-card", shiny::tags$span("Direct disrupted imports"), shiny::tags$strong(format_shock_usd(k$direct_disrupted_imports_usd))),
        shiny::div(class = "kpi-card", shiny::tags$span("Substitution allocated"), shiny::tags$strong(format_shock_usd(k$substitution_allocated_usd))),
        shiny::div(class = "kpi-card", shiny::tags$span("Residual unmet imports"), shiny::tags$strong(format_shock_usd(k$residual_unmet_imports_usd))),
        shiny::div(class = "kpi-card", shiny::tags$span("Substitution rate"), shiny::tags$strong(format_shock_pct(100 * (k$substitution_rate %||% NA_real_)))),
        shiny::div(class = "kpi-card", shiny::tags$span("Affected reporters"), shiny::tags$strong(k$affected_reporters)),
        shiny::div(class = "kpi-card", shiny::tags$span("Affected HS4"), shiny::tags$strong(k$affected_commodities)),
        shiny::div(class = "kpi-card", shiny::tags$span("Max reporter residual"), shiny::tags$strong(format_shock_usd(k$max_reporter_residual_usd))),
        shiny::p(class = "muted", shock_kpi_text_summary(k))
      )
    })

    ranked_reporters <- shiny::reactive({
      res <- active()
      if (is.null(res) || !isTRUE(res$ok)) return(data.table::data.table())
      metric <- input$rep_metric %||% "residual_unmet_value_usd"
      if (!nzchar(metric) || !metric %in% names(res$reporter_impacts)) {
        metric <- "residual_unmet_value_usd"
      }
      top_n <- as.integer(input$rep_top_n %||% 12L)
      if (!is.finite(top_n) || top_n < 1L) top_n <- 12L
      shock_rank_reporters_ui(res$reporter_impacts, metric, top_n)
    })

    shiny::observe({
      rr <- ranked_reporters()
      choices <- c("—" = "")
      if (nrow(rr)) {
        choices <- c(choices, stats::setNames(rr$reporter_iso3, rr$reporter_iso3))
      }
      shiny::updateSelectInput(session, "rep_select", choices = choices)
    })

    output$rep_rank_plot <- plotly::renderPlotly({
      shock_reporter_rank_plot(ranked_reporters(), input$rep_metric, rv$selected_reporter)
    })
    output$rep_rank_text <- shiny::renderUI({
      shiny::p(class = "muted", shock_reporter_rank_text(ranked_reporters(), input$rep_metric))
    })

    shiny::observeEvent(input$rep_select, {
      if (nzchar(input$rep_select %||% "")) rv$selected_reporter <- input$rep_select
    }, ignoreNULL = TRUE)

    output$rep_profile <- shiny::renderUI({
      res <- active()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(shiny::div(class = "empty-state", shiny::p(shock_no_active_result_message())))
      }
      iso <- rv$selected_reporter %||% ranked_reporters()$reporter_iso3[1]
      row <- res$reporter_impacts[reporter_iso3 == iso]
      if (!nrow(row)) {
        return(shiny::div(class = "empty-state", shiny::p("No affected reporters in the active result.")))
      }
      shiny::tags$ul(
        shiny::tags$li(paste("Reporter:", iso)),
        shiny::tags$li(paste("Baseline total imports:", format_shock_usd(row$baseline_total_imports_usd[1]))),
        shiny::tags$li(paste("Direct disruption:", format_shock_usd(row$direct_disrupted_value_usd[1]))),
        shiny::tags$li(paste("Substitution allocated:", format_shock_usd(row$substitution_allocated_value_usd[1]))),
        shiny::tags$li(paste("Residual unmet imports:", format_shock_usd(row$residual_unmet_value_usd[1]))),
        shiny::tags$li(paste(
          "Residual unmet imports as % of GDP:",
          format_shock_pct(row$residual_unmet_pct_gdp[1])
        )),
        shiny::tags$li(paste("Affected HS4 count:", row$affected_hs4_count[1])),
        shiny::tags$li(paste("Affected suppliers:", row$affected_supplier_count[1]))
      )
    })

    ranked_com <- shiny::reactive({
      res <- active()
      if (is.null(res) || !isTRUE(res$ok)) return(data.table::data.table())
      metric <- input$com_metric %||% "residual_unmet_value_usd"
      if (!metric %in% names(res$commodity_impacts)) metric <- "residual_unmet_value_usd"
      shock_rank_commodities_ui(res$commodity_impacts, metric, 15L)
    })

    output$com_rank_plot <- plotly::renderPlotly({
      shock_commodity_rank_plot(ranked_com(), input$com_metric %||% "residual_unmet_value_usd")
    })
    output$com_table <- DT::renderDT({
      DT::datatable(
        ranked_com(),
        options = list(pageLength = 8, scrollX = TRUE),
        rownames = FALSE,
        caption = "Commodity impacts (current USD)"
      )
    })

    output$sup_plot <- plotly::renderPlotly({
      res <- active()
      if (is.null(res) || !isTRUE(res$ok)) return(plotly::plot_ly(type = "bar"))
      shock_supplier_allocation_plot(shock_supplier_allocation_summary(res$supplier_impacts))
    })
    output$sup_table <- DT::renderDT({
      res <- active()
      dt <- if (is.null(res) || !isTRUE(res$ok)) data.table::data.table() else res$supplier_impacts
      DT::datatable(dt, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })

    map_prepared <- shiny::reactive({
      res <- active()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(shock_prepare_map_impacts(data.table::data.table()))
      }
      geom <- snap()$map_geometry
      shock_prepare_map_impacts(
        res$reporter_impacts,
        geometry = geom,
        metric = input$map_metric %||% "residual_unmet_value_usd",
        represented = coverage()$represented_reporters,
        selected_universe = coverage()$selected_reporters
      )
    })

    output$impact_map <- leaflet::renderLeaflet({
      shock_build_impact_leaflet(map_prepared())
    })
    output$map_text <- shiny::renderUI(shiny::p(class = "muted", shock_map_text_summary(map_prepared())))
    output$map_table <- DT::renderDT({
      DT::datatable(map_prepared()$table, options = list(pageLength = 8), rownames = FALSE,
                    caption = "Map table alternative (unavailable ≠ zero)")
    })

    output$conc_plot <- plotly::renderPlotly({
      res <- active()
      if (is.null(res) || !isTRUE(res$ok)) return(plotly::plot_ly(type = "scatter"))
      shock_concentration_change_plot(shock_prepare_concentration_changes(res$post_shock_dependency))
    })
    output$conc_table <- DT::renderDT({
      res <- active()
      dt <- if (is.null(res) || !isTRUE(res$ok)) {
        data.table::data.table()
      } else {
        shock_prepare_concentration_changes(res$post_shock_dependency)
      }
      DT::datatable(dt, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })

    output$path_empty <- shiny::renderUI({
      res <- active()
      if (is.null(res) || !isTRUE(res$ok)) {
        return(shiny::div(class = "empty-state", shiny::p(shock_no_active_result_message())))
      }
      mode <- res$scenario$propagation_mode %||% builder_scenario()$propagation_mode
      paths <- res$impact_paths %||% data.table::data.table()
      if (identical(mode, "direct_only")) {
        shiny::div(
          class = "empty-state",
          shiny::p("No propagation paths are generated in Direct only mode.")
        )
      } else if (!nrow(paths)) {
        shiny::div(
          class = "empty-state",
          shiny::p("No propagation paths are present for the active scenario result.")
        )
      } else {
        NULL
      }
    })
    output$path_plot <- plotly::renderPlotly({
      res <- active()
      paths <- if (is.null(res)) data.table::data.table() else res$impact_paths
      shock_propagation_depth_plot(shock_filter_propagation_paths(paths))
    })
    output$path_table <- DT::renderDT({
      res <- active()
      paths <- if (is.null(res)) data.table::data.table() else shock_filter_propagation_paths(res$impact_paths)
      DT::datatable(paths, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })

    output$compare_panel <- shiny::renderUI({
      hist <- history()
      choices <- c("— None —" = "")
      if (nrow(hist)) {
        choices <- c(choices, stats::setNames(hist$result_dir, hist$scenario_name))
      }
      shiny::tagList(
        shiny::selectInput(ns("compare_result_dir"), "Compare active result with", choices = choices),
        shiny::uiOutput(ns("compare_warnings"))
      )
    })

    compare_ui <- shiny::reactive({
      res_a <- active()
      dir_b <- input$compare_result_dir
      if (is.null(res_a) || !nzchar(dir_b %||% "")) return(NULL)
      res_b <- shock_load_persisted_result(dir_b)
      shock_prepare_comparison_ui(res_a, res_b, id_a = "active", id_b = "selected")
    })

    output$compare_warnings <- shiny::renderUI({
      cmp <- compare_ui()
      if (is.null(cmp)) return(shiny::p(class = "muted", "Select a historical scenario to compare."))
      if (!isTRUE(cmp$ok)) {
        return(shiny::div(class = "shock-error", paste(cmp$compatibility$errors, collapse = "; ")))
      }
      shiny::tagList(
        lapply(cmp$compatibility$warnings, function(w) shiny::p(class = "shock-warning", w)),
        shiny::p(paste("Newly affected:", paste(cmp$newly_affected %||% character(), collapse = ", "))),
        shiny::p(paste("No longer affected:", paste(cmp$no_longer_affected %||% character(), collapse = ", ")))
      )
    })

    output$compare_kpi_table <- DT::renderDT({
      cmp <- compare_ui()
      DT::datatable(cmp$kpi_diff %||% data.table::data.table(), options = list(dom = "t"), rownames = FALSE)
    })
    output$compare_rep_table <- DT::renderDT({
      cmp <- compare_ui()
      dt <- if (is.null(cmp$comparison)) data.table::data.table() else cmp$comparison$aligned_reporters
      DT::datatable(dt, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
    })

    output$edge_table <- DT::renderDT({
      res <- active()
      dt <- if (is.null(res) || !isTRUE(res$ok)) data.table::data.table() else res$edge_impacts
      DT::datatable(
        dt,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        filter = "top"
      )
    })

    history_display <- shiny::reactive({
      hist <- history()
      cols <- c(
        "scenario_name", "scenario_id", "created_at", "baseline_year_start", "baseline_year_end",
        "supplier", "hs_scope", "shock_size_pct", "substitution_mode", "propagation_mode",
        "residual_unmet_value_usd", "engine_version", "universe_version", "status"
      )
      if (!nrow(hist)) {
        empty <- data.table::data.table()
        for (nm in cols) empty[[nm]] <- character()
        return(empty)
      }
      for (nm in cols) {
        if (!nm %in% names(hist)) hist[[nm]] <- NA
      }
      hist[, cols, with = FALSE]
    })

    output$history_table <- DT::renderDT({
      show <- history_display()
      DT::datatable(
        show,
        selection = "single",
        options = list(pageLength = 6, scrollX = TRUE, order = list(list(2, "desc"))),
        rownames = FALSE,
        caption = "Select a row then use View result / Load definition. Paths are not shown."
      )
    })

    selected_history_row <- shiny::reactive({
      hist <- history()
      show <- history_display()
      idx <- input$history_table_rows_selected
      if (!length(idx) || !nrow(hist) || !nrow(show)) return(NULL)
      i <- as.integer(idx[1])
      if (is.na(i) || i < 1L || i > nrow(show)) return(NULL)
      sid <- as.character(show$scenario_id[i])
      if (!nzchar(sid %||% "")) return(NULL)
      row <- hist[scenario_id == sid]
      if (!nrow(row)) return(NULL)
      row[1]
    })

    selected_history_dir <- shiny::reactive({
      row <- selected_history_row()
      if (is.null(row)) return(NULL)
      row$result_dir[1]
    })

    output$history_actions <- shiny::renderUI({
      caps <- capabilities()
      has_sel <- !is.null(selected_history_row())
      shiny::div(
        class = "download-row",
        shiny::actionButton(
          ns("hist_load"), "Load definition",
          class = "btn-sm btn-outline-primary",
          disabled = if (!has_sel) "disabled" else NULL
        ),
        shiny::actionButton(
          ns("hist_view"), "View result",
          class = "btn-sm btn-outline-primary",
          disabled = if (!has_sel) "disabled" else NULL
        ),
        shiny::actionButton(
          ns("hist_compare"), "Use in comparison",
          class = "btn-sm btn-outline-primary",
          disabled = if (!has_sel) "disabled" else NULL
        ),
        if (isTRUE(caps$can_delete)) {
          shiny::actionButton(
            ns("hist_delete"), "Delete local result",
            class = "btn-sm btn-outline-danger",
            disabled = if (!has_sel) "disabled" else NULL
          )
        } else {
          shiny::tags$span(
            class = "muted",
            title = "Deletion requires persistence permission",
            "Delete local result (disabled — read-only)"
          )
        }
      )
    })

    shiny::observeEvent(input$hist_load, {
      d <- selected_history_dir()
      if (is.null(d)) {
        rv$last_error <- "Select a scenario history row first."
        return()
      }
      res <- shock_load_persisted_result(d)
      if (!isTRUE(res$ok)) {
        rv$last_error <- res$errors %||% "Unable to load scenario definition."
        rv$active_result_error <- rv$last_error
        return()
      }
      apply_scenario_to_inputs(res$scenario)
      rv$last_error <- NULL
      rv$last_success <- paste("Loaded definition:", res$scenario$scenario_name)
    })
    shiny::observeEvent(input$hist_view, {
      d <- selected_history_dir()
      if (is.null(d)) {
        rv$last_error <- "Select a scenario history row first."
        return()
      }
      res <- shock_load_persisted_result(d)
      if (!isTRUE(res$ok)) {
        activate_result(res, source = "history")
        return()
      }
      res$manifest <- res$manifest %||% list(
        scenario_id = res$scenario$scenario_id,
        scenario_name = res$scenario$scenario_name,
        scenario_hash = res$scenario_hash,
        result_hash = res$result_hash,
        engine_version = res$engine_version,
        reconciliation_ok = isTRUE(res$reconciliation$ok)
      )
      activate_result(res, source = "history")
      shiny::showNotification(
        paste("Loaded result:", res$scenario$scenario_name),
        type = "message", duration = 4
      )
    })
    shiny::observeEvent(input$hist_compare, {
      d <- selected_history_dir()
      rv$compare_dir <- d
      if (!is.null(rv$compare_dir)) {
        shiny::updateSelectInput(session, "compare_result_dir", selected = rv$compare_dir)
        rv$last_success <- "Selected history scenario added to comparison."
        rv$last_error <- NULL
      }
    })
    shiny::observeEvent(input$hist_delete, {
      if (!persist_enabled()) {
        rv$last_error <- "Scenario deletion is disabled in read-only mode."
        return()
      }
      d <- selected_history_dir()
      if (is.null(d)) return()
      out <- shock_safe_delete_result(d, root)
      if (!isTRUE(out$ok)) {
        rv$last_error <- out$error
      } else {
        rv$history_tick <- rv$history_tick + 1L
        rv$last_error <- NULL
        rv$last_success <- "Deleted local scenario result."
      }
    })

    output$hist_msg <- shiny::renderUI({
      shiny::tagList(
        if (!is.null(rv$last_error)) shiny::p(class = "shock-error", rv$last_error),
        if (!is.null(rv$last_success)) shiny::p(class = "muted", rv$last_success),
        if (!is.null(rv$active_result_error)) {
          shiny::p(class = "shock-error", paste("Result activation:", rv$active_result_error))
        }
      )
    })

    output$download_panel <- shiny::renderUI({
      has <- !is.null(active()) && isTRUE(active()$ok)
      shiny::div(
        class = "download-row",
        if (!has) {
          shiny::p(class = "muted", shock_no_active_result_message())
        },
        shiny::downloadButton(ns("dl_def"), "Definition JSON", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_man"), "Manifest JSON", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_rep"), "Reporter CSV", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_com"), "Commodity CSV", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_sup"), "Supplier CSV", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_edge"), "Edge CSV", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_dep"), "Post-shock dependency CSV", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_path"), "Paths CSV", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_diag"), "Diagnostics CSV", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_cmp"), "Comparison CSV", class = "btn-sm btn-outline-primary"),
        shiny::downloadButton(ns("dl_report"), "Scenario report MD", class = "btn-sm btn-outline-primary")
      )
    })

    output$coverage_panel <- shiny::renderUI({
      cov <- coverage()
      s <- snap()
      ingested <- NA_character_
      d <- detailed()
      if (nrow(d) && "ingested_at" %in% names(d)) {
        ingested <- as.character(max(as.character(d$ingested_at), na.rm = TRUE))
      }
      shiny::tags$ul(
        shiny::tags$li(paste("Selected reporters:", cov$selected_reporter_count %||% NA)),
        shiny::tags$li(paste("Represented reporters:", cov$represented_reporter_count %||% NA)),
        shiny::tags$li(paste("Missing reporters:", cov$missing_reporter_count %||% NA)),
        shiny::tags$li(paste("Production status:", cov$production_status %||% "unknown")),
        shiny::tags$li(paste("Universe checksum:", cov$universe_checksum %||% "n/a")),
        shiny::tags$li(paste("Engine version:", SHOCK_ENGINE_VERSION)),
        shiny::tags$li(paste("Latest detailed ingestion:", ingested)),
        shiny::tags$li("Propagation does not use network centrality as weights."),
        shiny::tags$li("Benchmark timing claims are deferred to Phase 13.")
      )
    })

    meta_active <- shiny::reactive({
      res <- active()
      if (is.null(res) || !isTRUE(res$ok)) return(list())
      shock_result_download_meta(res)
    })

    shiny::observe({

    })

    output$dl_def <- shiny::downloadHandler(
      filename = function() shock_download_filename("scenario_definition", ext = "json"),
      content = function(file) {
        res <- active()
        jsonlite::write_json(res$scenario %||% list(), file, auto_unbox = TRUE, pretty = TRUE)
      }
    )
    output$dl_man <- shiny::downloadHandler(
      filename = function() shock_download_filename("scenario_manifest", ext = "json"),
      content = function(file) {
        res <- active()
        jsonlite::write_json(res$manifest %||% meta_active(), file, auto_unbox = TRUE, pretty = TRUE)
      }
    )
    make_csv_dl <- function(getter) {
      shiny::downloadHandler(
        filename = function() shock_download_filename("shock_export", ext = "csv"),
        content = function(file) {
          res <- active()
          dt <- if (is.null(res) || !isTRUE(res$ok)) data.table::data.table() else getter(res)
          data.table::fwrite(shock_ui_download_table(dt, meta_active()), file, bom = TRUE)
        }
      )
    }
    output$dl_rep <- make_csv_dl(function(r) r$reporter_impacts)
    output$dl_com <- make_csv_dl(function(r) r$commodity_impacts)
    output$dl_sup <- make_csv_dl(function(r) r$supplier_impacts)
    output$dl_edge <- make_csv_dl(function(r) r$edge_impacts)
    output$dl_dep <- make_csv_dl(function(r) r$post_shock_dependency)
    output$dl_path <- make_csv_dl(function(r) r$impact_paths)
    output$dl_diag <- make_csv_dl(function(r) r$diagnostics)
    output$dl_cmp <- shiny::downloadHandler(
      filename = function() shock_download_filename("shock_comparison", ext = "csv"),
      content = function(file) {
        cmp <- compare_ui()
        dt <- cmp$kpi_diff %||% data.table::data.table()
        data.table::fwrite(shock_ui_download_table(dt, meta_active()), file, bom = TRUE)
      }
    )
    output$dl_report <- shiny::downloadHandler(
      filename = function() shock_download_filename("scenario_report", ext = "md"),
      content = function(file) {
        writeLines(shock_prepare_scenario_report_md(active()), file, useBytes = TRUE)
      }
    )
  })
}
