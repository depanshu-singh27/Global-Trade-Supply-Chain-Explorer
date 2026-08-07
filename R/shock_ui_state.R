shock_ui_methodology_notice <- function() {
  paste(
    "The simulator applies user-defined supply reductions to observed import relationships",
    "and estimates residual exposure after configurable supplier substitution.",
    "Results are deterministic scenario sensitivities, not forecasts of realised economic losses."
  )
}

shock_ui_partial_notice <- function(represented, selected, coverage = NULL) {
  shock_partial_status_notice(represented, selected, coverage = coverage)
}

shock_builder_defaults <- function(coverage = NULL, years = integer()) {
  years <- as.integer(years)
  years <- years[is.finite(years)]
  y_end <- if (length(years)) max(years) else 2024L
  y_start <- y_end
  list(
    scenario_name = "New scenario",
    scenario_description = "",
    baseline_year_start = y_start,
    baseline_year_end = y_end,
    target_supplier_iso3 = character(),
    target_reporter_iso3 = character(),
    target_hs_codes = character(),
    shock_type = "commodity_specific_supplier_reduction",
    shock_size_pct = 30,
    substitution_mode = "capacity_constrained",
    substitution_capacity_pct = 25,
    maximum_substitute_supplier_share = 100,
    enable_max_share = FALSE,
    propagation_mode = "direct_only",
    maximum_propagation_steps = 1L,
    propagation_decay = 1,
    minimum_propagated_value_usd = 0,
    minimum_dependency_share = 0,
    include_macro_normalisation = TRUE,
    acknowledge_partial_coverage = identical(coverage$production_status, "complete"),
    universe_version = coverage$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM,
    engine_version = SHOCK_ENGINE_VERSION
  )
}

shock_type_choices <- function() {
  c(
    "Supplier reduction (selected commodities)" = "supplier_export_reduction",
    "Commodity-specific supplier reduction" = "commodity_specific_supplier_reduction",
    "Reporter-specific bilateral reduction" = "reporter_specific_bilateral_reduction"
  )
}

shock_substitution_mode_choices <- function() {
  c(
    "No substitution" = "none",
    "Proportional existing suppliers" = "proportional",
    "Capacity-constrained existing suppliers" = "capacity_constrained",
    "Diversification-prioritised substitution" = "diversification"
  )
}

shock_propagation_mode_choices <- function() {
  c(
    "Direct only" = "direct_only",
    "First-order propagation" = "first_order",
    "Bounded multi-step propagation" = "multi_step"
  )
}

shock_reporter_rank_metric_choices <- function() {
  c(
    "Residual unmet value" = "residual_unmet_value_usd",
    "Residual % of total HS-85 imports" = "residual_unmet_pct_total_hs85_imports",
    "Residual unmet imports as % of GDP" = "residual_unmet_pct_gdp",
    "Direct disruption" = "direct_disrupted_value_usd",
    "Substitution allocated" = "substitution_allocated_value_usd"
  )
}

shock_map_metric_choices <- function() {
  shock_reporter_rank_metric_choices()
}

shock_control_choices <- function(baseline, coverage = NULL) {
  bl <- data.table::as.data.table(baseline)
  empty <- list(
    years = integer(),
    suppliers = character(),
    supplier_labels = character(),
    reporters = character(),
    reporter_labels = character(),
    hs_codes = character(),
    hs_labels = character(),
    represented_reporters = coverage$represented_reporters %||% character(),
    missing_reporters = coverage$missing_reporters %||% character()
  )
  if (!nrow(bl)) return(empty)

  years <- sort(unique(c(
    as.integer(bl$baseline_year_start),
    as.integer(bl$baseline_year_end)
  )))
  years <- years[is.finite(years)]

  sup <- unique(bl[, .(supplier_iso3, supplier_name)])
  data.table::setorder(sup, supplier_iso3)
  suppliers <- sup$supplier_iso3
  supplier_labels <- sprintf("%s — %s", suppliers, coalesce_chr(sup$supplier_name, suppliers))
  names(suppliers) <- supplier_labels

  rep <- unique(bl[, .(reporter_iso3, reporter_name)])
  data.table::setorder(rep, reporter_iso3)
  reporters <- rep$reporter_iso3
  reporter_labels <- sprintf("%s — %s", reporters, coalesce_chr(rep$reporter_name, reporters))
  names(reporters) <- reporter_labels

  hs <- unique(bl[, .(hs_code, commodity_description)])
  data.table::setorder(hs, hs_code)
  hs_codes <- hs$hs_code
  hs_labels <- sprintf(
    "%s — %s",
    hs_codes,
    coalesce_chr(hs$commodity_description, hs_codes)
  )
  names(hs_codes) <- hs_labels

  list(
    years = years,
    suppliers = suppliers,
    supplier_labels = supplier_labels,
    reporters = reporters,
    reporter_labels = reporter_labels,
    hs_codes = hs_codes,
    hs_labels = hs_labels,
    represented_reporters = coverage$represented_reporters %||% reporters,
    missing_reporters = coverage$missing_reporters %||% character()
  )
}

coalesce_chr <- function(x, fallback) {
  x <- as.character(x)
  ifelse(is.na(x) | !nzchar(x), as.character(fallback), x)
}

shock_control_visibility <- function(substitution_mode, propagation_mode, shock_type) {
  sub_mode <- as.character(substitution_mode %||% "capacity_constrained")[1]
  prop_mode <- as.character(propagation_mode %||% "direct_only")[1]
  stype <- as.character(shock_type %||% "commodity_specific_supplier_reduction")[1]
  list(
    show_capacity = sub_mode %in% c("capacity_constrained", "diversification"),
    show_max_share = sub_mode != "none",
    show_propagation_advanced = prop_mode %in% c("first_order", "multi_step"),
    show_multi_step = identical(prop_mode, "multi_step"),
    require_reporters = identical(stype, "reporter_specific_bilateral_reduction"),
    require_hs = stype %in% c(
      "commodity_specific_supplier_reduction",
      "reporter_specific_bilateral_reduction"
    )
  )
}

shock_ui_to_scenario <- function(inputs, coverage = NULL) {
  max_share_pct <- as.numeric(inputs$maximum_substitute_supplier_share %||% 100)
  if (!isTRUE(inputs$enable_max_share)) max_share_pct <- 100
  max_share <- max(0, min(100, max_share_pct)) / 100

  list(
    scenario_name = as.character(inputs$scenario_name %||% "Untitled scenario")[1],
    scenario_description = as.character(inputs$scenario_description %||% "")[1],
    baseline_year_start = as.integer(inputs$baseline_year_start %||% 2024L)[1],
    baseline_year_end = as.integer(inputs$baseline_year_end %||% inputs$baseline_year_start %||% 2024L)[1],
    target_supplier_iso3 = as.character(inputs$target_supplier_iso3 %||% character()),
    target_hs_codes = as.character(inputs$target_hs_codes %||% character()),
    target_reporter_iso3 = as.character(inputs$target_reporter_iso3 %||% character()),
    shock_type = as.character(inputs$shock_type %||% "commodity_specific_supplier_reduction")[1],
    shock_size_pct = as.numeric(inputs$shock_size_pct %||% 30)[1],
    substitution_mode = as.character(inputs$substitution_mode %||% "capacity_constrained")[1],
    substitution_capacity_pct = as.numeric(inputs$substitution_capacity_pct %||% 25)[1],
    maximum_substitute_supplier_share = max_share,
    propagation_mode = as.character(inputs$propagation_mode %||% "direct_only")[1],
    maximum_propagation_steps = as.integer(inputs$maximum_propagation_steps %||% 1L)[1],
    propagation_decay = as.numeric(inputs$propagation_decay %||% 1)[1],
    minimum_propagated_value_usd = as.numeric(inputs$minimum_propagated_value_usd %||% 0)[1],
    minimum_dependency_share = as.numeric(inputs$minimum_dependency_share %||% 0)[1],
    include_macro_normalisation = isTRUE(inputs$include_macro_normalisation %||% TRUE),
    acknowledge_partial_coverage = isTRUE(inputs$acknowledge_partial_coverage),
    universe_version = as.character(
      inputs$universe_version %||% coverage$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM
    )[1],
    engine_version = SHOCK_ENGINE_VERSION
  )
}

shock_partial_ack_required <- function(coverage) {
  !isTRUE(coverage_is_selected_universe_complete(coverage)) &&
    !identical(coverage$production_status, "complete")
}

shock_ui_capabilities <- function(runtime_cfg = NULL, history_n = 0L) {
  rt <- runtime_cfg %||% get_runtime_config()
  can_persist <- runtime_allows_scenario_persistence(rt)
  list(
    can_view = TRUE,
    can_validate = TRUE,
    can_preview = TRUE,
    can_run_in_memory = TRUE,
    can_load_history = isTRUE(as.integer(history_n %||% 0L) > 0L),
    can_download_safe_outputs = TRUE,
    can_persist = can_persist,
    can_delete = can_persist,
    read_only_notice = paste(
      "Scenario execution is available in memory.",
      "Persistent saving and deletion are disabled in read-only mode."
    )
  )
}

shock_no_active_result_message <- function() {
  paste(
    "No active scenario result. Configure and run a scenario,",
    "or select a saved scenario in Scenario History and choose View result."
  )
}

shock_duplicate_scenario_definition <- function(scenario, new_name = NULL) {
  sc <- normalize_shock_scenario(scenario)
  sc$scenario_id <- NA_character_
  sc$scenario_name <- as.character(new_name %||% paste(sc$scenario_name, "(copy)"))[1]
  sc$created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  normalize_shock_scenario(sc)
}

shock_example_catalog <- function(root = find_project_root(), baseline = NULL, coverage = NULL) {
  dirs <- shock_scenario_dirs(root)
  files <- list.files(dirs$examples, pattern = "\\.json$", full.names = TRUE)
  if (!length(files)) {
    return(data.table::data.table(
      file = character(), label = character(), available = logical(), reason = character()
    ))
  }
  rows <- lapply(files, function(f) {
    sc <- tryCatch(read_shock_scenario_file(f), error = function(e) NULL)
    if (is.null(sc)) {
      return(data.table::data.table(
        file = basename(f), label = basename(f), available = FALSE, reason = "Unreadable"
      ))
    }
    avail <- TRUE
    reason <- ""
    if (!is.null(baseline) && nrow(baseline)) {
      tg <- tryCatch(select_shock_target_edges(baseline, sc), error = function(e) NULL)
      if (is.null(tg) || !nrow(tg)) {

        avail <- FALSE
        reason <- "No matching targets in active baseline period"
      }
    }
    data.table::data.table(
      file = basename(f),
      path = f,
      scenario_id = sc$scenario_id %||% basename(f),
      label = sc$scenario_name %||% basename(f),
      available = avail,
      reason = reason,
      shock_size_pct = sc$shock_size_pct,
      substitution_mode = sc$substitution_mode,
      propagation_mode = sc$propagation_mode,
      baseline_year_start = sc$baseline_year_start,
      baseline_year_end = sc$baseline_year_end
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

shock_example_availability <- function(example_scenario, detailed, coverage = NULL) {
  sc <- normalize_shock_scenario(example_scenario)
  bl <- build_shock_baseline(
    detailed,
    year_min = sc$baseline_year_start,
    year_max = sc$baseline_year_end,
    coverage = coverage,
    universe_version = coverage$universe_checksum %||% sc$universe_version
  )$baseline
  tg <- select_shock_target_edges(bl, sc)
  list(available = nrow(tg) > 0L, n_targets = nrow(tg), baseline_rows = nrow(bl))
}
