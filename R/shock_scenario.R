default_shock_scenario <- function() {
  list(
    scenario_id = NA_character_,
    scenario_name = "Untitled scenario",
    scenario_description = "",
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    baseline_year_start = 2024L,
    baseline_year_end = 2024L,
    target_supplier_iso3 = character(),
    target_hs_codes = character(),
    target_reporter_iso3 = character(),
    shock_type = "commodity_specific_supplier_reduction",
    shock_size_pct = 30,
    substitution_mode = "capacity_constrained",
    substitution_capacity_pct = 25,
    substitution_allocation_method = "proportional_capacity",
    maximum_substitute_supplier_share = 1,
    propagation_mode = "direct_only",
    maximum_propagation_steps = 1L,
    propagation_decay = 1,
    propagation_pass_through = 1,
    minimum_propagated_value_usd = 0,
    minimum_dependency_share = 0,
    include_macro_normalisation = TRUE,
    acknowledge_partial_coverage = FALSE,
    random_seed = NA_integer_,
    universe_version = EXPECTED_UNIVERSE_CHECKSUM,
    engine_version = SHOCK_ENGINE_VERSION
  )
}

normalize_shock_scenario <- function(scenario) {
  base <- default_shock_scenario()
  if (is.null(scenario)) scenario <- list()
  if (!is.list(scenario)) stop("Scenario must be a list/JSON object.", call. = FALSE)
  out <- merge_config(base, scenario)

  out$scenario_name <- as.character(out$scenario_name %||% "Untitled scenario")[1]
  out$scenario_description <- as.character(out$scenario_description %||% "")[1]
  out$baseline_year_start <- as.integer(out$baseline_year_start)[1]
  out$baseline_year_end <- as.integer(out$baseline_year_end)[1]
  if (is.na(out$baseline_year_start) || is.na(out$baseline_year_end)) {
    stop("baseline_year_start and baseline_year_end are required.", call. = FALSE)
  }
  if (out$baseline_year_start > out$baseline_year_end) {
    tmp <- out$baseline_year_start
    out$baseline_year_start <- out$baseline_year_end
    out$baseline_year_end <- tmp
  }

  out$target_supplier_iso3 <- unique(as.character(out$target_supplier_iso3 %||% character()))
  out$target_supplier_iso3 <- out$target_supplier_iso3[nzchar(out$target_supplier_iso3)]
  out$target_hs_codes <- unique(as.character(out$target_hs_codes %||% character()))
  out$target_hs_codes <- out$target_hs_codes[nzchar(out$target_hs_codes)]
  out$target_reporter_iso3 <- unique(as.character(out$target_reporter_iso3 %||% character()))
  out$target_reporter_iso3 <- out$target_reporter_iso3[
    nzchar(out$target_reporter_iso3) & !(out$target_reporter_iso3 %in% c("__ALL__", "ALL"))
  ]

  out$shock_type <- as.character(out$shock_type %||% "commodity_specific_supplier_reduction")[1]
  out$shock_size_pct <- as.numeric(out$shock_size_pct)[1]
  out$substitution_mode <- as.character(out$substitution_mode %||% "capacity_constrained")[1]
  out$substitution_capacity_pct <- as.numeric(out$substitution_capacity_pct %||% 0)[1]
  out$maximum_substitute_supplier_share <- as.numeric(
    out$maximum_substitute_supplier_share %||% 1
  )[1]
  out$propagation_mode <- as.character(out$propagation_mode %||% "direct_only")[1]
  out$maximum_propagation_steps <- as.integer(out$maximum_propagation_steps %||% 1L)[1]
  out$propagation_decay <- as.numeric(out$propagation_decay %||% 1)[1]
  out$propagation_pass_through <- as.numeric(out$propagation_pass_through %||% 1)[1]
  out$minimum_propagated_value_usd <- as.numeric(out$minimum_propagated_value_usd %||% 0)[1]
  out$minimum_dependency_share <- as.numeric(out$minimum_dependency_share %||% 0)[1]
  out$include_macro_normalisation <- isTRUE(out$include_macro_normalisation)
  out$acknowledge_partial_coverage <- isTRUE(out$acknowledge_partial_coverage)
  out$universe_version <- as.character(out$universe_version %||% EXPECTED_UNIVERSE_CHECKSUM)[1]
  out$engine_version <- as.character(out$engine_version %||% SHOCK_ENGINE_VERSION)[1]
  out$created_at <- as.character(out$created_at %||% format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))[1]

  if (is.null(out$scenario_id) || !nzchar(as.character(out$scenario_id)) ||
      is.na(out$scenario_id)) {
    out$scenario_id <- paste0("scn_", substr(shock_scenario_hash(out), 1, 16))
  } else {
    out$scenario_id <- sanitize_shock_token(out$scenario_id, "scn")
  }
  out
}

shock_scenario_semantic_list <- function(scenario) {
  if (is.null(scenario)) scenario <- list()
  list(
    baseline_year_start = as.integer(scenario$baseline_year_start %||% 2024L)[1],
    baseline_year_end = as.integer(scenario$baseline_year_end %||% 2024L)[1],
    target_supplier_iso3 = sort(unique(as.character(scenario$target_supplier_iso3 %||% character()))),
    target_hs_codes = sort(unique(as.character(scenario$target_hs_codes %||% character()))),
    target_reporter_iso3 = sort(unique(as.character(scenario$target_reporter_iso3 %||% character()))),
    shock_type = as.character(scenario$shock_type %||% "commodity_specific_supplier_reduction")[1],
    shock_size_pct = as.numeric(scenario$shock_size_pct %||% 30)[1],
    substitution_mode = as.character(scenario$substitution_mode %||% "capacity_constrained")[1],
    substitution_capacity_pct = as.numeric(scenario$substitution_capacity_pct %||% 25)[1],
    maximum_substitute_supplier_share = as.numeric(scenario$maximum_substitute_supplier_share %||% 1)[1],
    propagation_mode = as.character(scenario$propagation_mode %||% "direct_only")[1],
    maximum_propagation_steps = as.integer(scenario$maximum_propagation_steps %||% 1L)[1],
    propagation_decay = as.numeric(scenario$propagation_decay %||% 1)[1],
    propagation_pass_through = as.numeric(scenario$propagation_pass_through %||% 1)[1],
    minimum_propagated_value_usd = as.numeric(scenario$minimum_propagated_value_usd %||% 0)[1],
    minimum_dependency_share = as.numeric(scenario$minimum_dependency_share %||% 0)[1],
    include_macro_normalisation = isTRUE(scenario$include_macro_normalisation %||% TRUE),
    acknowledge_partial_coverage = isTRUE(scenario$acknowledge_partial_coverage %||% FALSE),
    universe_version = as.character(scenario$universe_version %||% EXPECTED_UNIVERSE_CHECKSUM)[1],
    engine_version = as.character(scenario$engine_version %||% SHOCK_ENGINE_VERSION)[1]
  )
}

shock_scenario_hash <- function(scenario) {
  payload <- jsonlite::toJSON(
    shock_scenario_semantic_list(scenario),
    auto_unbox = TRUE,
    digits = 10,
    null = "null"
  )
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(as.character(payload), algo = "xxhash64"))
  }
  paste0(
    sprintf("%08x", sum(utf8ToInt(as.character(payload))) %% (2^31 - 1)),
    sprintf("%08x", nchar(as.character(payload)))
  )
}

read_shock_scenario_file <- function(path) {
  if (!file.exists(path)) stop("Scenario file not found: ", path, call. = FALSE)
  raw <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  normalize_shock_scenario(raw)
}

write_shock_scenario_file <- function(scenario, path) {
  sc <- normalize_shock_scenario(scenario)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(sc, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}
