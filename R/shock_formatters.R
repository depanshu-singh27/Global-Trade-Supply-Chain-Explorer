SHOCK_ENGINE_VERSION <- "1.0.0-phase10"
SHOCK_VALUE_TOLERANCE <- 1e-6

shock_substitution_modes <- function() {
  c(
    "none",
    "proportional",
    "capacity_constrained",
    "diversification"
  )
}

shock_propagation_modes <- function() {
  c("direct_only", "first_order", "multi_step")
}

shock_types <- function() {
  c(
    "supplier_export_reduction",
    "commodity_specific_supplier_reduction",
    "reporter_specific_bilateral_reduction"
  )
}

sanitize_shock_token <- function(x, fallback = "scenario") {
  sanitize_download_token(x, fallback)
}

shock_download_filename <- function(prefix, ..., ext = "csv") {
  parts <- c(
    sanitize_shock_token(prefix, "shock"),
    vapply(list(...), function(z) sanitize_shock_token(z, "x"), character(1))
  )
  paste0(paste(parts, collapse = "_"), ".", ext)
}

format_shock_usd <- function(x, scale = "auto") {
  format_trade_value_scaled(x, scale = scale)
}

format_shock_pct <- function(x, digits = 1L) {
  x <- sanitize_chart_numeric(x)
  if (length(x) != 1L) {
    return(vapply(x, format_shock_pct, character(1), digits = digits))
  }
  if (is.na(x) || !is.finite(x)) return("Unavailable")
  sprintf("%.*f%%", digits, x)
}

shock_methodology_notice <- function() {
  paste(
    "The simulator applies user-defined supply reductions to observed import relationships",
    "and estimates direct residual exposure after configurable supplier substitution.",
    "Results are scenario-based analytical sensitivities, not forecasts of realised economic losses."
  )
}

shock_partial_status_notice <- function(represented, selected, coverage = NULL) {
  if (!is.null(coverage) && length(coverage)) {
    return(detailed_coverage_notice(coverage, context = "shock"))
  }
  cov <- list(
    represented_reporter_count = as.integer(represented %||% 0L),
    selected_reporter_count = as.integer(selected %||% 0L),
    missing_reporter_count = max(
      0L,
      as.integer(selected %||% 0L) - as.integer(represented %||% 0L)
    ),
    production_status = if (
      as.integer(selected %||% 0L) > 0L &&
      identical(as.integer(represented %||% 0L), as.integer(selected %||% 0L))
    ) "complete" else "partial",
    request_summary = list(planned = 0L, active = 0L, quota_blocked = 0L)
  )
  detailed_coverage_notice(cov, context = "shock")
}

safe_shock_pct <- function(x) {
  x <- sanitize_chart_numeric(as.numeric(x))
  if (!is.finite(x)) return(NA_real_)
  max(0, min(100, x))
}

clamp01 <- function(x) {
  x <- sanitize_chart_numeric(as.numeric(x))
  ifelse(!is.finite(x), NA_real_, pmax(0, pmin(1, x)))
}
