map_metric_defs <- function() {
  list(
    trade_balance = list(
      column = "trade_balance_usd", label = "Trade balance",
      signed = TRUE, unit = "Current US$"
    ),
    imports = list(
      column = "imports_value_usd", label = "Imports",
      signed = FALSE, unit = "Current US$"
    ),
    exports = list(
      column = "exports_value_usd", label = "Exports",
      signed = FALSE, unit = "Current US$"
    ),
    total_trade = list(
      column = "total_trade_value_usd", label = "Total trade",
      signed = FALSE, unit = "Current US$"
    ),
    total_trade_pct_gdp = list(
      column = "total_trade_pct_gdp", label = "Total trade / GDP",
      signed = FALSE, unit = "Percent of GDP"
    ),
    balance_pct_gdp = list(
      column = "trade_balance_pct_gdp", label = "Trade balance / GDP",
      signed = TRUE, unit = "Percent of GDP"
    ),
    trade_per_capita = list(
      column = "total_trade_per_capita_usd", label = "Total trade per capita",
      signed = FALSE, unit = "Current US$ per person"
    )
  )
}

map_metric_label <- function(metric) {
  if (is.null(metric) || length(metric) < 1L || !nzchar(as.character(metric[[1]]) %||% "")) {
    return("Metric")
  }
  metric <- as.character(metric[[1]])
  defs <- map_metric_defs()
  if (!metric %in% names(defs)) return(metric)
  defs[[metric]]$label
}

map_metric_is_signed <- function(metric) {
  if (is.null(metric) || length(metric) < 1L) return(FALSE)
  metric <- as.character(metric[[1]])
  defs <- map_metric_defs()
  isTRUE(defs[[metric]]$signed)
}

format_map_metric_value <- function(x, metric) {
  x <- sanitize_chart_numeric(x)
  if (length(x) != 1L) {
    return(vapply(x, format_map_metric_value, character(1), metric = metric))
  }
  if (is.na(x)) return("No value available")
  if (metric %in% c("total_trade_pct_gdp", "balance_pct_gdp")) {
    return(format_pct(x, digits = 1L))
  }
  if (identical(metric, "trade_per_capita")) {
    return(format_per_person(x, digits = 0L))
  }
  format_usd_compact(x)
}

sanitize_map_filename_token <- function(x, fallback = "all") {
  x <- as.character(x %||% fallback)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- fallback
  substr(x, 1L, 48L)
}

map_download_filename <- function(prefix, year, extra = NULL, ext = "csv") {
  parts <- c(
    sanitize_map_filename_token(prefix, "trade_balance_map"),
    sanitize_map_filename_token(year, "year")
  )
  if (!is.null(extra) && nzchar(extra)) {
    parts <- c(parts, sanitize_map_filename_token(extra))
  }
  paste0(paste(parts, collapse = "_"), ".", ext)
}
