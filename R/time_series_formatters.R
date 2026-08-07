TS_AGGREGATE_ISO3 <- c("EUR", "WLD", "W00", "ASE", "S19", "X99")

ts_metric_defs <- function() {
  list(
    imports = list(column = "imports_value_usd", label = "Imports", signed = FALSE, allow_pct = TRUE),
    exports = list(column = "exports_value_usd", label = "Exports", signed = FALSE, allow_pct = TRUE),
    total_trade = list(column = "total_trade_value_usd", label = "Total trade", signed = FALSE, allow_pct = TRUE),
    trade_balance = list(column = "trade_balance_usd", label = "Trade balance", signed = TRUE, allow_pct = FALSE),
    total_trade_pct_gdp = list(column = "total_trade_pct_gdp", label = "Total trade / GDP", signed = FALSE, allow_pct = TRUE),
    balance_pct_gdp = list(column = "trade_balance_pct_gdp", label = "Trade balance / GDP", signed = TRUE, allow_pct = FALSE),
    trade_per_capita = list(column = "total_trade_per_capita_usd", label = "Total trade per capita", signed = FALSE, allow_pct = TRUE)
  )
}

ts_metric_label <- function(metric) {
  d <- ts_metric_defs()
  if (!metric %in% names(d)) return(as.character(metric))
  d[[metric]]$label
}

ts_metric_column <- function(metric) {
  d <- ts_metric_defs()
  d[[metric]]$column %||% metric
}

ts_metric_allows_pct_growth <- function(metric) {
  d <- ts_metric_defs()
  isTRUE(d[[metric]]$allow_pct)
}

ts_metric_is_signed <- function(metric) {
  d <- ts_metric_defs()
  isTRUE(d[[metric]]$signed)
}

format_ts_value <- function(x, metric = "total_trade", transform = "absolute") {
  x <- sanitize_chart_numeric(x)
  if (length(x) != 1L) {
    return(vapply(x, format_ts_value, character(1), metric = metric, transform = transform))
  }
  if (is.na(x)) return("Unavailable")
  if (identical(transform, "yoy") || identical(transform, "cagr") ||
      identical(transform, "share")) {
    return(format_pct(x, digits = 1L))
  }
  if (identical(transform, "index")) {
    return(sprintf("%.1f", x))
  }
  if (metric %in% c("total_trade_pct_gdp", "balance_pct_gdp")) {
    return(format_pct(x, digits = 1L))
  }
  if (identical(metric, "trade_per_capita")) {
    return(format_per_person(x))
  }
  format_usd_compact(x)
}

sanitize_ts_filename_token <- function(x, fallback = "all") {
  x <- as.character(x %||% fallback)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- fallback
  substr(x, 1L, 48L)
}

ts_download_filename <- function(prefix, ...) {
  parts <- c(sanitize_ts_filename_token(prefix, "trade_timeseries"),
             vapply(list(...), sanitize_ts_filename_token, character(1)))
  parts <- parts[nzchar(parts)]
  paste0(paste(parts, collapse = "_"), ".csv")
}
