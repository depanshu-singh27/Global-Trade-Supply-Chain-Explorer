write_trade_flow_csv <- function(dt, path) {
  out <- download_safe_columns(dt)
  data.table::fwrite(out, path, bom = TRUE)
  invisible(TRUE)
}

trade_flow_table_display <- function(filtered, technical = FALSE) {
  dt <- data.table::as.data.table(filtered)
  if (!nrow(dt)) return(dt)
  cols_core <- c(
    "year", "reporter_iso3", "reporter_name", "partner_iso3", "partner_name",
    "flow_code", "flow_name", "hs_code", "commodity_description",
    "trade_value_usd", "net_weight_kg", "quantity"
  )
  cols_tech <- c(
    "reporter_gdp_current_usd", "partner_gdp_current_usd",
    "ingested_at", "request_id", "universe_checksum", "production_status",
    "hs_revision"
  )
  keep <- intersect(cols_core, names(dt))
  if (isTRUE(technical)) keep <- c(keep, intersect(cols_tech, names(dt)))
  download_safe_columns(dt[, ..keep])
}
