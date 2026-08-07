object_bytes <- function(x) {
  as.numeric(utils::object.size(x))
}

profile_snapshot_memory <- function(snap) {
  if (is.null(snap) || !is.list(snap)) {
    return(data.table::data.table())
  }
  keys <- c(
    "trade_detailed_enriched", "country_year_analytics", "map_analytics",
    "map_geometry", "trade_global_enriched", "wdi_production_wide"
  )
  rows <- lapply(keys, function(k) {
    obj <- snap[[k]]
    data.table::data.table(
      object_name = k,
      memory_bytes = if (is.null(obj)) NA_real_ else object_bytes(obj),
      n_rows = if (is.data.frame(obj)) nrow(obj) else NA_integer_,
      measured_at = utc_now()
    )
  })
  out <- data.table::rbindlist(rows, fill = TRUE)
  out[, note := "object.size only; not process peak RSS"]
  out
}

profile_payload_sizes <- function(sankey = NULL, network_edges = NULL, network_nodes = NULL,
                                    path_rows = NULL, dt_rows = NULL) {
  data.table::data.table(
    payload = c("sankey", "network_edges", "network_nodes", "propagation_paths", "table_rows"),
    n_items = c(
      if (is.list(sankey)) length(sankey$links %||% sankey) else NA_integer_,
      if (!is.null(network_edges)) nrow(network_edges) else NA_integer_,
      if (!is.null(network_nodes)) nrow(network_nodes) else NA_integer_,
      if (!is.null(path_rows)) nrow(path_rows) else NA_integer_,
      if (!is.null(dt_rows)) as.integer(dt_rows) else NA_integer_
    ),
    approx_bytes = c(
      if (!is.null(sankey)) object_bytes(sankey) else NA_real_,
      if (!is.null(network_edges)) object_bytes(network_edges) else NA_real_,
      if (!is.null(network_nodes)) object_bytes(network_nodes) else NA_real_,
      if (!is.null(path_rows)) object_bytes(path_rows) else NA_real_,
      NA_real_
    ),
    rendering_boundary = "server_object_only",
    browser_measured = FALSE,
    measured_at = utc_now()
  )
}

compare_sparse_vs_dense_bytes <- function(i, j, x, dims) {

  sparse_bytes <- object_bytes(list(i = i, j = j, x = x))
  dense_bytes <- as.numeric(dims[1]) * as.numeric(dims[2]) * 8
  list(
    sparse_bytes = sparse_bytes,
    dense_bytes_estimate = dense_bytes,
    sparse_smaller = isTRUE(sparse_bytes < dense_bytes)
  )
}
