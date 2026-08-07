write_network_csv <- function(dt, path) {
  out <- data.table::as.data.table(dt)
  drop <- grep("path|url|header|secret|token|key|raw_file|cache|request_id",
               names(out), ignore.case = TRUE, value = TRUE)
  if (length(drop)) out[, (drop) := NULL]
  data.table::fwrite(out, path, bom = TRUE)
  invisible(TRUE)
}

sanitize_graphml_attrs <- function(dt) {
  out <- data.table::as.data.table(dt)
  drop <- grep("path|url|header|secret|token|key|raw_file|cache|request",
               names(out), ignore.case = TRUE, value = TRUE)
  if (length(drop)) out[, (drop) := NULL]
  out
}

write_network_graphml <- function(g, path) {
  if (is.null(g) || igraph::vcount(g) == 0L) {
    writeLines(
      c('<?xml version="1.0" encoding="UTF-8"?>',
        '<graphml xmlns="http://graphml.graphdrawing.org/xmlns">',
        '  <graph edgedefault="directed"/>',
        '</graphml>'),
      path
    )
    return(invisible(TRUE))
  }

  bad_v <- grep("path|url|secret|token|key|raw_file|cache",
                igraph::vertex_attr_names(g), ignore.case = TRUE, value = TRUE)
  bad_e <- grep("path|url|secret|token|key|raw_file|cache|request",
                igraph::edge_attr_names(g), ignore.case = TRUE, value = TRUE)
  for (a in bad_v) g <- igraph::delete_vertex_attr(g, a)
  for (a in bad_e) g <- igraph::delete_edge_attr(g, a)
  igraph::write_graph(g, file = path, format = "graphml")
  invisible(TRUE)
}

network_stats_download_dt <- function(stats, built, coverage = NULL) {
  data.table::data.table(
    metric = c(
      "node_count", "edge_count", "directed_density", "weak_component_count",
      "largest_weak_component_share_pct", "reciprocity", "average_degree",
      "total_observed_trade_value_usd", "top5_edge_share_pct",
      "top5_node_strength_share_pct", "edge_hhi", "node_strength_hhi",
      "eligible_edges", "visible_edges", "visible_value_coverage_pct",
      "self_edges_excluded", "self_edge_value_excluded",
      "production_status", "represented_reporters", "selected_reporters",
      "universe_checksum"
    ),
    value = c(
      stats$node_count, stats$edge_count, stats$directed_density,
      stats$weak_component_count, stats$largest_weak_component_share_pct,
      stats$reciprocity, stats$average_degree, stats$total_observed_trade_value_usd,
      stats$top5_edge_share_pct, stats$top5_node_strength_share_pct,
      stats$edge_hhi, stats$node_strength_hhi,
      built$eligible_n, built$visible_n, built$coverage_pct,
      built$self_excluded_count, built$self_excluded_value,
      coverage$production_status %||% NA_character_,
      coverage$represented_reporter_count %||% NA_integer_,
      coverage$selected_reporter_count %||% NA_integer_,
      coverage$universe_checksum %||% NA_character_
    )
  )
}
