rank_network_nodes <- function(nodes, metric = "total_strength", top_n = 15L) {
  dt <- data.table::as.data.table(nodes)
  if (!nrow(dt) || !metric %in% names(dt)) {
    return(data.table::data.table())
  }
  dt <- dt[is.finite(get(metric))]
  data.table::setorderv(dt, c(metric, "iso3"), c(-1L, 1L))
  top_n <- as.integer(top_n %||% 15L)
  head(dt, min(top_n, nrow(dt)))
}

rank_network_corridors <- function(edges, top_n = 15L) {
  dt <- data.table::as.data.table(edges)
  if (!nrow(dt)) return(dt)
  tot <- sum(dt$trade_value_usd, na.rm = TRUE)
  dt[, share_pct := if (tot > 0) 100 * trade_value_usd / tot else NA_real_]
  data.table::setorderv(dt, c("trade_value_usd", "from_iso3", "to_iso3"), c(-1L, 1L, 1L))
  head(dt, min(as.integer(top_n %||% 15L), nrow(dt)))
}

node_corridor_profile <- function(edges, iso3, top_n = 5L) {
  dt <- data.table::as.data.table(edges)
  iso <- as.character(iso3)
  inbound <- dt[to_iso3 == iso]
  outbound <- dt[from_iso3 == iso]
  list(
    inbound = rank_network_corridors(inbound, top_n = top_n),
    outbound = rank_network_corridors(outbound, top_n = top_n)
  )
}

selected_node_profile <- function(nodes, edges, iso3,
                                    detailed = NULL,
                                    year_min = NULL,
                                    year_max = NULL) {
  nd <- data.table::as.data.table(nodes)
  iso <- as.character(iso3 %||% "")
  if (!nrow(nd) || !nzchar(iso) || !iso %in% nd$iso3) {
    return(NULL)
  }
  row <- nd[iso3 == iso][1]
  corridors <- node_corridor_profile(edges, iso, top_n = 5L)
  top_hs <- data.table::data.table()
  if (!is.null(detailed) && nrow(detailed)) {
    d <- prepare_detailed_trade(detailed)
    if (!is.null(year_min) && !is.na(year_min)) d <- d[year >= as.integer(year_min)]
    if (!is.null(year_max) && !is.na(year_max)) d <- d[year <= as.integer(year_max)]
    d <- d[reporter_iso3 == iso | partner_iso3 == iso]
    if (nrow(d)) {
      top_hs <- d[, .(trade_value_usd = sum(trade_value_usd, na.rm = TRUE)),
                  by = .(hs_code, commodity_description)]
      data.table::setorderv(top_hs, c("trade_value_usd", "hs_code"), c(-1L, 1L))
      top_hs <- head(top_hs, 5L)
    }
  }
  list(
    iso3 = row$iso3,
    display_name = row$display_name,
    reporting_status = row$reporting_status,
    reporting_status_label = reporting_status_label(row$reporting_status),
    is_selected_reporter = isTRUE(row$is_selected_reporter),
    represented_as_reporter = isTRUE(row$represented_as_reporter),
    represented_as_partner = isTRUE(row$represented_as_partner),
    total_strength = row$total_strength,
    in_strength = row$in_strength,
    out_strength = row$out_strength,
    degree = row$degree,
    pagerank = row$pagerank,
    betweenness = row$betweenness,
    community = row$community,
    component_id = row$component_id,
    gdp_current_usd = row$gdp_current_usd,
    population_total = row$population_total,
    inbound = corridors$inbound,
    outbound = corridors$outbound,
    top_hs4 = top_hs
  )
}

network_kpi_summary <- function(built, stats, coverage = NULL) {
  list(
    nodes = stats$node_count %||% 0L,
    edges = stats$edge_count %||% 0L,
    visible_value = built$visible_value %||% 0,
    coverage_pct = built$coverage_pct,
    density = stats$directed_density,
    components = stats$weak_component_count,
    represented = coverage$represented_reporter_count %||% NA_integer_,
    selected = coverage$selected_reporter_count %||% NA_integer_,
    status = coverage$production_status %||% "partial"
  )
}

network_accessibility_summary <- function(built, stats, nodes, mode = "exports") {
  if (is.null(built) || (built$visible_n %||% 0) == 0L) {
    return("No network edges are available for the current filters.")
  }
  top <- rank_network_nodes(nodes, "total_strength", top_n = 1L)
  leader <- if (nrow(top)) {
    paste0(top$display_name[1], " (", top$iso3[1], ")")
  } else "Unavailable"
  sprintf(
    paste0(
      "%s network with %d nodes and %d visible edges ",
      "(%.1f%% of eligible trade value). ",
      "Largest total strength: %s. ",
      "%d weakly connected component(s). ",
      "Centrality describes only this filtered available-observation network."
    ),
    network_mode_label(mode),
    stats$node_count %||% 0L,
    stats$edge_count %||% 0L,
    built$coverage_pct %||% NA_real_,
    leader,
    stats$weak_component_count %||% 0L
  )
}

build_full_trade_network <- function(detailed,
                                       mode = "exports",
                                       year_min = NULL,
                                       year_max = NULL,
                                       reporters = NULL,
                                       partners = NULL,
                                       hs_codes = NULL,
                                       top_n = NW_DEFAULT_TOP_EDGES,
                                       min_value = NULL,
                                       focus_iso3 = NULL,
                                       ego_order = 1L,
                                       layout = "fr",
                                       selected_reporters = character(),
                                       selected_partners = character(),
                                       represented_reporters = character()) {
  built <- construct_network_edges(
    detailed,
    mode = mode,
    year_min = year_min,
    year_max = year_max,
    reporters = reporters,
    partners = partners,
    hs_codes = hs_codes,
    top_n = top_n,
    min_value = min_value,
    exclude_self = TRUE
  )

  nodes0 <- build_network_nodes(
    built$visible_edges,
    detailed = detailed,
    selected_reporters = selected_reporters,
    selected_partners = selected_partners,
    represented_reporters = represented_reporters
  )
  directed <- !identical(mode, "undirected")
  g <- create_trade_igraph(built$visible_edges, nodes0, directed = directed)

  if (!is.null(focus_iso3) && nzchar(as.character(focus_iso3)) &&
      !identical(as.character(focus_iso3), "__ALL__")) {
    g <- extract_ego_network(g, focus_iso3, order = ego_order)

    if (igraph::ecount(g) > 0L) {
      el <- igraph::as_data_frame(g, what = "edges")
      data.table::setDT(el)
      data.table::setnames(el, c("from", "to"), c("from_iso3", "to_iso3"), skip_absent = TRUE)
      built$visible_edges <- built$visible_edges[
        paste(from_iso3, to_iso3) %in% paste(el$from_iso3, el$to_iso3)
      ]
      built$visible_n <- nrow(built$visible_edges)
      built$visible_value <- sum(built$visible_edges$trade_value_usd, na.rm = TRUE)
      built$coverage_pct <- if ((built$eligible_value %||% 0) > 0) {
        100 * built$visible_value / built$eligible_value
      } else NA_real_
    } else {
      built$visible_edges <- built$visible_edges[0]
      built$visible_n <- 0L
      built$visible_value <- 0
      built$coverage_pct <- NA_real_
    }
  }

  enriched <- enrich_graph_metrics(g)
  g2 <- enriched$graph
  nodes <- enriched$nodes
  coords <- layout_network_coordinates(g2, method = layout)
  if (nrow(nodes) && nrow(coords)) {
    nodes <- merge(nodes, coords, by = "iso3", all.x = TRUE, sort = FALSE)
  }
  stats <- network_level_stats(g2, built$visible_edges, nodes)

  list(
    built = built,
    graph = g2,
    nodes = nodes,
    edges = built$visible_edges,
    stats = stats,
    warnings = enriched$warnings,
    layout = layout
  )
}
