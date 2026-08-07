trade_value_to_distance <- function(trade_value_usd, floor = 1) {
  w <- sanitize_chart_numeric(as.numeric(trade_value_usd))
  floor <- max(as.numeric(floor %||% 1), .Machine$double.eps)
  w[!is.finite(w) | w <= 0] <- NA_real_

  mx <- suppressWarnings(max(w, na.rm = TRUE))
  if (!is.finite(mx) || mx <= 0) {
    return(rep(NA_real_, length(w)))
  }
  scaled <- pmax(w / mx, floor / mx)
  dist <- 1 / scaled
  dist[!is.finite(dist) | dist <= 0] <- NA_real_
  dist
}

enrich_graph_metrics <- function(g) {
  if (igraph::vcount(g) == 0L) {
    return(list(graph = g, nodes = data.table::data.table(), warnings = character()))
  }
  warnings <- character()
  directed <- igraph::is_directed(g)
  w <- igraph::E(g)$trade_value_usd
  w <- sanitize_chart_numeric(w)
  w[!is.finite(w) | w < 0] <- 0
  igraph::E(g)$trade_value_usd <- w
  igraph::E(g)$distance <- trade_value_to_distance(w)

  iso <- igraph::V(g)$name
  in_str <- as.numeric(igraph::strength(g, mode = "in", weights = w))
  out_str <- as.numeric(igraph::strength(g, mode = "out", weights = w))
  tot_str <- in_str + out_str
  deg <- as.integer(igraph::degree(g, mode = "all"))
  in_deg <- as.integer(igraph::degree(g, mode = "in"))
  out_deg <- as.integer(igraph::degree(g, mode = "out"))

  pr <- tryCatch(
    as.numeric(igraph::page_rank(g, directed = directed, weights = w)$vector),
    error = function(e) {
      warnings <<- c(warnings, paste("PageRank unavailable:", conditionMessage(e)))
      rep(NA_real_, length(iso))
    }
  )
  pr <- sanitize_chart_numeric(pr)

  bw_w <- igraph::E(g)$distance
  if (any(!is.finite(bw_w))) {
    bw_w[!is.finite(bw_w)] <- max(bw_w[is.finite(bw_w)], na.rm = TRUE) * 10
  }
  bt <- tryCatch(
    as.numeric(igraph::betweenness(g, directed = directed, weights = bw_w, normalized = TRUE)),
    error = function(e) {
      warnings <<- c(warnings, paste("Betweenness unavailable:", conditionMessage(e)))
      rep(NA_real_, length(iso))
    }
  )
  bt <- sanitize_chart_numeric(bt)

  comps <- igraph::components(g, mode = if (directed) "weak" else "strong")
  component_id <- as.integer(comps$membership)
  component_size <- as.integer(comps$csize[comps$membership])

  community <- rep(NA_integer_, length(iso))
  if (igraph::ecount(g) > 0L && igraph::vcount(g) > 1L) {
    ug <- igraph::as_undirected(g, mode = "collapse",
                                edge.attr.comb = list(trade_value_usd = "sum",
                                                      distance = "ignore",
                                                      "ignore"))
    w_u <- igraph::E(ug)$trade_value_usd
    if (is.null(w_u) || !length(w_u)) w_u <- NULL
    cl <- tryCatch(
      igraph::cluster_walktrap(ug, weights = w_u),
      error = function(e) {
        tryCatch(
          if (exists("cluster_louvain", where = asNamespace("igraph"), inherits = FALSE) ||
              "cluster_louvain" %in% getNamespaceExports("igraph")) {
            igraph::cluster_louvain(ug, weights = w_u)
          } else NULL,
          error = function(e2) {
            warnings <<- c(warnings, "Community detection unavailable")
            NULL
          }
        )
      }
    )
    if (!is.null(cl)) {
      memb <- as.integer(igraph::membership(cl))

      umap <- setNames(memb, igraph::V(ug)$name)
      community <- as.integer(unname(umap[iso]))
    }
  }

  igraph::V(g)$total_strength <- tot_str
  igraph::V(g)$in_strength <- in_str
  igraph::V(g)$out_strength <- out_str
  igraph::V(g)$degree <- deg
  igraph::V(g)$in_degree <- in_deg
  igraph::V(g)$out_degree <- out_deg
  igraph::V(g)$pagerank <- pr
  igraph::V(g)$betweenness <- bt
  igraph::V(g)$component_id <- component_id
  igraph::V(g)$component_size <- component_size
  igraph::V(g)$community <- community

  attr_or <- function(nm, default) {
    v <- igraph::vertex_attr(g, nm)
    if (is.null(v)) default else v
  }
  nodes <- data.table::data.table(
    iso3 = iso,
    display_name = {
      dn <- attr_or("display_name", iso)
      ifelse(is.na(dn) | !nzchar(as.character(dn)), iso, as.character(dn))
    },
    reporting_status = as.character(attr_or("reporting_status", NA_character_)),
    is_selected_reporter = as.logical(attr_or("is_selected_reporter", FALSE)),
    is_selected_partner = as.logical(attr_or("is_selected_partner", FALSE)),
    represented_as_reporter = as.logical(attr_or("represented_as_reporter", FALSE)),
    represented_as_partner = as.logical(attr_or("represented_as_partner", FALSE)),
    gdp_current_usd = sanitize_chart_numeric(attr_or("gdp_current_usd", NA_real_)),
    population_total = sanitize_chart_numeric(attr_or("population_total", NA_real_)),
    total_strength = sanitize_chart_numeric(tot_str),
    in_strength = sanitize_chart_numeric(in_str),
    out_strength = sanitize_chart_numeric(out_str),
    degree = deg,
    in_degree = in_deg,
    out_degree = out_deg,
    pagerank = pr,
    betweenness = bt,
    community = community,
    component_id = component_id,
    component_size = component_size
  )

  if (any(is.finite(nodes$pagerank))) {
    nodes[, pagerank_quantile := as.integer(cut(
      pagerank,
      breaks = unique(stats::quantile(pagerank, probs = seq(0, 1, 0.25), na.rm = TRUE)),
      include.lowest = TRUE, labels = FALSE
    ))]
  } else {
    nodes[, pagerank_quantile := NA_integer_]
  }
  data.table::setorderv(nodes, "iso3")
  list(graph = g, nodes = nodes, warnings = unique(warnings))
}

top_share <- function(values, n = 5L) {
  v <- sanitize_chart_numeric(as.numeric(values))
  v <- v[is.finite(v) & v > 0]
  if (!length(v)) return(NA_real_)
  tot <- sum(v)
  if (tot <= 0) return(NA_real_)
  v <- sort(v, decreasing = TRUE)
  100 * sum(v[seq_len(min(as.integer(n), length(v)))]) / tot
}

herfindahl_index <- function(values) {
  v <- sanitize_chart_numeric(as.numeric(values))
  v <- v[is.finite(v) & v > 0]
  if (!length(v)) return(NA_real_)
  tot <- sum(v)
  if (tot <= 0) return(NA_real_)
  shares <- v / tot
  sum(shares^2)
}

network_level_stats <- function(g, edges, nodes) {
  ed <- data.table::as.data.table(edges)
  nd <- data.table::as.data.table(nodes)
  n_nodes <- if (!is.null(g)) igraph::vcount(g) else nrow(nd)
  n_edges <- if (!is.null(g)) igraph::ecount(g) else nrow(ed)
  directed <- if (!is.null(g)) igraph::is_directed(g) else TRUE

  dens <- if (!is.null(g) && n_nodes > 1L) {
    sanitize_chart_numeric(igraph::edge_density(g, loops = FALSE))
  } else NA_real_

  recip <- if (!is.null(g) && directed && n_edges > 0L) {
    sanitize_chart_numeric(igraph::reciprocity(g, mode = "default"))
  } else NA_real_

  n_comp <- if ("component_id" %in% names(nd) && nrow(nd)) {
    data.table::uniqueN(nd$component_id)
  } else NA_integer_

  largest_share <- if ("component_size" %in% names(nd) && nrow(nd) && n_nodes > 0) {
    100 * max(nd$component_size, na.rm = TRUE) / n_nodes
  } else NA_real_

  avg_deg <- if (nrow(nd) && "degree" %in% names(nd)) {
    mean(nd$degree, na.rm = TRUE)
  } else NA_real_

  total_value <- sum(ed$trade_value_usd, na.rm = TRUE)
  list(
    node_count = as.integer(n_nodes),
    edge_count = as.integer(n_edges),
    directed_density = dens,
    weak_component_count = as.integer(n_comp),
    largest_weak_component_share_pct = sanitize_chart_numeric(largest_share),
    reciprocity = recip,
    average_degree = sanitize_chart_numeric(avg_deg),
    total_observed_trade_value_usd = sanitize_chart_numeric(total_value),
    top5_edge_share_pct = top_share(ed$trade_value_usd, 5L),
    top5_node_strength_share_pct = top_share(nd$total_strength, 5L),
    edge_hhi = herfindahl_index(ed$trade_value_usd),
    node_strength_hhi = herfindahl_index(nd$total_strength)
  )
}

layout_network_coordinates <- function(g, method = "fr", seed = NW_LAYOUT_SEED) {
  method <- as.character(method %||% "fr")[1]
  if (!nzchar(method %||% "") || is.na(method)) method <- "fr"
  n <- igraph::vcount(g)
  if (n == 0L) {
    return(data.table::data.table(iso3 = character(), x = numeric(), y = numeric()))
  }
  if (n == 1L) {
    return(data.table::data.table(iso3 = igraph::V(g)$name, x = 0, y = 0))
  }
  set.seed(as.integer(seed %||% NW_LAYOUT_SEED)[1])
  coords <- switch(
    method,
    "kk" = igraph::layout_with_kk(g),
    "circle" = igraph::layout_in_circle(g),
    igraph::layout_with_fr(g)
  )
  data.table::data.table(
    iso3 = igraph::V(g)$name,
    x = as.numeric(coords[, 1]),
    y = as.numeric(coords[, 2])
  )
}

extract_ego_network <- function(g, focus_iso3, order = 1L) {
  if (igraph::vcount(g) == 0L || is.null(focus_iso3) || !nzchar(focus_iso3)) {
    return(g)
  }
  focus <- as.character(focus_iso3)
  if (!focus %in% igraph::V(g)$name) return(igraph::make_empty_graph(directed = igraph::is_directed(g)))
  order <- as.integer(order %||% 1L)
  order <- max(1L, min(order, 2L))
  vids <- unlist(igraph::ego(g, order = order, nodes = focus, mode = "all"))
  igraph::induced_subgraph(g, vids)
}
