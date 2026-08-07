filter_network_observations <- function(detailed,
                                          year_min = NULL,
                                          year_max = NULL,
                                          reporters = NULL,
                                          partners = NULL,
                                          hs_codes = NULL,
                                          flows = NULL) {
  dt <- prepare_detailed_trade(detailed)
  if (!nrow(dt)) return(dt)
  filter_detailed_trade(
    dt,
    year_min = year_min,
    year_max = year_max,
    reporters = reporters,
    partners = partners,
    flows = flows %||% c("M", "X"),
    hs_codes = hs_codes
  )
}

build_directed_edges_from_rows <- function(dt, mode = "exports") {
  dt <- data.table::as.data.table(dt)
  empty <- data.table::data.table(
    from_iso3 = character(), to_iso3 = character(),
    from_name = character(), to_name = character(),
    trade_value_usd = numeric(), observation_count = integer(),
    year_start = integer(), year_end = integer(),
    flow_mode = character(), hs_scope = character(),
    source_reporter_count = integer()
  )
  if (!nrow(dt)) return(empty)

  mode <- as.character(mode %||% NW_DEFAULT_MODE)
  if (identical(mode, "exports")) {
    rows <- dt[flow_code == "X"]
    if (!nrow(rows)) return(empty)
    edges <- rows[, .(
      from_iso3 = reporter_iso3,
      to_iso3 = partner_iso3,
      from_name = reporter_name,
      to_name = partner_name,
      trade_value_usd = trade_value_usd,
      year = year,
      hs_code = hs_code,
      source_reporter = reporter_iso3
    )]
  } else if (identical(mode, "imports")) {
    rows <- dt[flow_code == "M"]
    if (!nrow(rows)) return(empty)
    edges <- rows[, .(
      from_iso3 = partner_iso3,
      to_iso3 = reporter_iso3,
      from_name = partner_name,
      to_name = reporter_name,
      trade_value_usd = trade_value_usd,
      year = year,
      hs_code = hs_code,
      source_reporter = reporter_iso3
    )]
  } else if (identical(mode, "undirected")) {
    edges <- dt[, {
      a <- pmin(reporter_iso3, partner_iso3)
      b <- pmax(reporter_iso3, partner_iso3)
      an <- ifelse(reporter_iso3 <= partner_iso3, reporter_name, partner_name)
      bn <- ifelse(reporter_iso3 <= partner_iso3, partner_name, reporter_name)
      .(
        from_iso3 = a, to_iso3 = b, from_name = an, to_name = bn,
        trade_value_usd = trade_value_usd, year = year, hs_code = hs_code,
        source_reporter = reporter_iso3
      )
    }]
  } else {
    stop("Unknown network mode: ", mode, call. = FALSE)
  }

  edges <- edges[
    !is.na(from_iso3) & !is.na(to_iso3) &
      nchar(from_iso3) == 3L & nchar(to_iso3) == 3L &
      is.finite(trade_value_usd) & trade_value_usd >= 0
  ]
  edges[, flow_mode := mode]
  edges
}

aggregate_network_edges <- function(edge_rows, mode = "exports",
                                      hs_scope_label = "all_hs4") {
  empty <- data.table::data.table(
    from_iso3 = character(), to_iso3 = character(),
    from_name = character(), to_name = character(),
    trade_value_usd = numeric(), observation_count = integer(),
    year_start = integer(), year_end = integer(),
    flow_mode = character(), hs_scope = character(),
    source_reporter_count = integer()
  )
  dt <- data.table::as.data.table(edge_rows)
  if (!nrow(dt)) return(empty)

  out <- dt[, .(
    from_name = from_name[1],
    to_name = to_name[1],
    trade_value_usd = sum(trade_value_usd, na.rm = TRUE),
    observation_count = .N,
    year_start = min(year, na.rm = TRUE),
    year_end = max(year, na.rm = TRUE),
    flow_mode = mode[1],
    hs_scope = as.character(hs_scope_label),
    source_reporter_count = data.table::uniqueN(source_reporter)
  ), by = .(from_iso3, to_iso3)]

  out <- out[is.finite(trade_value_usd) & trade_value_usd > 0]
  data.table::setorderv(out, c("trade_value_usd", "from_iso3", "to_iso3"), c(-1L, 1L, 1L))
  out
}

exclude_self_edges <- function(edges) {
  dt <- data.table::as.data.table(edges)
  if (!nrow(dt)) {
    return(list(
      edges = dt,
      excluded_count = 0L,
      excluded_value = 0
    ))
  }
  self <- dt[from_iso3 == to_iso3]
  kept <- dt[from_iso3 != to_iso3]
  list(
    edges = kept,
    excluded_count = nrow(self),
    excluded_value = sum(self$trade_value_usd, na.rm = TRUE)
  )
}

select_top_network_edges <- function(edges, top_n = NW_DEFAULT_TOP_EDGES,
                                       min_value = NULL) {
  dt <- data.table::as.data.table(edges)
  eligible_n <- nrow(dt)
  eligible_value <- sum(dt$trade_value_usd, na.rm = TRUE)
  if (!nrow(dt)) {
    return(list(
      visible = dt,
      eligible_n = 0L,
      eligible_value = 0,
      visible_n = 0L,
      visible_value = 0,
      coverage_pct = NA_real_
    ))
  }
  if (!is.null(min_value) && is.finite(min_value) && min_value > 0) {
    dt <- dt[trade_value_usd >= as.numeric(min_value)]
  }
  top_n <- as.integer(top_n %||% NW_DEFAULT_TOP_EDGES)
  if (is.finite(top_n) && top_n > 0L && nrow(dt) > top_n) {
    data.table::setorderv(dt, c("trade_value_usd", "from_iso3", "to_iso3"), c(-1L, 1L, 1L))
    dt <- dt[seq_len(top_n)]
  }
  vis_value <- sum(dt$trade_value_usd, na.rm = TRUE)
  list(
    visible = dt,
    eligible_n = eligible_n,
    eligible_value = eligible_value,
    visible_n = nrow(dt),
    visible_value = vis_value,
    coverage_pct = if (eligible_value > 0) 100 * vis_value / eligible_value else NA_real_
  )
}

construct_network_edges <- function(detailed,
                                      mode = "exports",
                                      year_min = NULL,
                                      year_max = NULL,
                                      reporters = NULL,
                                      partners = NULL,
                                      hs_codes = NULL,
                                      top_n = NW_DEFAULT_TOP_EDGES,
                                      min_value = NULL,
                                      exclude_self = TRUE) {
  mode <- as.character(mode %||% NW_DEFAULT_MODE)
  flows <- if (identical(mode, "exports")) "X"
  else if (identical(mode, "imports")) "M"
  else c("M", "X")

  obs <- filter_network_observations(
    detailed,
    year_min = year_min, year_max = year_max,
    reporters = reporters, partners = partners,
    hs_codes = hs_codes, flows = flows
  )
  hs_scope <- if (is.null(hs_codes) || !length(hs_codes) ||
                   all(hs_codes %in% c("__ALL__", ""))) {
    "all_hs4"
  } else {
    paste(sort(unique(as.character(hs_codes))), collapse = "+")
  }

  raw_edges <- build_directed_edges_from_rows(obs, mode = mode)
  agg <- aggregate_network_edges(raw_edges, mode = mode, hs_scope_label = hs_scope)
  self_info <- if (isTRUE(exclude_self)) exclude_self_edges(agg) else list(
    edges = agg, excluded_count = 0L, excluded_value = 0
  )
  sel <- select_top_network_edges(self_info$edges, top_n = top_n, min_value = min_value)

  list(
    observations = obs,
    eligible_edges = self_info$edges,
    visible_edges = sel$visible,
    mode = mode,
    year_min = year_min,
    year_max = year_max,
    hs_scope = hs_scope,
    self_excluded_count = self_info$excluded_count,
    self_excluded_value = self_info$excluded_value,
    eligible_n = sel$eligible_n,
    eligible_value = sel$eligible_value,
    visible_n = sel$visible_n,
    visible_value = sel$visible_value,
    coverage_pct = sel$coverage_pct,
    source_rows = nrow(obs)
  )
}

build_network_nodes <- function(edges,
                                  detailed = NULL,
                                  selected_reporters = character(),
                                  selected_partners = character(),
                                  represented_reporters = character()) {
  dt <- data.table::as.data.table(edges)
  if (!nrow(dt)) {
    return(data.table::data.table(
      iso3 = character(), display_name = character(),
      reporting_status = character(),
      is_selected_reporter = logical(),
      is_selected_partner = logical(),
      represented_as_reporter = logical(),
      represented_as_partner = logical(),
      gdp_current_usd = numeric(),
      population_total = numeric()
    ))
  }
  nodes <- rbind(
    dt[, .(iso3 = from_iso3, display_name = from_name)],
    dt[, .(iso3 = to_iso3, display_name = to_name)]
  )
  nodes <- unique(nodes, by = "iso3")
  nodes[is.na(display_name) | !nzchar(display_name), display_name := iso3]

  selected_reporters <- as.character(selected_reporters %||% character())
  selected_partners <- as.character(selected_partners %||% character())
  represented_reporters <- as.character(represented_reporters %||% character())

  partner_obs <- character()
  if (!is.null(detailed) && nrow(detailed)) {
    d <- prepare_detailed_trade(detailed)
    partner_obs <- unique(d$partner_iso3)
  }

  nodes[, `:=`(
    is_selected_reporter = iso3 %in% selected_reporters,
    is_selected_partner = iso3 %in% selected_partners,
    represented_as_reporter = iso3 %in% represented_reporters,
    represented_as_partner = iso3 %in% partner_obs
  )]
  nodes[, reporting_status := data.table::fcase(
    represented_as_reporter & represented_as_partner, "both",
    represented_as_reporter, "represented_reporter",
    rep(TRUE, .N), "partner_only"
  )]

  nodes[, `:=`(gdp_current_usd = NA_real_, population_total = NA_real_)]
  if (!is.null(detailed) && nrow(detailed)) {
    d <- data.table::as.data.table(detailed)
    if ("reporter_gdp_current_usd" %in% names(d)) {
      rg <- unique(d[!is.na(reporter_iso3), .(
        iso3 = as.character(reporter_iso3),
        gdp = as.numeric(reporter_gdp_current_usd),
        pop = if ("reporter_population_total" %in% names(d)) as.numeric(reporter_population_total) else NA_real_
      )])
      nodes[rg, `:=`(gdp_current_usd = i.gdp, population_total = i.pop), on = "iso3"]
    }
    if ("partner_gdp_current_usd" %in% names(d)) {
      pg <- unique(d[!is.na(partner_iso3), .(
        iso3 = as.character(partner_iso3),
        gdp = as.numeric(partner_gdp_current_usd),
        pop = if ("partner_population_total" %in% names(d)) as.numeric(partner_population_total) else NA_real_
      )])
      miss <- nodes[is.na(gdp_current_usd), iso3]
      if (length(miss)) {
        nodes[pg[iso3 %in% miss], `:=`(
          gdp_current_usd = i.gdp, population_total = i.pop
        ), on = "iso3"]
      }
    }
  }
  data.table::setorderv(nodes, "iso3")
  nodes
}

create_trade_igraph <- function(edges, nodes = NULL, directed = TRUE) {
  ed <- data.table::as.data.table(edges)
  if (!nrow(ed)) {
    g <- igraph::make_empty_graph(n = 0, directed = isTRUE(directed))
    return(g)
  }
  if (is.null(nodes)) nodes <- build_network_nodes(ed)
  nd <- data.table::as.data.table(nodes)

  need <- unique(c(ed$from_iso3, ed$to_iso3))
  missing <- setdiff(need, nd$iso3)
  if (length(missing)) {
    nd <- rbind(nd, data.table::data.table(
      iso3 = missing, display_name = missing,
      reporting_status = "partner_only",
      is_selected_reporter = FALSE, is_selected_partner = FALSE,
      represented_as_reporter = FALSE, represented_as_partner = TRUE,
      gdp_current_usd = NA_real_, population_total = NA_real_
    ), fill = TRUE)
  }
  verts <- data.table::copy(nd)
  data.table::setnames(verts, "iso3", "name")
  g <- igraph::graph_from_data_frame(
    d = as.data.frame(ed[, .(
      from = from_iso3, to = to_iso3,
      trade_value_usd, observation_count, year_start, year_end,
      flow_mode, hs_scope, source_reporter_count
    )]),
    directed = isTRUE(directed),
    vertices = as.data.frame(verts)
  )
  g
}
