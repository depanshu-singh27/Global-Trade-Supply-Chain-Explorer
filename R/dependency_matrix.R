build_reporter_supplier_matrix <- function(shares,
                                             metric = "partner_share",
                                             top_rows = NULL,
                                             top_cols = NULL) {
  dt <- data.table::as.data.table(shares)
  empty <- list(
    long = data.table::data.table(),
    wide = NULL,
    row_ids = character(),
    col_ids = character(),
    n_rows = 0L,
    n_cols = 0L,
    n_observed = 0L
  )
  if (!nrow(dt)) return(empty)

  agg <- dt[, .(
    partner_import_value = sum(partner_import_value, na.rm = TRUE)
  ), by = .(reporter_iso3, partner_iso3, reporter_name, partner_name)]
  rep_tot <- agg[, .(reporter_total = sum(partner_import_value, na.rm = TRUE)), by = reporter_iso3]
  agg <- merge(agg, rep_tot, by = "reporter_iso3", all.x = TRUE)
  agg[, partner_share := ifelse(reporter_total > 0, partner_import_value / reporter_total, NA_real_)]
  agg[, value := switch(
    as.character(metric %||% "partner_share"),
    "import_value" = partner_import_value,
    "imports_pct_gdp" = NA_real_,
    partner_share
  )]
  agg[, value := sanitize_chart_numeric(value)]
  agg[, observed := TRUE]

  row_ord <- agg[, .(tot = sum(partner_import_value, na.rm = TRUE)), by = reporter_iso3]
  data.table::setorderv(row_ord, c("tot", "reporter_iso3"), c(-1L, 1L))
  col_ord <- agg[, .(tot = sum(partner_import_value, na.rm = TRUE)), by = partner_iso3]
  data.table::setorderv(col_ord, c("tot", "partner_iso3"), c(-1L, 1L))

  if (!is.null(top_rows) && is.finite(top_rows) && top_rows > 0) {
    row_ord <- head(row_ord, as.integer(top_rows))
  }
  if (!is.null(top_cols) && is.finite(top_cols) && top_cols > 0) {
    col_ord <- head(col_ord, as.integer(top_cols))
  }
  agg <- agg[reporter_iso3 %in% row_ord$reporter_iso3 & partner_iso3 %in% col_ord$partner_iso3]

  wide <- data.table::dcast(
    agg,
    reporter_iso3 ~ partner_iso3,
    value.var = "value",
    fun.aggregate = function(x) if (length(x)) x[1] else NA_real_
  )
  list(
    long = agg[, .(
      row_id = reporter_iso3, col_id = partner_iso3,
      reporter_iso3, partner_iso3, reporter_name, partner_name,
      value, partner_import_value, partner_share, observed
    )],
    wide = wide,
    row_ids = row_ord$reporter_iso3,
    col_ids = col_ord$partner_iso3,
    n_rows = nrow(row_ord),
    n_cols = nrow(col_ord),
    n_observed = nrow(agg),
    metric = metric
  )
}

make_country_commodity_node_id <- function(iso3, hs_code) {
  paste0(as.character(iso3), "::", as.character(hs_code))
}

parse_country_commodity_node_id <- function(node_id) {
  parts <- strsplit(as.character(node_id), "::", fixed = TRUE)
  data.table::data.table(
    node_id = as.character(node_id),
    iso3 = vapply(parts, function(p) p[1] %||% NA_character_, character(1)),
    hs_code = vapply(parts, function(p) p[2] %||% NA_character_, character(1))
  )
}

rank_active_dependency_nodes <- function(shares, max_nodes = DEP_NODE_CAP) {
  dt <- data.table::as.data.table(shares)
  if (!nrow(dt)) {
    return(data.table::data.table(node_id = character(), involvement = numeric()))
  }

  imp_nodes <- dt[, .(
    involvement = sum(partner_import_value, na.rm = TRUE),
    role = "importer"
  ), by = .(node_id = make_country_commodity_node_id(reporter_iso3, hs_code))]
  sup_nodes <- dt[, .(
    involvement = sum(partner_import_value, na.rm = TRUE),
    role = "supplier"
  ), by = .(node_id = make_country_commodity_node_id(partner_iso3, hs_code))]
  nodes <- rbind(imp_nodes, sup_nodes)[, .(
    involvement = sum(involvement, na.rm = TRUE)
  ), by = node_id]
  data.table::setorderv(nodes, c("involvement", "node_id"), c(-1L, 1L))
  max_nodes <- as.integer(max_nodes %||% DEP_NODE_CAP)
  if (is.finite(max_nodes) && max_nodes > 0L && nrow(nodes) > max_nodes) {
    nodes <- nodes[seq_len(max_nodes)]
  }
  nodes
}

build_country_commodity_sparse_matrix <- function(shares,
                                                   max_nodes = DEP_NODE_CAP,
                                                   weight = "partner_share") {
  dt <- data.table::as.data.table(shares)
  empty <- list(
    edges = data.table::data.table(),
    nodes = data.table::data.table(),
    n_nodes = 0L,
    n_edges = 0L,
    density = NA_real_,
    capped = FALSE
  )
  if (!nrow(dt)) return(empty)

  edges <- dt[, .(
    from_node = make_country_commodity_node_id(reporter_iso3, hs_code),
    to_node = make_country_commodity_node_id(partner_iso3, hs_code),
    hs_code,
    reporter_iso3,
    partner_iso3,
    partner_share,
    partner_import_value,
    observed = TRUE
  )]
  edges <- edges[is.finite(partner_share) & partner_share > 0]

  n_before <- data.table::uniqueN(c(edges$from_node, edges$to_node))
  nodes <- rank_active_dependency_nodes(dt, max_nodes = max_nodes)
  keep <- nodes$node_id
  edges <- edges[from_node %in% keep & to_node %in% keep]
  edges[, weight := if (identical(weight, "import_value")) partner_import_value else partner_share]
  edges[, weight := sanitize_chart_numeric(weight)]

  n_nodes <- length(keep)
  n_edges <- nrow(edges)
  dens <- if (n_nodes > 0L) n_edges / (n_nodes * n_nodes) else NA_real_
  list(
    edges = edges,
    nodes = nodes,
    n_nodes = as.integer(n_nodes),
    n_edges = as.integer(n_edges),
    density = sanitize_chart_numeric(dens),
    capped = n_before > n_nodes,
    n_eligible_nodes_before_cap = as.integer(n_before),
    max_nodes = as.integer(max_nodes %||% DEP_NODE_CAP)
  )
}

sparse_edges_to_display_matrix <- function(edges, row_ids, col_ids, value_col = "weight") {
  ed <- data.table::as.data.table(edges)
  if (!length(row_ids) || !length(col_ids)) {
    return(matrix(NA_real_, 0, 0))
  }
  mat <- matrix(NA_real_, nrow = length(row_ids), ncol = length(col_ids),
                dimnames = list(row_ids, col_ids))
  if (!nrow(ed)) return(mat)
  ri <- match(ed$from_node %||% ed$row_id, row_ids)
  ci <- match(ed$to_node %||% ed$col_id, col_ids)
  vals <- ed[[value_col]]
  ok <- !is.na(ri) & !is.na(ci)
  for (i in which(ok)) {
    mat[ri[i], ci[i]] <- vals[i]
  }
  mat
}
