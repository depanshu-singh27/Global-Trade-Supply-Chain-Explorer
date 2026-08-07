NW_DEFAULT_MODE <- "exports"
NW_LAYOUT_SEED <- 42L
NW_DEFAULT_TOP_EDGES <- 100L

network_mode_defs <- function() {
  list(
    exports = list(
      id = "exports",
      label = "Reported exports",
      flow = "X",
      description = "Edge direction: reporter → partner (reported exports)."
    ),
    imports = list(
      id = "imports",
      label = "Reported imports",
      flow = "M",
      description = "Edge direction: partner → reporter (importer-declared origin)."
    ),
    undirected = list(
      id = "undirected",
      label = "Undirected observed trade",
      flow = c("M", "X"),
      description = "Unordered pairs of observed trade; mirror rows may overlap."
    )
  )
}

network_mode_label <- function(mode) {
  defs <- network_mode_defs()
  m <- as.character(mode %||% NW_DEFAULT_MODE)
  if (!is.null(defs[[m]])) return(defs[[m]]$label)
  m
}

network_node_size_choices <- function() {
  c(
    "Total strength" = "total_strength",
    "Out-strength" = "out_strength",
    "In-strength" = "in_strength",
    "PageRank" = "pagerank",
    "Degree" = "degree"
  )
}

network_node_colour_choices <- function() {
  c(
    "Reporting status" = "reporting_status",
    "Selected-universe membership" = "universe_membership",
    "Community" = "community",
    "Centrality quantile (PageRank)" = "pagerank_quantile"
  )
}

network_layout_choices <- function() {
  c(
    "Fruchterman–Reingold" = "fr",
    "Kamada–Kawai" = "kk",
    "Circle" = "circle"
  )
}

network_centrality_rank_choices <- function() {
  c(
    "Total strength" = "total_strength",
    "In-strength" = "in_strength",
    "Out-strength" = "out_strength",
    "PageRank" = "pagerank",
    "Betweenness" = "betweenness",
    "Degree" = "degree"
  )
}

sanitize_network_filename_token <- function(x, fallback = "network") {
  sanitize_download_token(x, fallback)
}

network_download_filename <- function(prefix, ..., ext = "csv") {
  parts <- c(sanitize_network_filename_token(prefix, "trade_network"),
             vapply(list(...), function(z) sanitize_network_filename_token(z, "x"), character(1)))
  paste0(paste(parts, collapse = "_"), ".", ext)
}

format_network_share <- function(x, digits = 1L) {
  x <- sanitize_chart_numeric(x)
  if (length(x) != 1L) {
    return(vapply(x, format_network_share, character(1), digits = digits))
  }
  if (is.na(x) || !is.finite(x)) return("Unavailable")
  sprintf("%.*f%%", digits, x)
}

format_network_metric <- function(x, digits = 4L) {
  x <- sanitize_chart_numeric(x)
  if (length(x) != 1L) {
    return(vapply(x, format_network_metric, character(1), digits = digits))
  }
  if (is.na(x) || !is.finite(x)) return("Unavailable")
  if (abs(x) >= 1000) return(format(round(x, 1), big.mark = ",", scientific = FALSE, trim = TRUE))
  sprintf("%.*f", digits, x)
}

reporting_status_label <- function(status) {

  status <- as.character(status)
  if (!length(status)) return(character())
  status[is.na(status) | !nzchar(status)] <- "unknown"
  labels <- c(
    represented_reporter = "Represented reporter",
    partner_only = "Partner only",
    both = "Reporter and partner",
    selected_reporter_missing = "Selected reporter (not yet represented)",
    unknown = "Unknown"
  )
  out <- unname(labels[status])
  miss <- is.na(out)
  if (any(miss)) out[miss] <- status[miss]
  out
}
