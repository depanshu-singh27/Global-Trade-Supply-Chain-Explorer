map_palette_signed <- function() {

  c("#C45C26", "#E0A882", "#D7E0EA", "#7FB9A8", "#2A9D8F")
}

map_palette_sequential <- function() {

  c("#C5D7E8", "#9BB8D4", "#6F94BC", "#3D6E9A", "#1F4E79")
}

map_missing_fill <- function() "#B8C0C8"

map_polygon_border <- function() "#5B6B7C"

winsorised_abs_max <- function(values, probs = c(0.02, 0.98)) {
  v <- sanitize_chart_numeric(values)
  v <- v[is.finite(v)]
  if (!length(v)) return(1)
  q <- as.numeric(stats::quantile(v, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
  m <- max(abs(q), na.rm = TRUE)
  if (!is.finite(m) || m == 0) {
    m <- max(abs(v), na.rm = TRUE)
  }
  if (!is.finite(m) || m == 0) m <- 1
  m
}

signed_palette_domain <- function(values) {
  v <- sanitize_chart_numeric(values)
  v <- v[is.finite(v)]
  if (!length(v)) return(c(-1, 0, 1))
  m <- winsorised_abs_max(v)
  c(-m, m)
}

sequential_palette_domain <- function(values) {
  v <- sanitize_chart_numeric(values)
  v <- v[is.finite(v)]
  if (!length(v)) return(c(0, 1))
  q <- as.numeric(stats::quantile(v, probs = c(0.02, 0.98), na.rm = TRUE, names = FALSE, type = 7))
  rng <- range(q, na.rm = TRUE)
  if (!all(is.finite(rng))) rng <- range(v, na.rm = TRUE)
  if (rng[1] > 0) rng[1] <- 0
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || identical(rng[1], rng[2])) {
    rng <- c(0, max(1, abs(rng[1]), abs(rng[2]), na.rm = TRUE))
  }
  rng
}

quantile_breaks <- function(values, n = 5L) {
  v <- sanitize_chart_numeric(values)
  v <- v[is.finite(v)]
  n <- max(2L, as.integer(n))
  if (!length(v)) return(c(0, 1))
  probs <- seq(0, 1, length.out = n + 1L)
  br <- as.numeric(stats::quantile(v, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
  br <- unique(br)
  if (length(br) < 2L) {
    eps <- if (br[1] == 0) 1 else abs(br[1]) * 0.01
    br <- c(br[1], br[1] + eps)
  }
  br
}

build_map_color_meta <- function(values, metric, method = "continuous") {
  signed <- map_metric_is_signed(metric)
  vals <- sanitize_chart_numeric(values)
  method <- as.character(method %||% "continuous")
  if (signed && method %in% c("fixed_symmetric", "continuous", "quantile")) {

    if (identical(method, "quantile")) {
      br <- quantile_breaks(vals, 5L)

      if (min(br) < 0 && max(br) > 0 && !any(abs(br) < .Machine$double.eps^0.5)) {
        br <- sort(unique(c(br, 0)))
      }
      list(
        method = "quantile",
        signed = TRUE,
        palette = map_palette_signed(),
        domain = range(br),
        bins = br,
        na_color = map_missing_fill()
      )
    } else {
      dom <- signed_palette_domain(vals)
      list(
        method = "continuous",
        signed = TRUE,
        palette = map_palette_signed(),
        domain = dom,
        bins = NULL,
        na_color = map_missing_fill()
      )
    }
  } else if (identical(method, "quantile")) {
    br <- quantile_breaks(vals, 5L)
    list(
      method = "quantile",
      signed = FALSE,
      palette = map_palette_sequential(),
      domain = range(br),
      bins = br,
      na_color = map_missing_fill()
    )
  } else {
    dom <- sequential_palette_domain(vals)
    list(
      method = "continuous",
      signed = FALSE,
      palette = map_palette_sequential(),
      domain = dom,
      bins = NULL,
      na_color = map_missing_fill()
    )
  }
}

leaflet_color_fun <- function(meta) {
  if (identical(meta$method, "quantile") && !is.null(meta$bins)) {
    leaflet::colorBin(
      palette = meta$palette,
      domain = meta$domain,
      bins = meta$bins,
      na.color = meta$na_color,
      pretty = FALSE
    )
  } else {
    leaflet::colorNumeric(
      palette = meta$palette,
      domain = meta$domain,
      na.color = meta$na_color
    )
  }
}

clamp_map_values_to_domain <- function(values, domain) {
  v <- sanitize_chart_numeric(values)
  if (length(domain) < 2L || !all(is.finite(domain))) return(v)
  lo <- min(domain, na.rm = TRUE)
  hi <- max(domain, na.rm = TRUE)
  out <- v
  ok <- is.finite(out)
  out[ok] <- pmin(pmax(out[ok], lo), hi)
  out
}

map_unique_fill_colours <- function(values, meta) {
  pal <- leaflet_color_fun(meta)
  plot_vals <- clamp_map_values_to_domain(values, meta$domain)
  cols <- pal(plot_vals)
  cols <- cols[!is.na(cols)]
  length(unique(cols))
}
