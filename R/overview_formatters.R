format_usd_compact <- function(x, digits = 1L) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L) {
    return(vapply(x, format_usd_compact, character(1), digits = digits))
  }
  if (is.null(x) || length(x) == 0L || is.na(x) || is.nan(x) || is.infinite(x)) {
    return("Unavailable")
  }
  ax <- abs(x)
  sign <- if (x < 0) "-" else ""
  if (ax >= 1e12) {
    return(sprintf("%sUS$%.*ftn", sign, digits, ax / 1e12))
  }
  if (ax >= 1e9) {
    return(sprintf("%sUS$%.*fbn", sign, digits, ax / 1e9))
  }
  if (ax >= 1e6) {
    return(sprintf("%sUS$%.*fmn", sign, digits, ax / 1e6))
  }
  if (ax >= 1e3) {
    return(sprintf("%sUS$%s", sign, format(round(ax), big.mark = ",", scientific = FALSE)))
  }
  sprintf("%sUS$%.*f", sign, digits, ax)
}

format_pct <- function(x, digits = 1L, already_percent = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L) {
    return(vapply(x, format_pct, character(1), digits = digits, already_percent = already_percent))
  }
  if (is.null(x) || length(x) == 0L || is.na(x) || is.nan(x) || is.infinite(x)) {
    return("Unavailable")
  }
  val <- if (isTRUE(already_percent)) x else 100 * x
  sprintf("%.*f%%", digits, val)
}

format_count <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L) return(vapply(x, format_count, character(1)))
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("Unavailable")
  format(round(x), big.mark = ",", scientific = FALSE)
}

format_per_person <- function(x, digits = 0L) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L) {
    return(vapply(x, format_per_person, character(1), digits = digits))
  }
  if (is.null(x) || is.na(x) || is.nan(x) || is.infinite(x)) return("Unavailable")
  sprintf("US$%s per person", format(round(x, digits), big.mark = ",", scientific = FALSE))
}

format_yoy_delta <- function(pct) {
  pct <- suppressWarnings(as.numeric(pct))
  if (is.na(pct) || is.nan(pct) || is.infinite(pct)) return("No prior-year comparison")
  sign <- if (pct > 0) "+" else ""
  sprintf("%s%s vs prior year", sign, format_pct(pct, digits = 1L))
}

missing_label <- function(x = NULL) {
  "Unavailable"
}

sanitize_chart_numeric <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.infinite(x) | is.nan(x)] <- NA_real_
  x
}
