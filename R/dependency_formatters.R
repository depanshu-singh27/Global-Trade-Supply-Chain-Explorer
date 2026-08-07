DEP_DEFAULT_METRIC <- "partner_share"
DEP_NODE_CAP <- 200L
DEP_SHARE_TOLERANCE <- 1e-6

DEP_HHI_BANDS <- list(
  list(max = 0.15, label = "Diversified"),
  list(max = 0.25, label = "Moderate concentration"),
  list(max = 0.50, label = "High concentration"),
  list(max = 1.01, label = "Very high concentration")
)

DEP_SHARE_BANDS <- list(
  list(max = 0.25, label = "Below 25%"),
  list(max = 0.50, label = "25%–50%"),
  list(max = 0.75, label = "50%–75%"),
  list(max = 1.01, label = "Above 75%")
)

dependency_metric_choices <- function() {
  c(
    "Partner import share" = "partner_share",
    "Top-supplier share" = "top_1_share",
    "Top-three supplier share" = "top_3_share",
    "Supplier HHI" = "supplier_hhi",
    "Effective supplier count" = "effective_supplier_count",
    "Import value" = "import_value",
    "Imports as % of GDP" = "imports_pct_gdp"
  )
}

dependency_aggregation_choices <- function() {
  c(
    "Commodity-specific dependency" = "commodity_specific",
    "Reporter-wide weighted dependency" = "reporter_weighted",
    "Partner exposure across commodities" = "partner_exposure"
  )
}

dependency_matrix_mode_choices <- function() {
  c(
    "Reporter × supplier" = "reporter_supplier",
    "Country-commodity nodes" = "country_commodity"
  )
}

classify_hhi_band <- function(hhi) {
  x <- sanitize_chart_numeric(hhi)
  if (length(x) != 1L) {
    return(vapply(x, classify_hhi_band, character(1)))
  }
  if (is.na(x) || !is.finite(x) || x < 0) return("Unavailable")
  for (b in DEP_HHI_BANDS) {
    if (x < b$max) return(b$label)
  }
  "Very high concentration"
}

classify_share_band <- function(share) {
  x <- sanitize_chart_numeric(share)
  if (length(x) != 1L) {
    return(vapply(x, classify_share_band, character(1)))
  }
  if (is.na(x) || !is.finite(x) || x < 0) return("Unavailable")
  for (b in DEP_SHARE_BANDS) {
    if (x < b$max) return(b$label)
  }
  "Above 75%"
}

format_dependency_share <- function(x, digits = 1L) {
  x <- sanitize_chart_numeric(x)
  if (length(x) != 1L) {
    return(vapply(x, format_dependency_share, character(1), digits = digits))
  }
  if (is.na(x) || !is.finite(x)) return("Unavailable")
  sprintf("%.*f%%", digits, 100 * x)
}

format_dependency_hhi <- function(x, digits = 3L) {
  x <- sanitize_chart_numeric(x)
  if (length(x) != 1L) {
    return(vapply(x, format_dependency_hhi, character(1), digits = digits))
  }
  if (is.na(x) || !is.finite(x)) return("Unavailable")
  sprintf("%.*f", digits, x)
}

sanitize_dependency_filename_token <- function(x, fallback = "dependency") {
  sanitize_download_token(x, fallback)
}

dependency_download_filename <- function(prefix, ..., ext = "csv") {
  parts <- c(
    sanitize_dependency_filename_token(prefix, "dependency"),
    vapply(list(...), function(z) sanitize_dependency_filename_token(z, "x"), character(1))
  )
  paste0(paste(parts, collapse = "_"), ".", ext)
}
