TF_AGGREGATE_PARTNER_ISO3 <- c(
  "WLD", "W00", "EUR", "ASE", "S19", "X99", "UNK", "99"
)

tf_scale_divisor <- function(scale = "auto", values = NULL) {
  scale <- as.character(scale %||% "auto")
  if (identical(scale, "millions")) return(list(div = 1e6, unit = "US$ millions"))
  if (identical(scale, "billions")) return(list(div = 1e9, unit = "US$ billions"))
  mx <- suppressWarnings(max(as.numeric(values), na.rm = TRUE))
  if (!is.finite(mx) || mx < 1e9) return(list(div = 1e6, unit = "US$ millions"))
  list(div = 1e9, unit = "US$ billions")
}

format_trade_value_scaled <- function(x, scale = "auto", digits = 1L) {
  x <- sanitize_chart_numeric(x)
  if (length(x) != 1L) {
    return(vapply(x, format_trade_value_scaled, character(1),
                  scale = scale, digits = digits))
  }
  if (is.na(x)) return("Unavailable")
  sc <- tf_scale_divisor(scale, x)
  sprintf("%.*f %s", digits, x / sc$div, sc$unit)
}

sanitize_download_token <- function(x, fallback = "all") {
  x <- as.character(x %||% fallback)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) x <- fallback
  substr(x, 1L, 40L)
}

trade_flow_filename <- function(prefix, year_label, reporter, partner = "all",
                                  flow = "all", ext = "csv") {
  paste(
    sanitize_download_token(prefix, "trade_flows"),
    sanitize_download_token(year_label, "years"),
    sanitize_download_token(reporter, "all_reporters"),
    sanitize_download_token(partner, "all_partners"),
    sanitize_download_token(flow, "flows"),
    sep = "_"
  ) |> paste0(".", ext)
}

flow_label <- function(code) {
  code <- as.character(code)
  ifelse(code %in% c("M", "Import", "Imports"), "Imports",
         ifelse(code %in% c("X", "Export", "Exports"), "Exports", code))
}

normalize_flow_codes <- function(flow_scope = "both") {
  fs <- tolower(as.character(flow_scope %||% "both"))
  if (fs %in% c("m", "import", "imports")) return("M")
  if (fs %in% c("x", "export", "exports")) return("X")
  c("M", "X")
}
