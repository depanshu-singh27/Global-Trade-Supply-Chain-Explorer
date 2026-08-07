gte_palette <- function() {
  list(
    ink = "#1B2A3B",
    muted = "#5B6B7C",
    accent = "#1F4E79",
    accent_soft = "#2F6FAE",
    panel = "#FFFFFF",
    bg = "#F4F7FB",
    border = "#D7E0EA",
    imports = "#1F4E79",
    exports = "#2A9D8F",
    total = "#264653",
    balance_pos = "#2A9D8F",
    balance_neg = "#C45C26",
    surplus = "#2A9D8F",
    deficit = "#C45C26",
    warn = "#9A6B00",
    ok = "#1F7A4D",
    partial = "#B07D1A",
    unavailable = "#6B7280",
    grid = "#E6EDF5",
    zero = "#94A3B8"
  )
}

gte_bs_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bootswatch = NULL,
    primary = gte_palette()$accent,
    secondary = "#5B6B7C",
    success = gte_palette()$ok,
    warning = gte_palette()$warn,
    danger = gte_palette()$deficit,
    "font-size-base" = "0.95rem",
    "body-bg" = gte_palette()$bg,
    "body-color" = gte_palette()$ink,
    "border-radius" = "0.5rem"
  )
}

plotly_layout_base <- function(title, p = gte_palette()) {
  list(
    title = list(text = title, font = list(size = 15, color = p$ink)),
    font = list(family = "Segoe UI, Helvetica Neue, Arial, sans-serif", color = p$ink, size = 12),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    margin = list(l = 60, r = 24, t = 48, b = 48),
    legend = list(orientation = "h", y = -0.18, x = 0, font = list(size = 11)),
    hovermode = "closest",
    xaxis = list(gridcolor = p$grid, zerolinecolor = p$zero, tickfont = list(size = 11)),
    yaxis = list(gridcolor = p$grid, zerolinecolor = p$zero, tickfont = list(size = 11))
  )
}

status_badge_class <- function(status) {
  s <- tolower(as.character(status %||% "unavailable"))
  if (grepl("complete|ok|pass", s)) return("badge-complete")
  if (grepl("partial", s)) return("badge-partial")
  if (grepl("warn", s)) return("badge-warning")
  "badge-unavailable"
}

status_badge <- function(name, status, label_override = NULL) {
  cls <- status_badge_class(status)
  label <- label_override %||% tools::toTitleCase(gsub("_", " ", as.character(status)))
  shiny::span(
    class = paste("status-badge", cls),
    role = "status",
    shiny::span(class = "status-name", name),
    shiny::span(class = "status-value", label)
  )
}
