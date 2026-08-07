SECRET_PATTERN_SPECS <- list(
  list(name = "comtrade_env", pattern = "COMTRADE_PRIMARY\\s*=", severity = "critical"),
  list(name = "subscription_key_header", pattern = "Ocp-Apim-Subscription-Key", severity = "critical"),
  list(name = "api_key_assignment", pattern = "(?i)(api[_-]?key|subscription[_-]?key)\\s*[:=]\\s*['\\\"]?[A-Za-z0-9_\\-]{16,}", severity = "high"),
  list(name = "bearer_token", pattern = "(?i)authorization\\s*:\\s*bearer\\s+[A-Za-z0-9._\\-]+", severity = "high"),
  list(name = "private_key", pattern = "-----BEGIN (RSA |OPENSSH |EC )?PRIVATE KEY-----", severity = "critical")
)

ABSOLUTE_PATH_PATTERN <- "(?i)([A-Za-z]:\\\\|/Users/|/home/[a-z]|C:\\\\Users\\\\)"
AUTHENTICATED_URL_PATTERN <- "(?i)https?://[^\\s\"']*(api[_-]?key|subscription|token)=[^\\s\"']+"

scan_text_for_secrets <- function(text, source = "memory") {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  hits <- list()
  for (spec in SECRET_PATTERN_SPECS) {
    if (grepl(spec$pattern, text, perl = TRUE)) {
      hits[[length(hits) + 1L]] <- list(
        check = spec$name,
        severity = spec$severity,
        source = source,
        status = "fail"
      )
    }
  }
  if (grepl(ABSOLUTE_PATH_PATTERN, text, perl = TRUE)) {
    hits[[length(hits) + 1L]] <- list(
      check = "absolute_path",
      severity = "high",
      source = source,
      status = "fail"
    )
  }
  if (grepl(AUTHENTICATED_URL_PATTERN, text, perl = TRUE)) {
    hits[[length(hits) + 1L]] <- list(
      check = "authenticated_url",
      severity = "high",
      source = source,
      status = "fail"
    )
  }
  hits
}

scan_file_for_secrets <- function(path) {
  if (!file.exists(path)) return(list())

  info <- file.info(path)
  if (isTRUE(info$isdir)) return(list())
  if (!is.na(info$size) && info$size > 5e6) {
    return(list(list(
      check = "skipped_large_file",
      severity = "info",
      source = basename(path),
      status = "pass"
    )))
  }
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("parquet", "rds", "fst", "png", "jpg", "jpeg", "gif", "webp")) {

    return(scan_text_for_secrets(basename(path), source = basename(path)))
  }
  txt <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  scan_text_for_secrets(txt, source = basename(path))
}

scan_bundle_for_secrets <- function(bundle_dir) {
  files <- list.files(bundle_dir, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)

  renviron <- files[basename(files) == ".Renviron"]
  hits <- list()
  if (length(renviron)) {
    hits[[length(hits) + 1L]] <- list(
      check = "renviron_present",
      severity = "critical",
      source = ".Renviron",
      status = "fail"
    )
  }
  for (f in files) {
    if (basename(f) %in% c(".", "..")) next
    hits <- c(hits, scan_file_for_secrets(f))
  }
  fails <- Filter(function(h) identical(h$status, "fail"), hits)
  list(
    ok = !length(fails),
    hits = hits,
    fail_count = length(fails)
  )
}

assert_no_comtrade_in_logs <- function(lines) {
  text <- paste(lines, collapse = "\n")
  if (grepl("COMTRADE_PRIMARY\\s*=", text) ||
      grepl("(?i)comtrade_primary\\s*[:=]\\s*\\S+", text, perl = TRUE)) {
    stop("Startup logs must not contain COMTRADE_PRIMARY values.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_env_allowlist_usage <- function(env_names = names(Sys.getenv())) {
  forbidden <- intersect(env_names, release_forbidden_env())

  list(
    forbidden_present = forbidden,
    comtrade_required = FALSE,
    allowlist = release_env_allowlist()
  )
}
