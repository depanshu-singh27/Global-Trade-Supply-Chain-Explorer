`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}

utc_now <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

is_absolute_path <- function(path) {
  p <- as.character(path %||% "")
  grepl("^([A-Za-z]:)?[/\\\\]", p)
}

resolve_project_path <- function(path, root) {
  p <- as.character(path %||% "")
  if (!nzchar(p)) return(p)
  if (is_absolute_path(p)) p else file.path(root, p)
}

safe_read_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

write_json_atomic <- function(x, path, pretty = TRUE) {
  ensure_dir(dirname(path))
  tmp <- paste0(path, ".tmp")
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE, pretty = pretty, null = "null")
  txt <- as.character(txt)

  txt <- gsub("\r\n", "\n", txt, fixed = TRUE)
  txt <- gsub("\r", "\n", txt, fixed = TRUE)
  bytes <- charToRaw(enc2utf8(txt))
  if (length(bytes) >= 3L &&
      identical(as.integer(bytes[1:3]), c(0xEFL, 0xBBL, 0xBFL))) {
    bytes <- bytes[-(1:3)]
  }
  con <- file(tmp, open = "wb")
  on.exit({
    try(close(con), silent = TRUE)
  }, add = TRUE)
  writeBin(bytes, con)
  close(con)
  on.exit(NULL)
  file.rename(tmp, path)
  invisible(path)
}

normalize_json_file_lf <- function(path) {
  if (!file.exists(path)) return(invisible(FALSE))
  size <- file.info(path)$size
  if (is.na(size) || size <= 0) return(invisible(FALSE))
  raw <- readBin(path, what = "raw", n = as.integer(size))
  if (!length(raw)) return(invisible(FALSE))
  had_bom <- length(raw) >= 3L &&
    identical(as.integer(raw[1:3]), c(0xEFL, 0xBBL, 0xBFL))
  body <- if (had_bom) raw[-(1:3)] else raw
  txt <- rawToChar(body, multiple = FALSE)
  Encoding(txt) <- "UTF-8"
  norm <- gsub("\r\n", "\n", txt, fixed = TRUE)
  norm <- gsub("\r", "\n", norm, fixed = TRUE)
  bytes <- charToRaw(enc2utf8(norm))
  if (!had_bom && identical(bytes, body)) return(invisible(FALSE))
  tmp <- paste0(path, ".tmp")
  con <- file(tmp, open = "wb")
  on.exit({
    try(close(con), silent = TRUE)
  }, add = TRUE)
  writeBin(bytes, con)
  close(con)
  on.exit(NULL)
  file.rename(tmp, path)
  invisible(TRUE)
}

normalize_bundle_json_files <- function(dest_dir) {
  paths <- list.files(dest_dir, pattern = "\\.json$", full.names = TRUE,
                      ignore.case = TRUE)
  for (p in paths) normalize_json_file_lf(p)
  invisible(dest_dir)
}

file_digest_stub <- function(path) {
  info <- file.info(path)
  paste0(basename(path), ":", as.character(info$size %||% 0), ":",
         as.character(info$mtime %||% NA))
}

log_msg <- function(..., level = "INFO") {
  cat(sprintf("[%s] %s %s\n", utc_now(), level, paste0(..., collapse = "")))
}

is_iso3 <- function(x) {
  grepl("^[A-Z]{3}$", x)
}

as_char_code <- function(x) {
  if (is.null(x)) return(character())
  out <- as.character(x)
  out[is.na(x)] <- NA_character_
  out
}

null_to_na <- function(x) {
  if (is.null(x)) NA else x
}

list_get <- function(lst, ...) {
  keys <- c(...)
  cur <- lst
  for (k in keys) {
    if (is.null(cur) || is.null(cur[[k]])) return(NULL)
    cur <- cur[[k]]
  }
  cur
}

pluck_chr <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L) return(default)
  as.character(x[[1]])
}

pluck_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) return(default)
  suppressWarnings(as.numeric(x[[1]]))
}
