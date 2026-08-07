write_ts_csv <- function(dt, path) {
  out <- data.table::as.data.table(dt)
  drop <- grep("path|url|header|secret|token|key|raw_file|cache", names(out),
               ignore.case = TRUE, value = TRUE)
  if (length(drop)) out[, (drop) := NULL]
  data.table::fwrite(out, path, bom = TRUE)
  invisible(TRUE)
}
