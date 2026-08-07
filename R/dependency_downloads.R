write_dependency_csv <- function(dt, path) {
  out <- data.table::as.data.table(dt)
  drop <- grep("path|url|header|secret|token|key|raw_file|cache|request_id",
               names(out), ignore.case = TRUE, value = TRUE)
  if (length(drop)) out[, (drop) := NULL]
  data.table::fwrite(out, path, bom = TRUE)
  invisible(TRUE)
}

write_dependency_sparse_csv <- function(sparse, path) {
  ed <- data.table::as.data.table(sparse$edges %||% data.table::data.table())
  write_dependency_csv(ed, path)
}

write_dependency_mtx <- function(sparse, path) {
  ed <- data.table::as.data.table(sparse$edges %||% data.table::data.table())
  nodes <- sparse$nodes$node_id %||% character()
  n <- length(nodes)
  if (!n) {
    writeLines(c("%%MatrixMarket matrix coordinate real general", "0 0 0"), path)
    return(invisible(TRUE))
  }
  idx <- setNames(seq_along(nodes), nodes)
  i <- unname(idx[ed$from_node])
  j <- unname(idx[ed$to_node])
  v <- ed$weight
  ok <- !is.na(i) & !is.na(j) & is.finite(v)
  lines <- c(
    "%%MatrixMarket matrix coordinate real general",
    sprintf("%d %d %d", n, n, sum(ok)),
    sprintf("%d %d %.10g", i[ok], j[ok], v[ok])
  )
  writeLines(lines, path)
  invisible(TRUE)
}

dependency_diagnostics_download <- function(built, sparse = NULL, coverage = NULL) {
  excl <- built$diagnostics$excluded %||% data.table::data.table()
  recon <- built$reconciliation %||% list()
  base <- data.table::data.table(
    metric = c(
      "source_rows", "eligible_rows", "eligible_value",
      "share_reconciliation_ok", "share_max_abs_error", "share_groups", "share_failures",
      "sparse_nodes", "sparse_edges", "sparse_density",
      "production_status", "represented_reporters", "selected_reporters",
      "universe_checksum"
    ),
    value = c(
      built$diagnostics$source_rows %||% NA,
      built$diagnostics$eligible_rows %||% NA,
      built$diagnostics$eligible_value %||% NA,
      recon$ok %||% NA,
      recon$max_abs_error %||% NA,
      recon$n_groups %||% NA,
      recon$failures %||% NA,
      sparse$n_nodes %||% NA,
      sparse$n_edges %||% NA,
      sparse$density %||% NA,
      coverage$production_status %||% NA_character_,
      coverage$represented_reporter_count %||% NA,
      coverage$selected_reporter_count %||% NA,
      coverage$universe_checksum %||% NA_character_
    )
  )
  if (nrow(excl)) {
    extra <- excl[, .(metric = paste0("excluded_", reason), value = as.character(n_rows))]
    base <- rbind(base, extra, fill = TRUE)
  }
  base
}
