args <- commandArgs(trailingOnly = TRUE)

source_root <- if (length(args) >= 1L) {
  args[[1L]]
} else {
  "/opt/gtsc/app/data/release/demo"
}

dest_root <- if (length(args) >= 2L) {
  args[[2L]]
} else {
  "/opt/gtsc/runtime-rds"
}

if (!dir.exists(source_root)) {
  stop("Demo source directory does not exist: ", source_root)
}

dir.create(dest_root, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(dest_root)) {
  stop("Could not create RDS runtime directory: ", dest_root)
}

parquet_files <- list.files(
  source_root,
  pattern = "\\.parquet$",
  full.names = TRUE
)

if (!length(parquet_files)) {
  stop("No Parquet files found under: ", source_root)
}

message(
  "Converting ",
  length(parquet_files),
  " demo Parquet files to runtime RDS cache"
)

for (parquet_path in parquet_files) {
  rds_name <- sub(
    "\\.parquet$",
    ".rds",
    basename(parquet_path),
    ignore.case = TRUE
  )

  rds_path <- file.path(dest_root, rds_name)

  value <- data.table::as.data.table(
    arrow::read_parquet(parquet_path)
  )

  saveRDS(
    value,
    rds_path,
    compress = "gzip",
    version = 3
  )

  check <- readRDS(rds_path)

  if (!identical(names(value), names(check)) ||
      nrow(value) != nrow(check)) {
    stop(
      "RDS verification failed for: ",
      basename(parquet_path)
    )
  }

  message(
    basename(parquet_path),
    " -> ",
    rds_name,
    " [",
    nrow(value),
    " rows]"
  )

  rm(value, check)
  gc(verbose = FALSE)
}

message(
  "Demo RDS conversion complete: ",
  normalizePath(dest_root, winslash = "/", mustWork = FALSE)
)
