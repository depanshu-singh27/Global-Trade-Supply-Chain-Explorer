root <- normalizePath(getwd(), winslash = "/")
if (file.exists(file.path(root, "renv/activate.R"))) source(file.path(root, "renv/activate.R"))
source(file.path(root, "R/zzz_bootstrap.R"))
source_project_r(root)

src <- file.path(root, "data", "processed")
paths <- ensure_release_dirs(release_paths(root))
dest <- paths$current
if (dir.exists(dest)) {
  unlink(list.files(dest, full.names = TRUE), recursive = TRUE, force = TRUE)
}
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

res <- build_release_bundle(src, dest, profile = "release", root = root)

file.copy(res$manifest_path,
          file.path(paths$manifests, "release_bundle_manifest.json"),
          overwrite = TRUE)
message("RELEASE_BUNDLE_OK=", res$dest_dir)
message("RELEASE_FILES=", length(res$files))
message("RELEASE_GLOBAL=", res$manifest$global_production_status %||% NA)
message("RELEASE_DETAILED=", res$manifest$detailed_production_status %||% NA)
message("RELEASE_FORECAST=", res$manifest$forecast_data_mode %||% NA)
message("RELEASE_UNIVERSE=", res$manifest$universe_checksum %||% NA)
