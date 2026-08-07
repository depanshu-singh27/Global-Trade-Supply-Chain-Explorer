args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(getwd(), winslash = "/")
if (file.exists(file.path(root, "renv/activate.R"))) source(file.path(root, "renv/activate.R"))
source(file.path(root, "R/zzz_bootstrap.R"))
source_project_r(root)

paths <- release_paths(root)
bundle <- if (length(args)) args[[1]] else paths$current
profile <- Sys.getenv("GTSC_RELEASE_PROFILE", unset = "")
if (!nzchar(profile)) profile <- NULL

res <- validate_release_bundle(bundle, expected_profile = profile)
out <- file.path(paths$validation, "release_validation.parquet")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(as.data.frame(res), out)
print(res)
fails <- res[status == "fail"]
if (nrow(fails)) {
  message("BUNDLE_VALIDATION_FAIL")
  quit(status = 1)
}
message("BUNDLE_VALIDATION_OK")
