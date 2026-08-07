root <- normalizePath(getwd(), winslash = "/")
if (file.exists(file.path(root, "renv/activate.R"))) source(file.path(root, "renv/activate.R"))
source(file.path(root, "R/zzz_bootstrap.R"))
source_project_r(root)

paths <- ensure_release_dirs(release_paths(root))
rows <- list()
add <- function(check, status, detail = NA_character_, severity = "mandatory") {
  rows[[length(rows) + 1L]] <<- validation_row(check, status, detail, severity)
}

cfg <- tryCatch(normalise_runtime_config(list(runtime_profile = "demo")), error = function(e) NULL)
add("runtime_configuration", if (!is.null(cfg)) "pass" else "fail")

demo_dir <- paths$demo
if (dir.exists(demo_dir) && file.exists(file.path(demo_dir, "release_bundle_manifest.json"))) {
  vr <- validate_release_bundle(demo_dir, expected_profile = "demo")
  add("demo_bundle_validation", gate_status_from_rows(vr),
      paste(vr$check[vr$status == "fail"], collapse = ","))
} else {
  add("demo_bundle_validation", "not_run", "demo bundle missing")
}

rel_dir <- paths$current
if (dir.exists(rel_dir) && file.exists(file.path(rel_dir, "release_bundle_manifest.json"))) {
  vr <- validate_release_bundle(rel_dir, expected_profile = "release")
  add("release_bundle_validation", gate_status_from_rows(vr),
      paste(vr$check[vr$status == "fail"], collapse = ","))
} else {
  add("release_bundle_validation", "not_run", "release bundle not built", "optional")
}

man_path <- file.path(paths$manifests, "release_candidate_manifest.json")
add("release_manifest", if (file.exists(man_path)) "pass" else "not_run")


wf <- file.path(root, ".github/workflows")
add("ci_r_tests",
    if (file.exists(file.path(wf, "r-tests.yml"))) "pass" else "fail")
add("ci_container",
    if (file.exists(file.path(wf, "container-build.yml"))) "pass" else "fail")
add("ci_security",
    if (file.exists(file.path(wf, "security-scan.yml"))) "pass" else "fail")

add("dockerfile", if (file.exists(file.path(root, "Dockerfile"))) "pass" else "fail")
add("compose", if (file.exists(file.path(root, "docker-compose.yml"))) "pass" else "fail")
add("entrypoint", if (file.exists(file.path(root, "docker/entrypoint.sh"))) "pass" else "fail")

cv <- file.path(paths$validation, "container_validation.parquet")
if (file.exists(cv)) {
  cdt <- data.table::as.data.table(arrow::read_parquet(cv))
  add("container_smoke", gate_status_from_rows(cdt))
} else {
  add("container_smoke", "not_run", severity = "optional")
}

renv_st <- tryCatch({
  s <- renv::status()
  "pass"
}, error = function(e) "warning")
add("renv_status", renv_st, severity = "mandatory")

out <- data.table::rbindlist(rows, fill = TRUE)
arrow::write_parquet(as.data.frame(out),
                     file.path(paths$validation, "release_validation.parquet"))

gate <- gate_status_from_rows(out[severity == "mandatory"])
warnings <- out[status %in% c("warning", "not_run")]
report <- c(
  "# Phase 14 release-candidate report",
  "",
  paste0("- Gate: **", toupper(gate), "**"),
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
  "",
  "## Checks",
  paste0("- ", out$check, ": ", out$status,
         ifelse(is.na(out$detail) | !nzchar(out$detail), "", paste0(" (", out$detail, ")")))
)
writeLines(report, file.path(paths$validation, "release_candidate_report.md"))
print(out)
message("RELEASE_CANDIDATE_GATE=", gate)
if (identical(gate, "fail")) quit(status = 1)
invisible(out)
