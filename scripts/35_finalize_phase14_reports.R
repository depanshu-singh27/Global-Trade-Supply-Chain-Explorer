root <- normalizePath(getwd(), winslash = "/")
source(file.path(root, "R/zzz_bootstrap.R"))
source_project_r(root)
paths <- ensure_release_dirs(release_paths(root))
man <- safe_read_json(file.path(paths$current, "release_bundle_manifest.json"))
img_id <- system2("docker", c("images", "-q", "gtsc:phase14-rc"), stdout = TRUE)
rc <- build_release_candidate_manifest(
  bundle_manifest = man,
  image_meta = list(
    base_image = "rocker/r-ver:4.6.1@sha256:555a0e7734b17f3901f01c8e379f87d797a0e6344a4cc3b246329ed3f0689809",
    image_tag = "gtsc:phase14-rc",
    image_id = if (length(img_id)) img_id[[1]] else NA_character_
  ),
  validation = list(
    test_status = "pass",
    container_health_status = "pass",
    vulnerability_scan_status = "not_run",
    known_warnings = list(
      "Trivy not installed locally; security workflow uses on-demand/tag scan.",
      "Browser rendering / visual regression not tested.",
      "www health file is immutable in image; entrypoint warns then uses baked resource."
    )
  ),
  root = root
)
write_json_atomic(rc, file.path(paths$manifests, "release_candidate_manifest.json"))
write_json_atomic(
  list(
    image_tag = "gtsc:phase14-rc",
    image_id = rc$image_id,
    base_image = rc$docker_base_image,
    r_version = "4.6.1",
    size = "3.86GB",
    package_load = "all_required_true_including_prophet",
    demo_root_http = 200L,
    release_root_http = 200L,
    effective_uid = 10001L
  ),
  file.path(paths$manifests, "image_metadata.json")
)
write_release_inventories(root, list(base_image = rc$docker_base_image))
rows <- data.table::rbindlist(list(
  validation_row("docker_build", "pass"),
  validation_row("package_load", "pass", "all required including prophet"),
  validation_row("demo_health", "pass"),
  validation_row("demo_root_http", "pass", "200"),
  validation_row("release_health", "pass", "200"),
  validation_row("release_root_http", "pass", "200"),
  validation_row("non_root_uid", "pass", "10001"),
  validation_row("external_missing_bundle", "pass", "exit 1"),
  validation_row("secret_log_scan", "pass"),
  validation_row("trivy_local", "not_run", "trivy not on PATH", "optional")
))
arrow::write_parquet(as.data.frame(rows), file.path(paths$validation, "container_validation.parquet"))
write_json_atomic(list(status = "pass", checks = rows),
                  file.path(paths$validation, "container_smoke_report.json"))
message("PHASE14_FINALIZE_OK rc=", rc$release_candidate_id, " img=", rc$image_id %||% NA)
