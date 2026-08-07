root <- normalizePath(getwd(), winslash = "/")
if (file.exists(file.path(root, "renv/activate.R"))) source(file.path(root, "renv/activate.R"))
source(file.path(root, "R/zzz_bootstrap.R"))
source_project_r(root)

paths <- ensure_release_dirs(release_paths(root))
bundle_man <- NULL
for (cand in c(file.path(paths$current, "release_bundle_manifest.json"),
               file.path(paths$demo, "release_bundle_manifest.json"))) {
  if (file.exists(cand)) {
    bundle_man <- safe_read_json(cand)
    break
  }
}
inv <- write_release_inventories(root, list(base_image = "rocker/r-ver:4.6.1"))
rc <- build_release_candidate_manifest(
  bundle_manifest = bundle_man,
  image_meta = list(
    base_image = "rocker/r-ver:4.6.1@sha256:555a0e7734b17f3901f01c8e379f87d797a0e6344a4cc3b246329ed3f0689809",
    image_tag = Sys.getenv("GTSC_RELEASE_IMAGE_TAG", "gtsc:phase14-rc")
  ),
  validation = list(
    test_status = "not_run",
    container_health_status = "not_run",
    vulnerability_scan_status = "not_run"
  ),
  root = root
)
out <- file.path(paths$manifests, "release_candidate_manifest.json")
write_json_atomic(rc, out)
message("RELEASE_MANIFEST_OK=", out)
message("RC_ID=", rc$release_candidate_id)
invisible(NULL)
