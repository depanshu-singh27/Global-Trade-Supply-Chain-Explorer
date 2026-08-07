root <- normalizePath(getwd(), winslash = "/")
if (file.exists(file.path(root, "renv/activate.R"))) source(file.path(root, "renv/activate.R"))
source(file.path(root, "R/zzz_bootstrap.R"))
source_project_r(root)

build_image <- parse_env_bool("GTSC_RELEASE_BUILD_IMAGE", TRUE)
run_container <- parse_env_bool("GTSC_RELEASE_RUN_CONTAINER", TRUE)
security_scan <- parse_env_bool("GTSC_RELEASE_SECURITY_SCAN", FALSE)
image_tag <- Sys.getenv("GTSC_RELEASE_IMAGE_TAG", "gtsc:phase14-rc")

message("=== Phase 14: configuration ===")
cfg <- normalise_runtime_config(list(runtime_profile = "demo"))
print(cfg[c("runtime_profile", "public_mode", "read_only_mode", "allow_scenario_writes")])

message("=== Phase 14: demo bundle ===")
demo <- build_demo_bundle(root = root)
demo_v <- validate_release_bundle(demo$dest_dir, expected_profile = "demo")
if (any(demo_v$status == "fail")) stop("Demo bundle validation failed")

message("=== Phase 14: release bundle (if processed data present) ===")
src <- file.path(root, "data", "processed")
paths <- ensure_release_dirs(release_paths(root))
if (file.exists(file.path(src, "trade_global_hs85_annual.parquet"))) {
  if (dir.exists(paths$current)) {
    unlink(list.files(paths$current, full.names = TRUE), recursive = TRUE, force = TRUE)
  }
  dir.create(paths$current, recursive = TRUE, showWarnings = FALSE)
  rel <- build_release_bundle(src, paths$current, profile = "release", root = root)
  rel_v <- validate_release_bundle(rel$dest_dir, expected_profile = "release")
  if (any(rel_v$status == "fail")) stop("Release bundle validation failed")
} else {
  message("Processed data absent — skipping release bundle build")
}

message("=== Phase 14: release manifest + inventories ===")
source(file.path(root, "scripts/32_build_release_manifest.R"), local = TRUE)

message("=== Phase 14: offline tests ===")
test_res <- testthat::test_dir(file.path(root, "tests/testthat"), reporter = "summary")

message("=== Phase 14: renv status ===")
print(renv::status())

message("=== Phase 14: app source check ===")

Sys.setenv(GTSC_RUNTIME_PROFILE = "demo", GTSC_DATA_ROOT = demo$dest_dir)

stopifnot(file.exists(file.path(root, "app.R")))
message("APP_SOURCE_OK")

if (isTRUE(build_image) && nzchar(Sys.which("docker"))) {
  message("=== Phase 14: docker build ===")
  st <- system2("docker", c("build", "--tag", image_tag, root))
  if (!identical(st, 0L)) stop("Docker build failed")
  if (isTRUE(run_container)) {
    message("=== Phase 14: container smoke ===")
    Sys.setenv(GTSC_RELEASE_IMAGE_TAG = image_tag)
    st2 <- system2("Rscript", file.path(root, "scripts/33_validate_container_runtime.R"))
    if (!identical(st2, 0L)) stop("Container validation failed")
  }
} else {
  message("Docker build skipped (GTSC_RELEASE_BUILD_IMAGE=false or docker missing)")
}

if (isTRUE(security_scan) && nzchar(Sys.which("trivy"))) {
  message("=== Phase 14: trivy scan ===")
  system2("trivy", c("image", "--severity", "CRITICAL,HIGH", "--format", "table", image_tag))
} else {
  message("Security scan not_run (set GTSC_RELEASE_SECURITY_SCAN=true and install trivy)")
}

message("=== Phase 14: release candidate validation ===")
st3 <- system2("Rscript", file.path(root, "scripts/34_validate_release_candidate.R"))
message("PHASE14_ORCHESTRATION_DONE status=", st3)
quit(status = if (identical(st3, 0L)) 0 else 1)
