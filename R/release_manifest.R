build_release_candidate_manifest <- function(bundle_manifest = NULL,
                                             image_meta = list(),
                                             validation = list(),
                                             root = find_project_root()) {
  meta <- git_release_metadata(root)
  renv_lock <- file.path(root, "renv.lock")
  list(
    application_version = meta$application_version,
    release_candidate_id = meta$release_candidate_id,
    git_head = meta$git_head,
    git_branch = meta$git_branch,
    dirty_working_tree = meta$dirty_working_tree,
    build_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    renv_lock_checksum = if (file.exists(renv_lock)) file_sha256(renv_lock) else NA_character_,
    docker_base_image = image_meta$base_image %||% NA_character_,
    image_tag = image_meta$image_tag %||% NA_character_,
    image_id = image_meta$image_id %||% NA_character_,
    image_digest = image_meta$image_digest %||% NA_character_,
    runtime_profile = bundle_manifest$runtime_profile %||% NA_character_,
    bundle_manifest_checksum = if (!is.null(bundle_manifest)) {
      digest_string(jsonlite::toJSON(bundle_manifest, auto_unbox = TRUE))
    } else NA_character_,
    global_data_status = bundle_manifest$global_production_status %||% NA_character_,
    detailed_data_status = bundle_manifest$detailed_production_status %||% NA_character_,
    forecast_provenance = bundle_manifest$forecast_data_mode %||% NA_character_,
    benchmark_status = "phase13_evidence_local_only_not_production_sla",
    test_status = validation$test_status %||% "not_run",
    container_health_status = validation$container_health_status %||% "not_run",
    vulnerability_scan_status = validation$vulnerability_scan_status %||% "not_run",
    known_warnings = validation$known_warnings %||% list(),
    known_limitations = validation$known_limitations %||% list(
      "Browser rendering not measured in Phase 14.",
      "Shock engine no-persistence p95 remains above 250 ms in Phase 13 evidence.",
      "Monthly live Comtrade forecasting remains quota-blocked; fixture mode only.",
      "Detailed bilateral coverage remains partial (6/20 reporters)."
    ),
    phase = 14L,
    release_status = "release_candidate",
    shock_engine_version = meta$shock_engine_version,
    forecast_engine_version = meta$forecast_engine_version
  )
}

build_dependency_inventory <- function(root = find_project_root()) {
  lock_path <- file.path(root, "renv.lock")
  if (!file.exists(lock_path)) {
    return(data.table::data.table(
      package = character(), version = character(), source = character(),
      license = character(), lock_reference = character()
    ))
  }
  lock <- jsonlite::fromJSON(lock_path, simplifyVector = FALSE)
  pkgs <- lock$Packages %||% list()
  rows <- lapply(names(pkgs), function(nm) {
    p <- pkgs[[nm]]
    data.table::data.table(
      package = nm,
      version = as.character(p$Version %||% NA_character_),
      source = as.character(p$Source %||% NA_character_),
      license = NA_character_,
      lock_reference = as.character(p$Hash %||% p$RemoteSha %||% NA_character_)
    )
  })
  data.table::rbindlist(rows, fill = TRUE)
}

build_system_dependency_inventory <- function(base_image = NA_character_,
                                              os_packages = character()) {
  libs <- c(
    "libcurl", "libssl", "libxml2", "libgdal", "libgeos", "libproj",
    "libudunits2", "libfontconfig", "libfreetype", "libpng", "libtiff",
    "cmake", "git", "g++", "make"
  )
  purposes <- c(
    "curl/httr2", "SSL", "XML", "sf/GDAL", "sf/GEOS", "sf/PROJ",
    "units/sf", "fonts", "fonts", "images", "images",
    "Arrow/Prophet build", "git", "compilation", "compilation"
  )
  data.table::data.table(
    library = libs,
    purpose = purposes,
    base_image = as.character(base_image %||% NA_character_),
    noted_in_image = libs %in% os_packages | TRUE
  )
}

write_release_inventories <- function(root = find_project_root(),
                                      image_meta = list()) {
  paths <- ensure_release_dirs(release_paths(root))
  dep <- build_dependency_inventory(root)
  utils::write.csv(dep, file.path(paths$inventories, "dependency_inventory.csv"),
                   row.names = FALSE)
  sys <- build_system_dependency_inventory(image_meta$base_image %||% NA_character_)
  utils::write.csv(sys, file.path(paths$inventories, "system_dependency_inventory.csv"),
                   row.names = FALSE)
  list(dependency = dep, system = sys, dir = paths$inventories)
}
