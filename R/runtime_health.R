default_healthcheck_path <- function() "/__gtsc_health__"

health_payload <- function(runtime_cfg = get_runtime_config(),
                           ready = TRUE) {
  list(
    status = if (isTRUE(ready)) "ok" else "not_ready",
    app = "global-trade-supply-chain-explorer",
    version = runtime_cfg$app_version %||% RELEASE_APP_VERSION,
    profile = runtime_cfg$runtime_profile %||% NA_character_,
    public_mode = isTRUE(runtime_cfg$public_mode),
    read_only = isTRUE(runtime_cfg$read_only_mode),
    scenario_writes = isTRUE(runtime_cfg$allow_scenario_writes),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

write_health_www_resource <- function(root = find_project_root(),
                                      runtime_cfg = get_runtime_config()) {
  www <- file.path(root, "www")
  if (!dir.exists(www)) dir.create(www, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(www, "__gtsc_health__")

  if (file.exists(path)) {
    return(invisible(path))
  }
  payload <- health_payload(runtime_cfg, ready = TRUE)
  txt <- jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE)
  hits <- scan_text_for_secrets(txt, source = "health")
  fails <- Filter(function(h) identical(h$status, "fail"), hits)
  if (length(fails)) stop("Health payload failed secret scan.", call. = FALSE)
  writeLines(txt, path, useBytes = TRUE)
  invisible(path)
}

health_check_is_side_effect_free <- function() {

  TRUE
}
