RELEASE_APP_VERSION <- "0.1.0-rc.14"
RELEASE_ENGINE_VERSION <- "1.0.0-phase14"

runtime_profile_names <- function() c("demo", "release", "external")

default_runtime_config <- function() {
  list(
    runtime_profile = "demo",
    data_root = "data/processed",
    scenario_root = "data/scenarios",
    performance_root = "data/performance",
    host = "0.0.0.0",
    port = 3838L,
    log_level = "INFO",
    public_mode = TRUE,
    allow_scenario_writes = FALSE,
    read_only_mode = TRUE,
    enable_technical_diagnostics = FALSE,
    release_manifest = NA_character_,
    healthcheck_path = "/__gtsc_health__",
    startup_timeout_seconds = 120L,
    app_version = RELEASE_APP_VERSION
  )
}

parse_env_bool <- function(name, default = FALSE) {
  raw <- Sys.getenv(name, unset = "")
  if (!nzchar(raw)) return(isTRUE(default))
  tolower(raw) %in% c("1", "true", "yes", "on")
}

normalise_runtime_config <- function(cfg = list()) {
  base <- default_runtime_config()
  for (nm in names(cfg)) base[[nm]] <- cfg[[nm]]

  prof <- Sys.getenv("GTSC_RUNTIME_PROFILE", unset = "")
  if (nzchar(prof)) base$runtime_profile <- tolower(prof)
  data_root <- Sys.getenv("GTSC_DATA_ROOT", unset = "")
  if (nzchar(data_root)) base$data_root <- data_root
  scen <- Sys.getenv("GTSC_SCENARIO_ROOT", unset = "")
  if (nzchar(scen)) base$scenario_root <- scen
  perf <- Sys.getenv("GTSC_PERFORMANCE_ROOT", unset = "")
  if (nzchar(perf)) base$performance_root <- perf
  host <- Sys.getenv("GTSC_HOST", unset = "")
  if (nzchar(host)) base$host <- host
  port <- suppressWarnings(as.integer(Sys.getenv("GTSC_PORT", unset = "")))
  if (!is.na(port)) base$port <- port
  lvl <- Sys.getenv("GTSC_LOG_LEVEL", unset = "")
  if (nzchar(lvl)) base$log_level <- toupper(lvl)
  if (nzchar(Sys.getenv("GTSC_PUBLIC_MODE", ""))) {
    base$public_mode <- parse_env_bool("GTSC_PUBLIC_MODE", TRUE)
  }
  if (nzchar(Sys.getenv("GTSC_ALLOW_SCENARIO_WRITES", ""))) {
    base$allow_scenario_writes <- parse_env_bool("GTSC_ALLOW_SCENARIO_WRITES", FALSE)
  }
  if (nzchar(Sys.getenv("GTSC_READ_ONLY_MODE", ""))) {
    base$read_only_mode <- parse_env_bool("GTSC_READ_ONLY_MODE", TRUE)
  }
  if (nzchar(Sys.getenv("GTSC_ENABLE_TECHNICAL_DIAGNOSTICS", ""))) {
    base$enable_technical_diagnostics <- parse_env_bool("GTSC_ENABLE_TECHNICAL_DIAGNOSTICS", FALSE)
  }
  man <- Sys.getenv("GTSC_RELEASE_MANIFEST", unset = "")
  if (nzchar(man)) base$release_manifest <- man
  hp <- Sys.getenv("GTSC_HEALTHCHECK_PATH", unset = "")
  if (nzchar(hp)) base$healthcheck_path <- hp
  to <- suppressWarnings(as.integer(Sys.getenv("GTSC_STARTUP_TIMEOUT_SECONDS", unset = "")))
  if (!is.na(to)) base$startup_timeout_seconds <- to

  validate_runtime_config(base)
}

validate_runtime_config <- function(cfg) {
  if (is.null(cfg) || !is.list(cfg)) stop("Runtime config must be a list.", call. = FALSE)
  prof <- tolower(as.character(cfg$runtime_profile %||% ""))
  if (!prof %in% runtime_profile_names()) {
    stop("Unknown runtime profile: ", prof, call. = FALSE)
  }
  cfg$runtime_profile <- prof
  port <- as.integer(cfg$port %||% NA_integer_)
  if (is.na(port) || port < 1L || port > 65535L) {
    stop("GTSC_PORT must be between 1 and 65535", call. = FALSE)
  }
  cfg$port <- port
  host <- as.character(cfg$host %||% "")
  if (!nzchar(host) || grepl("[[:space:]]", host)) {
    stop("Invalid GTSC_HOST", call. = FALSE)
  }
  for (nm in c("data_root", "scenario_root", "performance_root")) {
    p <- as.character(cfg[[nm]] %||% "")
    if (!nzchar(p)) stop("Missing ", nm, call. = FALSE)
    if (grepl("\\.\\.", p) || grepl("^~", p)) {
      stop("Unsafe path for ", nm, call. = FALSE)
    }
  }

  if (isTRUE(cfg$read_only_mode) && isTRUE(cfg$allow_scenario_writes)) {
    cfg$read_only_warning <- "read_only_mode=true with allow_scenario_writes=true; writes limited to scenario_root"
  }
  cfg$comtrade_key_required <- FALSE
  cfg
}

runtime_allows_scenario_persistence <- function(cfg = normalise_runtime_config()) {

  isTRUE(cfg$allow_scenario_writes) && !isTRUE(cfg$read_only_mode)
}

apply_runtime_paths_to_config <- function(app_cfg, runtime_cfg = normalise_runtime_config()) {
  root <- app_cfg$project_root %||% find_project_root()

  app_cfg$paths$processed <- resolve_project_path(runtime_cfg$data_root, root)
  app_cfg$runtime <- runtime_cfg
  app_cfg
}

get_runtime_config <- function() {
  if (exists(".gtsc_runtime_cfg", envir = .GlobalEnv, inherits = FALSE)) {
    return(get(".gtsc_runtime_cfg", envir = .GlobalEnv))
  }
  normalise_runtime_config()
}

set_runtime_config <- function(cfg) {
  assign(".gtsc_runtime_cfg", cfg, envir = .GlobalEnv)
  invisible(cfg)
}
