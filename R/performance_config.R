PERF_ENGINE_VERSION <- "1.0.0-phase13"
PERF_FIXTURE_VERSION <- "perf_fx_v1"

default_performance_config <- function() {
  list(
    benchmark_id = paste0("perf_", format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")),
    benchmark_mode = "smoke",
    phase_label = "baseline",
    iterations = 3L,
    warmup_iterations = 1L,
    synthetic_seed = 13013L,
    synthetic_reporters = 20L,
    synthetic_partners = 20L,
    synthetic_hs4 = 20L,
    synthetic_years = 6L,
    synthetic_active_nodes = 200L,
    synthetic_edge_density = 0.08,
    scenario_mode = "capacity_constrained",
    include_persistence = FALSE,
    include_propagation = TRUE,
    profile_memory = TRUE,
    profile_reactives = FALSE,
    universe_version = EXPECTED_UNIVERSE_CHECKSUM,
    fixture_version = PERF_FIXTURE_VERSION,
    output_root = "data/performance"
  )
}

normalise_performance_config <- function(cfg = list()) {
  base <- default_performance_config()
  for (nm in names(cfg)) base[[nm]] <- cfg[[nm]]

  mode <- tolower(as.character(Sys.getenv("GTSC_PERF_MODE", unset = base$benchmark_mode)))
  if (nzchar(mode)) base$benchmark_mode <- mode
  iters <- suppressWarnings(as.integer(Sys.getenv("GTSC_PERF_ITERATIONS", unset = "")))
  if (!is.na(iters) && iters > 0L) base$iterations <- iters
  warm <- suppressWarnings(as.integer(Sys.getenv("GTSC_PERF_WARMUP", unset = "")))
  if (!is.na(warm) && warm >= 0L) base$warmup_iterations <- warm
  nodes <- suppressWarnings(as.integer(Sys.getenv("GTSC_PERF_ACTIVE_NODES", unset = "")))
  if (!is.na(nodes) && nodes > 0L) base$synthetic_active_nodes <- nodes
  if (identical(tolower(Sys.getenv("GTSC_PERF_INCLUDE_PROPAGATION", "")), "true")) {
    base$include_propagation <- TRUE
  }
  if (identical(tolower(Sys.getenv("GTSC_PERF_INCLUDE_PROPAGATION", "")), "false")) {
    base$include_propagation <- FALSE
  }
  if (identical(tolower(Sys.getenv("GTSC_PERF_INCLUDE_PERSISTENCE", "")), "true")) {
    base$include_persistence <- TRUE
  }
  if (identical(tolower(Sys.getenv("GTSC_PERF_PROFILE_MEMORY", "")), "true")) {
    base$profile_memory <- TRUE
  }
  phase <- Sys.getenv("GTSC_PERF_PHASE", unset = "")
  if (nzchar(phase)) base$phase_label <- tolower(phase)

  out_dir <- Sys.getenv("GTSC_PERF_OUTPUT_DIR", unset = "")
  if (nzchar(out_dir)) base$output_root <- out_dir

  validate_performance_config(base)
}

validate_performance_config <- function(cfg) {
  if (is.null(cfg) || !is.list(cfg)) stop("Performance config must be a list.", call. = FALSE)
  if (as.integer(cfg$iterations %||% 0L) < 1L) {
    stop("iterations must be >= 1", call. = FALSE)
  }
  if (as.integer(cfg$warmup_iterations %||% -1L) < 0L) {
    stop("warmup_iterations must be >= 0", call. = FALSE)
  }
  nodes <- as.integer(cfg$synthetic_active_nodes %||% 0L)
  if (is.na(nodes) || nodes < 1L || nodes > 500L) {
    stop("synthetic_active_nodes must be between 1 and 500", call. = FALSE)
  }
  root <- as.character(cfg$output_root %||% "")
  if (!nzchar(root) || grepl("^(/|[A-Za-z]:\\\\|~)", root) || grepl("\\.\\.", root)) {

    if (!grepl("^data[/\\\\]performance", root)) {
      stop("Unsafe performance output root; use a relative path under data/performance", call. = FALSE)
    }
  }
  if (!grepl("^data[/\\\\]performance", root)) {
    stop("output_root must be under data/performance", call. = FALSE)
  }
  cfg$iterations <- as.integer(cfg$iterations)
  cfg$warmup_iterations <- as.integer(cfg$warmup_iterations)
  cfg$synthetic_active_nodes <- as.integer(cfg$synthetic_active_nodes)
  cfg$synthetic_seed <- as.integer(cfg$synthetic_seed)
  cfg
}

performance_paths <- function(cfg = normalise_performance_config()) {
  root <- cfg$output_root
  list(
    root = root,
    fixtures = file.path(root, "fixtures"),
    profiles = file.path(root, "profiles"),
    results = file.path(root, "results"),
    reports = file.path(root, "reports")
  )
}

ensure_performance_dirs <- function(cfg = normalise_performance_config()) {
  p <- performance_paths(cfg)
  lapply(p, ensure_dir)
  invisible(p)
}

safe_git_head <- function() {
  tryCatch({
    out <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)
    if (length(out)) as.character(out[1]) else NA_character_
  }, error = function(e) NA_character_)
}

safe_git_dirty <- function() {
  tryCatch({
    out <- system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE)
    length(out) > 0L
  }, error = function(e) NA)
}

capture_benchmark_environment <- function(cfg = normalise_performance_config()) {
  pkgs <- c("data.table", "shiny", "plotly", "leaflet", "igraph", "arrow", "forecast", "prophet")
  vers <- lapply(pkgs, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
  })
  names(vers) <- pkgs
  list(
    generated_at = utc_now(),
    R_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
    logical_cpus = tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_),
    package_versions = vers,
    git_head = safe_git_head(),
    git_dirty = safe_git_dirty(),
    universe_version = cfg$universe_version,
    fixture_version = cfg$fixture_version,
    engine_version = PERF_ENGINE_VERSION,
    benchmark_mode = cfg$benchmark_mode,
    phase_label = cfg$phase_label,
    iterations = cfg$iterations,
    warmup_iterations = cfg$warmup_iterations,
    contains_credentials = FALSE,
    contains_absolute_user_paths = FALSE
  )
}

strip_unsafe_metadata_paths <- function(x) {
  if (is.list(x)) {
    return(lapply(x, strip_unsafe_metadata_paths))
  }
  if (is.character(x)) {
    x <- gsub("[A-Za-z]:\\\\Users\\\\[^/\\\\]+", "<user>", x)
    x <- gsub("/Users/[^/]+", "<user>", x)
    x <- gsub("COMTRADE_PRIMARY=[^\\s]+", "COMTRADE_PRIMARY=<redacted>", x)
  }
  x
}
