find_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in seq_len(20L)) {
    if (file.exists(file.path(path, "config.yml")) ||
        file.exists(file.path(path, "global-trade-explorer.Rproj")) ||
        file.exists(file.path(path, "DESCRIPTION"))) {
      return(path)
    }
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  normalizePath(start, winslash = "/", mustWork = FALSE)
}

merge_config <- function(base, override) {
  if (is.null(override)) return(base)
  if (is.null(base)) return(override)
  if (!is.list(base) || !is.list(override) || is.null(names(override))) {
    return(override)
  }
  out <- base
  for (nm in names(override)) {
    if (nm %in% names(out) && is.list(out[[nm]]) && is.list(override[[nm]])) {
      out[[nm]] <- merge_config(out[[nm]], override[[nm]])
    } else {
      out[[nm]] <- override[[nm]]
    }
  }
  out
}

load_config <- function(env = Sys.getenv("GTE_ENV", "development"),
                        root = find_project_root()) {
  cfg_path <- file.path(root, "config.yml")
  if (!file.exists(cfg_path)) {
    stop("config.yml not found at: ", cfg_path, call. = FALSE)
  }
  raw <- yaml::read_yaml(cfg_path)

  if (is.null(raw[['default']])) {
    stop("config.yml must contain a 'default' section.", call. = FALSE)
  }
  env <- tolower(env)
  if (!env %in% names(raw)) {
    warning("Unknown environment '", env, "'; falling back to default.",
            call. = FALSE)
    env <- "default"
  }
  cfg <- merge_config(raw[['default']], if (env == "default") NULL else raw[[env]])

  cfg[['paths']] <- lapply(cfg[['paths']], function(p) resolve_project_path(p, root))
  cfg$project_root <- root
  cfg$environment <- env
  cfg
}

ensure_data_dirs <- function(cfg = load_config()) {
  dirs <- unique(unlist(cfg[['paths']], use.names = FALSE))
  extra <- c(
    file.path(cfg[['paths']]$raw, "comtrade"),
    file.path(cfg[['paths']]$raw, "wdi")
  )
  for (d in c(dirs, extra)) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(dirs)
}

comtrade_key_present <- function() {
  nzchar(Sys.getenv("COMTRADE_PRIMARY"))
}
