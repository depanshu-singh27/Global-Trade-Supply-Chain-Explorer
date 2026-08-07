runtime_log <- function(level = "INFO", message, fields = list()) {
  level <- toupper(as.character(level %||% "INFO"))
  safe_fields <- redact_runtime_log_fields(fields)
  payload <- list(
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    level = level,
    msg = as.character(message %||% "")
  )
  if (length(safe_fields)) payload <- c(payload, safe_fields)
  line <- paste0(
    "{\"ts\":\"", payload$ts, "\",",
    "\"level\":\"", payload$level, "\",",
    "\"msg\":", jsonlite::toJSON(payload$msg, auto_unbox = TRUE)
  )
  extras <- setdiff(names(payload), c("ts", "level", "msg"))
  for (nm in extras) {
    val <- payload[[nm]]
    if (is.null(val) || (length(val) == 1L && is.na(val))) next
    line <- paste0(
      line, ",\"", nm, "\":",
      jsonlite::toJSON(val, auto_unbox = TRUE, null = "null")
    )
  }
  line <- paste0(line, "}")
  if (identical(level, "ERROR") || identical(level, "WARN")) {
    message(line)
  } else {
    cat(line, "\n", sep = "", file = stdout())
  }
  invisible(line)
}

redact_runtime_log_fields <- function(fields) {
  if (!length(fields)) return(list())
  banned <- c(
    "COMTRADE_PRIMARY", "COMTRADE_SECONDARY", "COMTRADE_API_KEY",
    "api_key", "subscription_key", "password", "token", "secret",
    "authorization", "env", "environment", "full_env"
  )
  out <- list()
  for (nm in names(fields)) {
    if (tolower(nm) %in% tolower(banned)) next
    val <- fields[[nm]]
    if (is.character(val) && length(val) == 1L) {
      if (grepl("COMTRADE|subscription[_-]?key|api[_-]?key", val, ignore.case = TRUE)) next

      if (grepl("^[A-Za-z]:\\\\|^/Users/|^/home/", val)) {
        val <- basename(val)
      }
    }
    out[[nm]] <- val
  }
  out
}

log_runtime_data_diagnostics <- function(cfg, runtime_cfg, snap,
                                         load_seconds = NA_real_) {
  data_root <- as.character(cfg[["paths"]]$processed %||% "")
  report <- snapshot_table_report(snap)
  runtime_log("INFO", "runtime_data_root", list(
    runtime_profile = runtime_cfg$runtime_profile,
    data_root = data_root,
    data_root_exists = dir.exists(data_root),
    bundle_manifest = file.path(data_root, "release_bundle_manifest.json"),
    bundle_manifest_exists =
      file.exists(file.path(data_root, "release_bundle_manifest.json")),
    working_directory = getwd(),
    project_root = as.character(cfg$project_root %||% ""),
    data_files_discovered = length(list.files(data_root)),
    snapshot_load_seconds = round(as.numeric(load_seconds), 2),
    snapshot_components = length(snap),
    snapshot_tables = nrow(report)
  ))
  if (nrow(report)) {
    runtime_log("INFO", "runtime_snapshot_tables", list(
      tables = paste0(report$table, "=", report$rows, collapse = ",")
    ))
  }
  invisible(report)
}

log_module_registration <- function(modules, status = "registered") {
  runtime_log("INFO", "module_registration", list(
    status = status,
    count = length(modules),
    modules = paste(modules, collapse = ",")
  ))
}

log_output_error <- function(output_id, error) {
  call_ctx <- tryCatch(conditionCall(error), error = function(e) NULL)
  runtime_log("ERROR", "output_error", list(
    output_id = as.character(output_id %||% "<unknown>"),
    error_class = paste(class(error), collapse = ","),
    error_message = conditionMessage(error),
    call_context = if (is.null(call_ctx)) {
      "<none>"
    } else {
      paste(utils::head(deparse(call_ctx), 1L), collapse = " ")
    }
  ))
}

install_runtime_error_logger <- function(public_mode = TRUE) {
  if (isTRUE(public_mode)) {
    options(shiny.sanitize.errors = TRUE)
  }
  if (!is.function(getExportedValue_safe("shiny", "onUnhandledError"))) {
    return(invisible(FALSE))
  }
  ok <- tryCatch({
    shiny::onUnhandledError(function(error) log_output_error("<session>", error))
    TRUE
  }, error = function(e) FALSE)
  invisible(ok)
}

getExportedValue_safe <- function(pkg, name) {
  tryCatch(getExportedValue(pkg, name), error = function(e) NULL)
}

log_startup_metadata <- function(runtime_cfg, bundle_meta = list()) {
  runtime_log("INFO", "startup_metadata", list(
    app_version = runtime_cfg$app_version %||% RELEASE_APP_VERSION,
    git_commit = bundle_meta$git_head %||% git_release_metadata()$git_head,
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    runtime_profile = runtime_cfg$runtime_profile,
    data_mode = bundle_meta$data_mode %||% runtime_cfg$runtime_profile,
    global_status = bundle_meta$global_production_status %||% NA_character_,
    detailed_coverage = bundle_meta$detailed_production_status %||% NA_character_,
    forecast_provenance = bundle_meta$forecast_data_mode %||% NA_character_,
    read_only = isTRUE(runtime_cfg$read_only_mode),
    scenario_writes = isTRUE(runtime_cfg$allow_scenario_writes),
    public_mode = isTRUE(runtime_cfg$public_mode),
    host = runtime_cfg$host,
    port = runtime_cfg$port
  ))
}
