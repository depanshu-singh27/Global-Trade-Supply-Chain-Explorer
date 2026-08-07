.perf_counters_env <- new.env(parent = emptyenv())
.perf_counters_env$enabled <- FALSE
.perf_counters_env$counts <- list(
  snapshot_load_count = 0L,
  detailed_filter_count = 0L,
  graph_build_count = 0L,
  dependency_build_count = 0L,
  shock_execution_count = 0L,
  scenario_history_scan_count = 0L,
  forecast_filter_count = 0L,
  forecast_snapshot_load_count = 0L
)

perf_counters_enabled <- function() isTRUE(.perf_counters_env$enabled)

enable_perf_counters <- function(on = TRUE) {
  .perf_counters_env$enabled <- isTRUE(on)
  invisible(isTRUE(on))
}

reset_perf_counters <- function() {
  for (nm in names(.perf_counters_env$counts)) {
    .perf_counters_env$counts[[nm]] <- 0L
  }
  invisible(.perf_counters_env$counts)
}

get_perf_counters <- function() {
  as.list(.perf_counters_env$counts)
}

inc_perf_counter <- function(name) {
  if (!perf_counters_enabled()) return(invisible(NULL))
  if (!name %in% names(.perf_counters_env$counts)) {
    .perf_counters_env$counts[[name]] <- 0L
  }
  .perf_counters_env$counts[[name]] <- as.integer(.perf_counters_env$counts[[name]]) + 1L
  invisible(NULL)
}

perf_quantile <- function(x, p) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, names = FALSE, type = 7))
}

time_operation_ms <- function(expr, envir = parent.frame()) {
  t0 <- proc.time()[["elapsed"]]
  val <- tryCatch(eval(expr, envir = envir), error = function(e) e)
  t1 <- proc.time()[["elapsed"]]
  elapsed_ms <- (t1 - t0) * 1000
  list(
    value = val,
    elapsed_ms = elapsed_ms,
    ok = !inherits(val, "error"),
    error = if (inherits(val, "error")) conditionMessage(val) else NA_character_
  )
}

run_timed_iterations <- function(expr_fn,
                                   iterations = 3L,
                                   warmup = 1L,
                                   profile_memory = FALSE) {
  iterations <- as.integer(iterations)
  warmup <- as.integer(warmup)
  times <- numeric(0)
  last_value <- NULL
  mem_bytes <- NA_real_
  status <- "ok"
  warning_msg <- NA_character_

  total <- iterations + warmup
  for (i in seq_len(total)) {
    t0 <- proc.time()[["elapsed"]]
    res <- tryCatch(expr_fn(), error = function(e) e)
    t1 <- proc.time()[["elapsed"]]
    ms <- (t1 - t0) * 1000
    if (inherits(res, "error")) {
      status <- "error"
      warning_msg <- conditionMessage(res)
      last_value <- NULL
      break
    }
    last_value <- res
    if (i > warmup) times <- c(times, ms)
  }
  if (isTRUE(profile_memory) && !is.null(last_value)) {
    mem_bytes <- as.numeric(utils::object.size(last_value))
  }
  list(
    times_ms = times,
    minimum_ms = if (length(times)) min(times) else NA_real_,
    median_ms = if (length(times)) stats::median(times) else NA_real_,
    mean_ms = if (length(times)) mean(times) else NA_real_,
    p95_ms = if (length(times)) perf_quantile(times, 0.95) else NA_real_,
    maximum_ms = if (length(times)) max(times) else NA_real_,
    iterations = length(times),
    warmup_iterations = warmup,
    memory_bytes = mem_bytes,
    result = last_value,
    benchmark_status = status,
    warning = warning_msg
  )
}

result_checksum <- function(x) {

  payload <- tryCatch({
    if (is.data.frame(x)) {
      paste(nrow(x), ncol(x), paste(names(x), collapse = ","),
            digest_numeric_summary(x), sep = "|")
    } else if (is.list(x)) {
      paste(sort(names(x)), collapse = ",")
    } else {
      paste(class(x), length(x), sep = ":")
    }
  }, error = function(e) "checksum_error")
  sprintf("chk_%s", substr(digest_simple(payload), 1, 16))
}

digest_numeric_summary <- function(dt) {
  nums <- vapply(dt, is.numeric, logical(1))
  if (!any(nums)) return("nonum")
  vals <- unlist(lapply(dt[, nums, with = FALSE], function(z) {
    z <- z[is.finite(z)]
    if (!length(z)) return(0)
    c(sum(z), mean(z), length(z))
  }), use.names = FALSE)
  sprintf("%.6g", sum(vals, na.rm = TRUE))
}

digest_simple <- function(text) {

  bytes <- as.integer(charToRaw(enc2utf8(as.character(text)[1])))
  h <- 2166136261
  for (b in bytes) {
    h <- (h + b + 1) %% 2147483647
    h <- (h * 16777619) %% 2147483647
  }
  sprintf("%08x", as.integer(h %% 2147483647))
}

make_benchmark_row <- function(timed,
                                 meta,
                                 operation,
                                 module,
                                 dataset_tier = "actual",
                                 dataset_mode = "actual_processed",
                                 input_rows = NA_integer_,
                                 nodes = NA_integer_,
                                 edges = NA_integer_,
                                 active_groups = NA_integer_,
                                 cold_or_warm = "warm",
                                 cache_state = "n/a",
                                 result_for_checksum = NULL) {
  env <- meta$env %||% list()
  cfg <- meta$cfg %||% list()
  chk_src <- result_for_checksum %||% timed$result
  data.table::data.table(
    benchmark_id = cfg$benchmark_id %||% NA_character_,
    phase = cfg$phase_label %||% "baseline",
    module = as.character(module),
    operation = as.character(operation),
    dataset_tier = as.character(dataset_tier),
    dataset_mode = as.character(dataset_mode),
    input_rows = as.integer(input_rows),
    nodes = as.integer(nodes),
    edges = as.integer(edges),
    active_groups = as.integer(active_groups),
    cold_or_warm = as.character(cold_or_warm),
    cache_state = as.character(cache_state),
    iterations = as.integer(timed$iterations %||% 0L),
    warmup_iterations = as.integer(timed$warmup_iterations %||% 0L),
    minimum_ms = round(as.numeric(timed$minimum_ms), 3),
    median_ms = round(as.numeric(timed$median_ms), 3),
    mean_ms = round(as.numeric(timed$mean_ms), 3),
    p95_ms = round(as.numeric(timed$p95_ms), 3),
    maximum_ms = round(as.numeric(timed$maximum_ms), 3),
    memory_bytes = as.numeric(timed$memory_bytes %||% NA_real_),
    result_checksum = result_checksum(chk_src),
    benchmark_status = timed$benchmark_status %||% "ok",
    warning = timed$warning %||% NA_character_,
    generated_at = utc_now(),
    R_version = env$R_version %||% NA_character_,
    package_versions = jsonlite::toJSON(env$package_versions %||% list(), auto_unbox = TRUE),
    universe_version = cfg$universe_version %||% EXPECTED_UNIVERSE_CHECKSUM,
    fixture_version = cfg$fixture_version %||% PERF_FIXTURE_VERSION,
    Git_HEAD = env$git_head %||% NA_character_,
    rendering_measured = FALSE,
    browser_automation = FALSE
  )
}
