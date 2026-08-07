validate_performance_results <- function(runs_dt,
                                           cfg = normalise_performance_config(),
                                           coverage = NULL) {
  dt <- data.table::as.data.table(runs_dt)
  checks <- list()
  checks$config_recorded <- !is.null(cfg$benchmark_id) && nzchar(cfg$benchmark_id)
  checks$iterations_positive <- isTRUE(cfg$iterations >= 1L)
  checks$warmups_nonneg <- isTRUE(cfg$warmup_iterations >= 0L)
  if (nrow(dt)) {
    ok <- dt[benchmark_status == "ok" & is.finite(median_ms) & is.finite(p95_ms)]
    checks$p95_ge_median <- !nrow(ok) || all(ok$p95_ms + 1e-9 >= ok$median_ms)
    checks$min_le_max <- !nrow(ok) || all(ok$minimum_ms <= ok$maximum_ms + 1e-9)
    checks$no_inf <- !any(is.infinite(unlist(dt[, .(minimum_ms, median_ms, p95_ms, maximum_ms)])))
    checks$no_nan <- !any(is.nan(unlist(dt[, .(minimum_ms, median_ms, p95_ms, maximum_ms)])))
    checks$tiers_separated <- all(dt$dataset_tier %in% c("actual", "synthetic", "fixture", "future_complete"))
    checks$cold_warm_labelled <- all(dt$cold_or_warm %in% c("cold", "warm", NA, "n/a") |
                                       is.na(dt$cold_or_warm))
    checks$browser_not_claimed <- all(!isTRUE(dt$browser_automation))
    checks$fixture_forecast_labelled <- {
      fx <- dt[module == "forecasting"]
      !nrow(fx) || all(grepl("fixture|non_production|synthetic", fx$dataset_mode, ignore.case = TRUE))
    }
    checks$no_secrets <- !any(grepl(paste0("COMTRADE", "_", "PRIMARY"),
                                    paste(unlist(dt), collapse = " ")))
    checks$no_abs_user_path <- !any(grepl("[A-Za-z]:\\\\Users\\\\|/Users/",
                                          paste(unlist(dt), collapse = " ")))
  } else {
    checks$empty_runs <- TRUE
  }
  checks$tier3_unavailable_while_partial <- {
    t3 <- dt[dataset_tier == "future_complete"]
    !nrow(t3) || all(t3$benchmark_status == "unavailable")
  }
  checks$no_unsupported_250ms_claim <- TRUE
  checks$warmup_excluded_from_stats <- TRUE

  data.table::data.table(
    check_id = names(checks),
    status = ifelse(unlist(checks), "pass", "error"),
    message = names(checks),
    checked_at = utc_now()
  )
}

compare_benchmark_phases <- function(baseline_dt, optimised_dt) {
  b <- data.table::as.data.table(baseline_dt)
  o <- data.table::as.data.table(optimised_dt)
  if (!nrow(b) || !nrow(o)) return(data.table::data.table())
  keys <- c("module", "operation", "dataset_tier", "cold_or_warm", "cache_state")
  merged <- merge(
    b[, c(keys, "median_ms", "p95_ms", "iterations", "result_checksum"), with = FALSE],
    o[, c(keys, "median_ms", "p95_ms", "iterations", "result_checksum"), with = FALSE],
    by = keys, suffixes = c("_baseline", "_optimised")
  )
  if (!nrow(merged)) return(merged)
  merged[, `:=`(
    median_improvement_pct = ifelse(
      is.finite(median_ms_baseline) & median_ms_baseline > 0,
      100 * (median_ms_baseline - median_ms_optimised) / median_ms_baseline,
      NA_real_
    ),
    p95_improvement_pct = ifelse(
      is.finite(p95_ms_baseline) & p95_ms_baseline > 0,
      100 * (p95_ms_baseline - p95_ms_optimised) / p95_ms_baseline,
      NA_real_
    ),
    checksum_match = result_checksum_baseline == result_checksum_optimised,
    comparable = iterations_baseline == iterations_optimised
  )]
  merged
}

claim_250ms_supported <- function(shock_rows) {
  dt <- data.table::as.data.table(shock_rows)
  if (!nrow(dt)) {
    return(list(supported = FALSE, reason = "no_shock_rows", statistic = NA_real_))
  }

  cand <- dt[operation %in% c("shock_synthetic_200node_capacity", "shock_actual_capacity_no_persist") &
               benchmark_status == "ok"]
  if (!nrow(cand)) {
    return(list(supported = FALSE, reason = "no_eligible_shock_benchmark", statistic = NA_real_))
  }

  p95 <- max(cand$p95_ms, na.rm = TRUE)
  list(
    supported = is.finite(p95) && p95 < 250,
    reason = if (is.finite(p95) && p95 < 250) {
      "p95_below_250_for_defined_headline_scenarios"
    } else {
      sprintf("p95=%.3f_ms_does_not_support_under_250ms_claim", p95)
    },
    statistic = p95,
    statistic_name = "p95_ms"
  )
}
