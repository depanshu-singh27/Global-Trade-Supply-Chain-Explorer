align_scenario_reporter_impacts <- function(a, b, id_a = "A", id_b = "B") {
  da <- data.table::as.data.table(a)
  db <- data.table::as.data.table(b)
  if (!nrow(da) && !nrow(db)) return(data.table::data.table())
  if (!"reporter_iso3" %in% names(da)) da <- data.table::data.table(reporter_iso3 = character())
  if (!"reporter_iso3" %in% names(db)) db <- data.table::data.table(reporter_iso3 = character())
  keep <- c("reporter_iso3", "residual_unmet_value_usd", "direct_disrupted_value_usd",
            "substitution_allocated_value_usd", "scenario_rank")
  da <- da[, intersect(keep, names(da)), with = FALSE]
  db <- db[, intersect(keep, names(db)), with = FALSE]
  data.table::setnames(da, setdiff(names(da), "reporter_iso3"),
                       paste0(setdiff(names(da), "reporter_iso3"), "_", id_a))
  data.table::setnames(db, setdiff(names(db), "reporter_iso3"),
                       paste0(setdiff(names(db), "reporter_iso3"), "_", id_b))
  merge(da, db, by = "reporter_iso3", all = TRUE)
}

compare_shock_scenarios <- function(result_a, result_b, id_a = "A", id_b = "B") {
  ra <- result_a$reporter_impacts %||% data.table::data.table()
  rb <- result_b$reporter_impacts %||% data.table::data.table()
  aligned <- align_scenario_reporter_impacts(ra, rb, id_a, id_b)
  if (nrow(aligned)) {
    col_a <- paste0("residual_unmet_value_usd_", id_a)
    col_b <- paste0("residual_unmet_value_usd_", id_b)
    aligned[, residual_diff := sanitize_chart_numeric(get(col_b) %||% 0) -
              sanitize_chart_numeric(get(col_a) %||% 0)]
    aligned[is.na(get(col_a)), (col_a) := 0]
    aligned[is.na(get(col_b)), (col_b) := 0]
    aligned[, residual_pct_diff := data.table::fifelse(
      get(col_a) > 0,
      100 * (get(col_b) - get(col_a)) / get(col_a),
      NA_real_
    )]
    rank_a <- paste0("scenario_rank_", id_a)
    rank_b <- paste0("scenario_rank_", id_b)
    if (rank_a %in% names(aligned) && rank_b %in% names(aligned)) {
      aligned[, rank_movement := as.integer(get(rank_a)) - as.integer(get(rank_b))]
    } else {
      aligned[, rank_movement := NA_integer_]
    }
    aligned[, newly_affected := is.na(get(paste0("residual_unmet_value_usd_", id_a))) |
              (get(paste0("residual_unmet_value_usd_", id_a)) <= 0 &
                 get(paste0("residual_unmet_value_usd_", id_b)) > 0)]

    aligned[, newly_affected := get(col_a) <= 0 & get(col_b) > 0]
    aligned[, no_longer_affected := get(col_a) > 0 & get(col_b) <= 0]
  }
  totals <- list(
    direct_disrupted_diff =
      sum(rb$direct_disrupted_value_usd, na.rm = TRUE) -
      sum(ra$direct_disrupted_value_usd, na.rm = TRUE),
    substitution_diff =
      sum(rb$substitution_allocated_value_usd, na.rm = TRUE) -
      sum(ra$substitution_allocated_value_usd, na.rm = TRUE),
    residual_diff =
      sum(rb$residual_unmet_value_usd, na.rm = TRUE) -
      sum(ra$residual_unmet_value_usd, na.rm = TRUE),
    affected_reporter_diff =
      sum(rb$residual_unmet_value_usd > 0, na.rm = TRUE) -
      sum(ra$residual_unmet_value_usd > 0, na.rm = TRUE)
  )
  list(
    aligned_reporters = aligned,
    totals = totals,
    scenario_a = result_a$scenario$scenario_id %||% id_a,
    scenario_b = result_b$scenario$scenario_id %||% id_b
  )
}
