rank_with_ties <- function(df, value_col, tie_col_cols, desc = TRUE) {
  data.table::setDT(df)
  for (c in tie_col_cols) {
    df[, (c) := as.character(get(c))]
  }
  ord <- do.call(order, c(list(df[[value_col]], decreasing = desc), lapply(tie_col_cols, function(c) df[[c]])))
  df[ord]
}

compute_universe_checksum <- function(top_reporters,
                                        top_partners,
                                        top_hs4,
                                        ranking_year,
                                        classification = "HS",
                                        methodology = "imports_exports_world_partner_hs85") {
  reps <- sort(unique(as.character(top_reporters$reporter_code)))
  pars <- sort(unique(as.character(top_partners$partner_code)))
  hs <- sort(unique(as.character(top_hs4$hs_code)))
  key <- paste(
    "universe",
    as.character(as.integer(ranking_year)),
    as.character(classification),
    as.character(methodology),
    paste(reps, collapse = ","),
    paste(pars, collapse = ","),
    paste(hs, collapse = ","),
    sep = "|"
  )
  paste0("uv_", sha256_short(key, n = 20L))
}

select_top_reporters_from_global <- function(trade_global_dt,
                                               ranking_year = 2024L,
                                               top_n = 20L,
                                               world_partner_iso3 = "W00",
                                               country_iso3_set = NULL,
                                               excluded_reporter_iso3 = c("EUR", "WLD", "W00", "ASE"),
                                               eligible_reporter_codes = NULL) {
  dt <- data.table::as.data.table(trade_global_dt)
  dt <- dt[year == as.integer(ranking_year)]

  if (!"partner_iso3" %in% names(dt)) {
    stop("trade_global_dt must include partner_iso3 for reporter ranking.", call. = FALSE)
  }
  if (!"trade_value_usd" %in% names(dt)) {
    stop("trade_global_dt must include trade_value_usd for reporter ranking.", call. = FALSE)
  }

  if (all(c("reporter_code", "reporter_iso3") %in% names(dt))) {
    if (exists("classify_reporter_entity", mode = "function")) {
      dt[, reporter_entity_type := classify_reporter_entity(
        reporter_code = reporter_code,
        iso3 = reporter_iso3,
        is_group = reporter_iso3 %in% excluded_reporter_iso3,
        is_aggregate_flag = FALSE,
        entity_type_raw = NA_character_,
        defensive_special_iso3 = excluded_reporter_iso3
      )]

      dt[reporter_iso3 %in% excluded_reporter_iso3,
         reporter_entity_type := data.table::fifelse(
           reporter_iso3 %in% c("W00", "WLD"), "special", "special"
         )]
    } else {
      dt[, is_world := as.character(reporter_code) == "0" | reporter_iso3 == world_partner_iso3]
      if ("reporter_name" %in% names(dt)) {
        dt[, is_unspecified := grepl("unspecified|not specified|nes", reporter_name, ignore.case = TRUE)]
      } else {
        dt[, is_unspecified := FALSE]
      }
      dt[, is_aggregate := reporter_iso3 %in% excluded_reporter_iso3 |
            (!is_world & !is_unspecified & !grepl("^[A-Z]{3}$", reporter_iso3))]
      dt[, is_country := !is_world & !is_unspecified & !is_aggregate]
      dt[, reporter_entity_type := data.table::fifelse(
        is_world, "special",
        data.table::fifelse(is_unspecified, "unknown",
          data.table::fifelse(is_aggregate, "special", "country_or_economy"))
      )]
    }
  } else {
    dt[, reporter_entity_type := NA_character_]
  }

  if ("reporter_iso3" %in% names(dt) && length(excluded_reporter_iso3)) {
    dt <- dt[!(reporter_iso3 %in% excluded_reporter_iso3)]
  }

  if (!is.null(eligible_reporter_codes) && "reporter_code" %in% names(dt)) {
    dt <- dt[as.character(reporter_code) %in% as.character(eligible_reporter_codes)]
  }

  if (!is.null(country_iso3_set) && "reporter_iso3" %in% names(dt)) {
    dt <- dt[reporter_iso3 %in% country_iso3_set]
  }

  if ("reporter_entity_type" %in% names(dt)) {
    dt <- dt[reporter_entity_type == "country_or_economy" | is.na(reporter_entity_type)]
  }

  world_dt <- dt[partner_iso3 == world_partner_iso3]
  if (!nrow(world_dt)) {
    stop("No World partner rows found for ranking_year in trade_global_hs85_annual.", call. = FALSE)
  }

  score <- world_dt[, .(
    score_value_usd = sum(trade_value_usd, na.rm = TRUE),
    imports_value_usd = sum(trade_value_usd[flow_code == "M"], na.rm = TRUE),
    exports_value_usd = sum(trade_value_usd[flow_code == "X"], na.rm = TRUE)
  ), by = .(reporter_code, reporter_iso3, reporter_name, reporter_entity_type)]

  data.table::setDT(score)
  score <- score[order(-score_value_usd, reporter_iso3, reporter_code)]
  top <- score[seq_len(min(top_n, nrow(score)))]
  top[, rank := .I]
  data.table::setcolorder(top, c("rank", setdiff(names(top), "rank")))
  top[]
}

select_top_partners_and_hs4_from_bilateral <- function(bilateral_dt,
                                                        top_reporter_codes,
                                                        ranking_year = 2024L,
                                                        top_partners_n = 20L,
                                                        top_hs4_n = 20L,
                                                        world_partner_iso3 = "W00",
                                                        country_iso3_set = NULL) {
  dt <- data.table::as.data.table(bilateral_dt)
  dt <- dt[year == as.integer(ranking_year)]

  if (!all(c("reporter_code", "partner_code", "partner_iso3", "hs_code", "trade_value_usd", "flow_code") %in% names(dt))) {
    stop("bilateral_dt missing required columns.", call. = FALSE)
  }

  dt <- dt[reporter_code %in% as.character(top_reporter_codes)]
  if (!nrow(dt)) stop("No bilateral rows for selected top reporters.", call. = FALSE)

  dt <- dt[partner_iso3 != world_partner_iso3]

  if (!is.null(country_iso3_set)) {
    dt <- dt[partner_iso3 %in% country_iso3_set]
  }

  if ("hs_level" %in% names(dt)) {
    chapter_dt <- dt[hs_level == 2]
  } else {
    chapter_dt <- dt[nchar(hs_code) == 2 & hs_code == "85"]
  }

  partner_score <- chapter_dt[, .(
    score_value_usd = sum(trade_value_usd, na.rm = TRUE)
  ), by = .(partner_code, partner_iso3, partner_name)]

  partner_score <- partner_score[order(-score_value_usd, partner_iso3, partner_code)]
  top_partners <- partner_score[seq_len(min(top_partners_n, nrow(partner_score)))]
  top_partners[, partner_rank := .I]
  data.table::setcolorder(top_partners, c("partner_rank", setdiff(names(top_partners), "partner_rank")))

  selected_partner_codes <- as.character(top_partners$partner_code)
  dt_top_partners <- dt[partner_code %in% selected_partner_codes]

  hs4_dt <- dt_top_partners
  if ("hs_level" %in% names(hs4_dt)) {
    hs4_dt <- hs4_dt[hs_level == 4]
  }
  hs4_dt <- hs4_dt[nchar(hs_code) == 4 & substr(hs_code, 1, 2) == "85"]

  hs4_score <- hs4_dt[, .(
    score_value_usd = sum(trade_value_usd, na.rm = TRUE)
  ), by = .(hs_code, commodity_description)]

  hs4_score <- hs4_score[order(-score_value_usd, hs_code)]
  top_hs4 <- hs4_score[seq_len(min(top_hs4_n, nrow(hs4_score)))]
  top_hs4[, hs_rank := .I]

  list(
    top_partners = top_partners,
    top_hs4 = top_hs4
  )
}

persist_analytical_universe <- function(universe, cfg = load_config()) {
  ensure_dir(cfg[['paths']]$processed)
  out_path <- file.path(cfg[['paths']]$processed, "analytical_universe.json")
  write_json_atomic(universe, out_path, pretty = TRUE)
  out_path
}
