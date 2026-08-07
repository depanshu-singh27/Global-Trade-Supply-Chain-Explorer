AGGREGATE_DENYLIST_ISO3 <- c("EUR", "WLD", "W00", "ASE", "OSS", "CSS", "PSS",
                              "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "EUU",
                              "EMU", "OED", "HIC", "UMC", "LMC", "LIC", "LMY",
                              "MIC", "INX", "ARB", "CEB", "EAS", "ECS", "LCN",
                              "MEA", "NAC", "SAS", "SSF")

build_macro_country_universe <- function(cfg = load_config(),
                                           trade_global = NULL,
                                           trade_detailed = NULL,
                                           top_reporters = NULL,
                                           top_partners = NULL,
                                           eligible_reporters = NULL) {
  processed <- cfg[['paths']]$processed
  reference <- cfg[['paths']]$reference

  if (is.null(trade_global)) {
    p <- file.path(processed, "trade_global_hs85_annual.parquet")
    if (!file.exists(p)) stop("Missing trade_global_hs85_annual.parquet", call. = FALSE)
    trade_global <- data.table::as.data.table(arrow::read_parquet(p))
  } else {
    trade_global <- data.table::as.data.table(trade_global)
  }
  if (is.null(trade_detailed)) {
    p <- file.path(processed, "trade_detailed_top20.parquet")
    trade_detailed <- if (file.exists(p)) {
      data.table::as.data.table(arrow::read_parquet(p))
    } else {
      data.table::data.table()
    }
  } else {
    trade_detailed <- data.table::as.data.table(trade_detailed)
  }
  if (is.null(top_reporters)) {
    p <- file.path(processed, "top_reporters.parquet")
    top_reporters <- if (file.exists(p)) {
      data.table::as.data.table(arrow::read_parquet(p))
    } else {
      data.table::data.table()
    }
  } else {
    top_reporters <- data.table::as.data.table(top_reporters)
  }
  if (is.null(top_partners)) {
    p <- file.path(processed, "top_partners.parquet")
    top_partners <- if (file.exists(p)) {
      data.table::as.data.table(arrow::read_parquet(p))
    } else {
      data.table::data.table()
    }
  } else {
    top_partners <- data.table::as.data.table(top_partners)
  }
  if (is.null(eligible_reporters)) {
    p <- file.path(reference, "comtrade_reporters_eligible.parquet")
    raw_p <- file.path(reference, "comtrade_reporters_raw.parquet")
    if (file.exists(p)) {
      eligible_reporters <- data.table::as.data.table(arrow::read_parquet(p))
    } else if (file.exists(raw_p) && exists("filter_eligible_reporters", mode = "function")) {
      raw <- data.table::as.data.table(arrow::read_parquet(raw_p))
      eligible_reporters <- filter_eligible_reporters(raw, years = 2019:2024)$eligible
    } else {
      eligible_reporters <- data.table::data.table()
    }
  } else {
    eligible_reporters <- data.table::as.data.table(eligible_reporters)
  }

  global_iso <- unique(as.character(trade_global$reporter_iso3))
  top_rep_iso <- unique(as.character(top_reporters$reporter_iso3))
  top_par_iso <- unique(as.character(top_partners$partner_iso3))
  det_rep_iso <- if (nrow(trade_detailed)) unique(as.character(trade_detailed$reporter_iso3)) else character()
  det_par_iso <- if (nrow(trade_detailed) && "partner_iso3" %in% names(trade_detailed)) {
    unique(as.character(trade_detailed$partner_iso3))
  } else {
    character()
  }

  all_iso <- sort(unique(c(global_iso, top_rep_iso, top_par_iso, det_rep_iso, det_par_iso)))
  all_iso <- all_iso[!is.na(all_iso) & nzchar(all_iso)]

  name_map <- data.table::data.table(iso3 = character(), country_name = character())
  if (nrow(trade_global)) {
    name_map <- rbind(
      name_map,
      unique(trade_global[, .(iso3 = reporter_iso3, country_name = reporter_name)]),
      fill = TRUE
    )
  }
  if (nrow(top_reporters)) {
    name_map <- rbind(
      name_map,
      unique(top_reporters[, .(iso3 = reporter_iso3, country_name = reporter_name)]),
      fill = TRUE
    )
  }
  if (nrow(top_partners)) {
    name_map <- rbind(
      name_map,
      unique(top_partners[, .(iso3 = partner_iso3, country_name = partner_name)]),
      fill = TRUE
    )
  }
  if (nrow(eligible_reporters) && all(c("iso3", "reporter_name") %in% names(eligible_reporters))) {
    name_map <- rbind(
      name_map,
      unique(eligible_reporters[, .(iso3, country_name = reporter_name)]),
      fill = TRUE
    )
  }
  name_map <- name_map[!is.na(iso3) & nzchar(iso3)]
  name_map <- unique(name_map, by = "iso3")

  entity_map <- data.table::data.table(iso3 = character(), entity_type = character())
  if (nrow(eligible_reporters) && "iso3" %in% names(eligible_reporters)) {
    if ("reporter_entity_type" %in% names(eligible_reporters)) {
      entity_map <- unique(eligible_reporters[, .(
        iso3, entity_type = reporter_entity_type
      )])
    } else {
      entity_map <- unique(eligible_reporters[, .(iso3, entity_type = "country_or_economy")])
    }
  }

  raw_p <- file.path(reference, "comtrade_reporters_raw.parquet")
  if (file.exists(raw_p) && exists("classify_reporter_entity", mode = "function")) {
    raw <- data.table::as.data.table(arrow::read_parquet(raw_p))
    if (!"is_aggregate_flag" %in% names(raw)) raw[, is_aggregate_flag := FALSE]
    if (!"entity_type_raw" %in% names(raw)) raw[, entity_type_raw := NA_character_]
    raw[, entity_type := classify_reporter_entity(
      reporter_code, iso3, is_group, is_aggregate_flag, entity_type_raw
    )]
    entity_map <- rbind(entity_map, unique(raw[, .(iso3, entity_type)]), fill = TRUE)
    entity_map <- unique(entity_map, by = "iso3")
  }

  dt <- data.table::data.table(iso3 = all_iso)
  dt <- name_map[dt, on = "iso3"]
  dt <- entity_map[dt, on = "iso3"]
  dt[is.na(entity_type), entity_type := NA_character_]

  dt[is.na(entity_type) & iso3 %in% AGGREGATE_DENYLIST_ISO3, entity_type := "special"]
  dt[is.na(entity_type) & grepl("^[A-Z]{3}$", iso3), entity_type := "country_or_economy"]
  dt[is.na(entity_type), entity_type := "unknown"]

  dt[, world_bank_code := iso3]
  dt[, selected_top_reporter := iso3 %in% top_rep_iso]
  dt[, selected_top_partner := iso3 %in% top_par_iso]
  dt[, represented_global_reporter := iso3 %in% global_iso]
  dt[, represented_detailed_reporter := iso3 %in% det_rep_iso]
  dt[, source_scope := data.table::fifelse(
    selected_top_reporter & selected_top_partner, "top_reporter_and_partner",
    data.table::fifelse(selected_top_reporter, "top_reporter",
      data.table::fifelse(selected_top_partner, "top_partner",
        data.table::fifelse(represented_detailed_reporter, "detailed_reporter",
          data.table::fifelse(represented_global_reporter, "global_reporter", "other"))))
  )]

  dt[, exclusion_reason := NA_character_]
  dt[iso3 %in% AGGREGATE_DENYLIST_ISO3, exclusion_reason := "defensive_aggregate_denylist"]
  dt[is.na(exclusion_reason) & entity_type %in% c("aggregate", "group", "special"),
     exclusion_reason := paste0("entity_type_", entity_type)]
  dt[is.na(exclusion_reason) & entity_type == "unknown",
     exclusion_reason := "unknown_entity_type"]
  dt[is.na(exclusion_reason) & !grepl("^[A-Z]{3}$", iso3),
     exclusion_reason := "invalid_iso3_format"]
  dt[, included := is.na(exclusion_reason) & entity_type == "country_or_economy"]
  dt[!included & is.na(exclusion_reason), exclusion_reason := "not_country_or_economy"]
  dt[, generated_at := utc_now()]

  data.table::setcolorder(dt, c(
    "iso3", "country_name", "world_bank_code", "entity_type", "source_scope",
    "selected_top_reporter", "selected_top_partner",
    "represented_global_reporter", "represented_detailed_reporter",
    "included", "exclusion_reason", "generated_at"
  ))
  dt[order(iso3)]
}

persist_macro_country_universe <- function(universe_dt, cfg = load_config()) {
  ensure_dir(cfg[['paths']]$processed)
  path <- file.path(cfg[['paths']]$processed, "macro_country_universe.parquet")
  if (exists("atomic_write_parquet_dt", mode = "function")) {
    atomic_write_parquet_dt(universe_dt, path)
  } else {
    arrow::write_parquet(universe_dt, path)
  }
  invisible(path)
}
