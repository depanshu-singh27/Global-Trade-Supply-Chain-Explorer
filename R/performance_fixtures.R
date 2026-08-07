PERF_REPORTER_POOL <- c(
  "DEU", "USA", "CHN", "JPN", "KOR", "IND", "ITA", "FRA", "GBR", "NLD",
  "SGP", "THA", "MYS", "VNM", "TWN", "MEX", "CAN", "BRA", "AUS", "ESP"
)
PERF_PARTNER_POOL <- c(
  "CHN", "USA", "DEU", "JPN", "KOR", "TWN", "MYS", "VNM", "THA", "SGP",
  "IND", "MEX", "CAN", "FRA", "ITA", "NLD", "GBR", "BRA", "AUS", "IDN"
)
PERF_HS4_POOL <- c(
  "8501", "8504", "8507", "8517", "8525", "8528", "8536", "8541", "8542", "8544",
  "8502", "8503", "8518", "8523", "8527", "8532", "8533", "8538", "8543", "8545"
)

generate_performance_detailed_fixture <- function(cfg = normalise_performance_config()) {
  set.seed(as.integer(cfg$synthetic_seed))
  n_rep <- as.integer(cfg$synthetic_reporters)
  n_par <- as.integer(cfg$synthetic_partners)
  n_hs <- as.integer(cfg$synthetic_hs4)
  years <- seq.int(2019L, 2019L + as.integer(cfg$synthetic_years) - 1L)
  reporters <- PERF_REPORTER_POOL[seq_len(min(n_rep, length(PERF_REPORTER_POOL)))]
  partners <- PERF_PARTNER_POOL[seq_len(min(n_par, length(PERF_PARTNER_POOL)))]
  hs <- PERF_HS4_POOL[seq_len(min(n_hs, length(PERF_HS4_POOL)))]
  dens <- as.numeric(cfg$synthetic_edge_density)

  rows <- list()
  for (y in years) {
    for (r in reporters) {
      for (p in partners) {
        if (identical(r, p)) next
        for (h in hs) {
          if (stats::runif(1) > dens) next
          for (fl in c("M", "X")) {
            if (stats::runif(1) > 0.85 && fl == "X") next
            rows[[length(rows) + 1L]] <- data.table::data.table(
              year = y,
              frequency = "A",
              reporter_iso3 = r,
              partner_iso3 = p,
              hs_code = as.character(h),
              flow_code = fl,
              trade_value_usd = abs(stats::rlnorm(1, meanlog = 15, sdlog = 1.2)),
              reporter_name = r,
              partner_name = p,
              commodity_description = paste("Synthetic HS", h),
              reporter_code = NA_character_,
              partner_code = NA_character_,
              flow_name = if (fl == "M") "Import" else "Export",
              hs_revision = "HS",
              dataset_mode = "synthetic_scaled",
              is_synthetic = TRUE,
              fixture_version = cfg$fixture_version,
              universe_version = cfg$universe_version
            )
          }
        }
      }
    }
  }
  dt <- data.table::rbindlist(rows, fill = TRUE)

  target_nodes <- as.integer(cfg$synthetic_active_nodes)
  imports <- dt[flow_code == "M"]
  node_ids <- unique(paste(imports$reporter_iso3, imports$hs_code, sep = "::"))
  if (length(node_ids) < min(target_nodes, 50L)) {

    extra <- list()
    for (r in reporters[1:min(10, length(reporters))]) {
      for (h in hs) {
        for (p in partners) {
          if (identical(r, p)) next
          extra[[length(extra) + 1L]] <- data.table::data.table(
            year = max(years), frequency = "A",
            reporter_iso3 = r, partner_iso3 = p, hs_code = as.character(h),
            flow_code = "M", trade_value_usd = 1e7 + stats::runif(1) * 1e6,
            reporter_name = r, partner_name = p,
            commodity_description = paste("Synthetic HS", h),
            reporter_code = NA_character_, partner_code = NA_character_,
            flow_name = "Import", hs_revision = "HS",
            dataset_mode = "synthetic_scaled", is_synthetic = TRUE,
            fixture_version = cfg$fixture_version,
            universe_version = cfg$universe_version
          )
        }
      }
    }
    dt <- data.table::rbindlist(list(dt, data.table::rbindlist(extra, fill = TRUE)), fill = TRUE)
  }
  data.table::setorderv(dt, c("year", "reporter_iso3", "partner_iso3", "hs_code", "flow_code"))
  attr(dt, "fixture_seed") <- cfg$synthetic_seed
  attr(dt, "fixture_checksum") <- result_checksum(dt)
  dt
}

generate_performance_country_year_fixture <- function(detailed, cfg = normalise_performance_config()) {
  dt <- data.table::as.data.table(detailed)
  if (!nrow(dt)) return(data.table::data.table())
  cy <- dt[, .(
    imports_usd = sum(trade_value_usd[flow_code == "M"], na.rm = TRUE),
    exports_usd = sum(trade_value_usd[flow_code == "X"], na.rm = TRUE)
  ), by = .(year, reporter_iso3)]
  cy[, `:=`(
    total_trade_usd = imports_usd + exports_usd,
    trade_balance_usd = exports_usd - imports_usd,
    reporter_name = reporter_iso3,
    dataset_mode = "synthetic_scaled",
    is_synthetic = TRUE,
    fixture_version = cfg$fixture_version
  )]
  cy
}

write_performance_fixtures <- function(cfg = normalise_performance_config()) {
  paths <- ensure_performance_dirs(cfg)
  detailed <- generate_performance_detailed_fixture(cfg)
  cy <- generate_performance_country_year_fixture(detailed, cfg)

  imports <- detailed[flow_code == "M" & year == max(year)]
  nodes <- unique(paste(imports$reporter_iso3, imports$hs_code, sep = "::"))
  meta <- list(
    fixture_version = cfg$fixture_version,
    seed = cfg$synthetic_seed,
    generated_at = utc_now(),
    dataset_mode = "synthetic_scaled",
    is_synthetic = TRUE,
    n_rows = nrow(detailed),
    n_reporters = data.table::uniqueN(detailed$reporter_iso3),
    n_partners = data.table::uniqueN(detailed$partner_iso3),
    n_hs4 = data.table::uniqueN(detailed$hs_code),
    active_country_commodity_nodes = length(nodes),
    target_active_nodes = cfg$synthetic_active_nodes,
    fixture_checksum = attr(detailed, "fixture_checksum"),
    note = "Synthetic benchmark fixture only. Values do not represent real trade.",
    contains_credentials = FALSE
  )
  atomic_write_parquet_dt(detailed, file.path(paths$fixtures, "synthetic_detailed.parquet"))
  atomic_write_parquet_dt(cy, file.path(paths$fixtures, "synthetic_country_year.parquet"))
  write_json_atomic(meta, file.path(paths$fixtures, "synthetic_fixture_manifest.json"))
  invisible(list(detailed = detailed, country_year = cy, meta = meta, paths = paths))
}

load_performance_fixtures <- function(cfg = normalise_performance_config()) {
  paths <- performance_paths(cfg)
  list(
    detailed = safe_read_parquet_dt(file.path(paths$fixtures, "synthetic_detailed.parquet")),
    country_year = safe_read_parquet_dt(file.path(paths$fixtures, "synthetic_country_year.parquet")),
    meta = safe_read_json(file.path(paths$fixtures, "synthetic_fixture_manifest.json"))
  )
}

tier3_unavailable_notice <- function(coverage = NULL) {
  rep_n <- coverage$represented_reporter_count %||% NA_integer_
  sel_n <- coverage$selected_reporter_count %||% 20L
  sprintf(
    "Not available — detailed production coverage remains %s/%s.",
    as.character(rep_n %||% "6"),
    as.character(sel_n %||% "20")
  )
}
