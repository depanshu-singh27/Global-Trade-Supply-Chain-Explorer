options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)

cfg <- load_config()
ensure_data_dirs(cfg)
use_fixtures <- identical(tolower(Sys.getenv("GTSC_FORECAST_USE_FIXTURES", "")), "true")

snap <- load_processed_snapshot(cfg)
coverage <- snap$detailed_coverage %||% trade_flow_coverage_status(snap)

if (isTRUE(use_fixtures)) {
  source(file.path(root, "tests/testthat/helper-forecasting.R"), local = FALSE)
  bundle <- make_forecast_fixture_bundle(12L)
  monthly_long <- bundle$monthly_long
  candidates <- bundle$candidates
  monthly_long[, `:=`(
    data_mode = FORECAST_DATA_MODE_FIXTURE,
    data_source = FORECAST_DATA_SOURCE_FIXTURE
  )]
  candidates[, production_status := "fixture"]
  write_forecast_data_provenance(
    resolve_forecast_provenance(
      data_mode = FORECAST_DATA_MODE_FIXTURE,
      data_source = FORECAST_DATA_SOURCE_FIXTURE,
      monthly_long = monthly_long,
      candidates = candidates,
      force_fixture = TRUE
    ),
    cfg = cfg
  )
  cat("Using offline forecasting fixtures (data_mode=fixture_synthetic).\n")
} else {
  plan <- safe_read_parquet_dt(monthly_plan_file(cfg))
  state <- safe_read_parquet_dt(monthly_pipeline_state_file(cfg))
  cand_path <- file.path(cfg$paths$processed, "monthly_trade_candidates.parquet")
  candidates <- safe_read_parquet_dt(cand_path)
  if (is.null(candidates) || !nrow(candidates)) {
    detailed <- snap$trade_detailed_enriched %||% snap$trade_detailed
    candidates <- build_annual_forecast_candidates(detailed, top_n = 30L, coverage = coverage)
    if (nrow(candidates)) {
      atomic_write_parquet_dt(candidates, cand_path)
    }
  }

  raw_rows <- data.table::data.table()
  if (!is.null(state) && nrow(state)) {
    ok <- state[status %in% c("succeeded", "skipped_cached", "empty")]
    rows <- lapply(ok$request_id, function(rid) {
      prow <- plan[request_id == rid][1]
      if (!nrow(prow)) return(NULL)
      path <- file.path(root, prow$raw_output_path)
      if (!file.exists(path)) return(NULL)
      parsed <- parse_monthly_comtrade_json(
        paste(readLines(path, warn = FALSE), collapse = "\n"),
        request_meta = list(request_id = rid, classification = prow$classification)
      )
      if (!isTRUE(parsed$ok) || !nrow(parsed$rows)) return(NULL)
      parsed$rows
    })
    raw_rows <- data.table::rbindlist(Filter(Negate(is.null), rows), fill = TRUE)
  }
  if (!nrow(raw_rows)) {
    cat("No monthly raw rows available; writing empty monthly_trade_long and exiting.\n")
    atomic_write_parquet_dt(data.table::data.table(), file.path(cfg$paths$processed, "monthly_trade_long.parquet"))
    write_forecast_data_provenance(
      resolve_forecast_provenance(
        data_mode = FORECAST_DATA_MODE_LIVE,
        data_source = FORECAST_DATA_SOURCE_LIVE,
        monthly_state = state,
        force_fixture = FALSE
      ),
      cfg = cfg
    )
    quit(status = 0)
  }
  monthly_long <- build_monthly_series_long(
    raw_rows, candidates,
    universe_version = coverage$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM
  )
  monthly_long[, `:=`(
    data_mode = FORECAST_DATA_MODE_LIVE,
    data_source = FORECAST_DATA_SOURCE_LIVE
  )]

  write_forecast_data_provenance(
    resolve_forecast_provenance(
      data_mode = FORECAST_DATA_MODE_LIVE,
      data_source = FORECAST_DATA_SOURCE_LIVE,
      monthly_state = state,
      monthly_long = monthly_long,
      candidates = candidates
    ),
    cfg = cfg
  )
  cat("Using live monthly Comtrade rows (data_mode=live_comtrade).\n")
}

quality <- compute_monthly_series_quality(monthly_long)
selected <- select_stable_forecast_series(quality, candidates = candidates, n = 10L)
atomic_write_parquet_dt(candidates, file.path(cfg$paths$processed, "monthly_trade_candidates.parquet"))
atomic_write_parquet_dt(monthly_long, file.path(cfg$paths$processed, "monthly_trade_long.parquet"))
atomic_write_parquet_dt(quality, file.path(cfg$paths$processed, "monthly_series_quality.parquet"))
atomic_write_parquet_dt(selected, file.path(cfg$paths$processed, "forecast_selected_series.parquet"))
cat(
  "MONTHLY_SERIES_OK rows=", nrow(monthly_long),
  " stable=", nrow(selected),
  " rejected=", sum(quality$stable == FALSE), "\n",
  sep = ""
)
