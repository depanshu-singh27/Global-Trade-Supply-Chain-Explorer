root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R"); source("R/validation.R")
source("R/reference_data.R"); source("R/calculations.R"); source("R/data_access.R")

cfg <- load_config()
setwd(cfg$project_root)
ensure_data_dirs(cfg)

trade_path <- file.path(cfg[['paths']]$interim, "trade_pilot_cleaned.parquet")
wdi_long_path <- file.path(cfg[['paths']]$interim, "wdi_pilot_cleaned_long.parquet")
wdi_wide_path <- file.path(cfg[['paths']]$interim, "wdi_pilot_cleaned_wide.parquet")

if (!file.exists(trade_path)) {
  stop("Missing cleaned trade data. Run 04_clean_trade_data.R first.", call. = FALSE)
}

trade <- data.table::as.data.table(arrow::read_parquet(trade_path))
wdi_long <- if (file.exists(wdi_long_path)) {
  data.table::as.data.table(arrow::read_parquet(wdi_long_path))
} else empty_wdi_long()
wdi_wide <- if (file.exists(wdi_wide_path)) {
  data.table::as.data.table(arrow::read_parquet(wdi_wide_path))
} else data.table::data.table()

countries <- build_country_reference()
summary_dt <- build_pilot_country_summary(trade, wdi_wide)

out <- cfg[['paths']]$processed
arrow::write_parquet(trade, file.path(out, "trade_pilot.parquet"))
arrow::write_parquet(wdi_long, file.path(out, "wdi_pilot_long.parquet"))
arrow::write_parquet(wdi_wide, file.path(out, "wdi_pilot_wide.parquet"))
arrow::write_parquet(countries, file.path(out, "country_reference.parquet"))
arrow::write_parquet(summary_dt, file.path(out, "pilot_country_summary.parquet"))

if (requireNamespace("fst", quietly = TRUE) && nrow(summary_dt)) {
  fst::write_fst(summary_dt, file.path(out, "pilot_country_summary.fst"))
}

expected_wdi <- vapply(cfg$wdi$indicators, `[[`, character(1), "code")
unmatched_r <- if (file.exists(file.path(cfg[['paths']]$interim, "unmatched_reporters.parquet"))) {
  data.table::as.data.table(arrow::read_parquet(file.path(cfg[['paths']]$interim, "unmatched_reporters.parquet")))
} else data.table::data.table()
unmatched_p <- if (file.exists(file.path(cfg[['paths']]$interim, "unmatched_partners.parquet"))) {
  data.table::as.data.table(arrow::read_parquet(file.path(cfg[['paths']]$interim, "unmatched_partners.parquet")))
} else data.table::data.table()

results <- bind_validation(
  validate_required_columns(
    trade,
    c("year", "reporter_iso3", "partner_iso3", "flow_code", "hs_code",
      "trade_value_usd"),
    "trade_pilot"
  ),
  validate_year_range(trade$year, cfg$pilot$start_year, cfg$pilot$end_year, "trade_pilot"),
  validate_iso3(trade$reporter_iso3, "trade_pilot", "reporter_iso3"),
  validate_iso3(trade$partner_iso3, "trade_pilot", "partner_iso3"),
  validate_mapping_coverage(trade$hs_code, trade$commodity_description, "trade_pilot", "hs"),
  validate_hs_character(trade$hs_code, "trade_pilot"),
  validate_non_negative(trade$trade_value_usd, "trade_pilot"),
  validate_unique_keys(
    trade,
    c("year", "frequency", "reporter_code", "partner_code", "flow_code", "hs_code"),
    "trade_pilot"
  ),
  validate_mapping_coverage(trade$reporter_code, trade$reporter_iso3, "trade_pilot", "reporter"),
  validate_mapping_coverage(trade$partner_code, trade$partner_iso3, "trade_pilot", "partner"),
  if (nrow(unmatched_r)) {
    .validation_row("unmatched_reporters", "trade_pilot", "warning",
                    "Unmatched reporter codes retained in interim report", nrow(unmatched_r))
  } else {
    .validation_row("unmatched_reporters", "trade_pilot", "pass",
                    "No unmatched reporters", 0L)
  },
  if (nrow(unmatched_p)) {
    .validation_row("unmatched_partners", "trade_pilot", "warning",
                    "Unmatched partner codes retained in interim report", nrow(unmatched_p))
  } else {
    .validation_row("unmatched_partners", "trade_pilot", "pass",
                    "No unmatched partners", 0L)
  },
  if (nrow(wdi_long)) {
    bind_validation(
      validate_required_columns(
        wdi_long,
        c("iso3", "year", "indicator_code", "value"),
        "wdi_pilot_long"
      ),
      validate_iso3(wdi_long$iso3, "wdi_pilot_long", "iso3"),
      validate_year_range(wdi_long$year, cfg$pilot$start_year, cfg$pilot$end_year, "wdi_pilot_long"),
      validate_unique_keys(
        wdi_long, c("iso3", "year", "indicator_code"),
        "wdi_pilot_long", "wdi_unique"
      ),
      validate_wdi_indicators(wdi_long$indicator_code, expected_wdi, "wdi_pilot_long")
    )
  },
  validate_parquet_roundtrip(file.path(out, "trade_pilot.parquet"), "trade_pilot"),
  validate_parquet_roundtrip(file.path(out, "wdi_pilot_long.parquet"), "wdi_pilot_long"),
  validate_parquet_roundtrip(file.path(out, "country_reference.parquet"), "country_reference")
)

arrow::write_parquet(results, file.path(out, "validation_results.parquet"))

manifest <- list(
  pipeline = "pilot",
  completed_at = utc_now(),
  environment = cfg$environment,
  hs_chapter = cfg$pilot$hs_chapter,
  years = seq.int(cfg$pilot$start_year, cfg$pilot$end_year),
  reporters = cfg$pilot$reporters,
  partners = cfg$pilot$partners,
  outputs = list(
    trade_pilot = list(rows = nrow(trade), cols = ncol(trade)),
    wdi_pilot_long = list(rows = nrow(wdi_long), cols = ncol(wdi_long)),
    wdi_pilot_wide = list(rows = nrow(wdi_wide), cols = ncol(wdi_wide)),
    country_reference = list(rows = nrow(countries), cols = ncol(countries)),
    pilot_country_summary = list(rows = nrow(summary_dt), cols = ncol(summary_dt))
  ),
  validation = summarise_validation(results),
  notes = "COMTRADE_PRIMARY presence checked at fetch time; key never written to manifest."
)
write_json_atomic(manifest, file.path(out, "pipeline_manifest.json"))

vs <- summarise_validation(results)
log_msg(sprintf(
  "Processed outputs written. trade_rows=%d validation pass=%d warn=%d error=%d",
  nrow(trade), vs$n_pass, vs$n_warning, vs$n_error
))
cat("STAGE_OK 05_build_processed_data\n")
if (vs$n_error > 0) quit(status = 1)
