root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R"); source("R/validation.R")
source("R/reference_data.R"); source("R/calculations.R")

cfg <- load_config()
setwd(cfg$project_root)
ensure_data_dirs(cfg)

comtrade_path <- file.path(cfg[['paths']]$interim, "comtrade_pilot_raw_rows.parquet")
wdi_path <- file.path(cfg[['paths']]$interim, "wdi_pilot_raw_long.parquet")

if (!file.exists(comtrade_path)) {
  stop("Missing interim Comtrade data. Run 02_fetch_comtrade_pilot.R first.", call. = FALSE)
}

log_msg("Cleaning trade data")
raw_trade <- data.table::as.data.table(arrow::read_parquet(comtrade_path))
countries <- build_country_reference()
flows <- build_flow_reference()
hs <- build_hs85_reference()
cleaned <- clean_trade_data(raw_trade, cfg, countries, flows, hs)

arrow::write_parquet(
  cleaned$trade,
  file.path(cfg[['paths']]$interim, "trade_pilot_cleaned.parquet")
)
arrow::write_parquet(
  cleaned$unmatched_reporters,
  file.path(cfg[['paths']]$interim, "unmatched_reporters.parquet")
)
arrow::write_parquet(
  cleaned$unmatched_partners,
  file.path(cfg[['paths']]$interim, "unmatched_partners.parquet")
)
arrow::write_parquet(
  cleaned$unmatched_hs,
  file.path(cfg[['paths']]$interim, "unmatched_hs.parquet")
)

if (file.exists(wdi_path)) {
  log_msg("Cleaning WDI data")
  raw_wdi <- data.table::as.data.table(arrow::read_parquet(wdi_path))
  wdi_clean <- clean_wdi_data(raw_wdi, cfg, countries)
  arrow::write_parquet(
    wdi_clean$long,
    file.path(cfg[['paths']]$interim, "wdi_pilot_cleaned_long.parquet")
  )
  arrow::write_parquet(
    wdi_clean$wide,
    file.path(cfg[['paths']]$interim, "wdi_pilot_cleaned_wide.parquet")
  )
  log_msg(sprintf("WDI cleaned long rows=%d", nrow(wdi_clean$long)))
}

log_msg(sprintf(
  "Trade cleaned rows=%d unmatched_reporters=%d unmatched_partners=%d",
  nrow(cleaned$trade), nrow(cleaned$unmatched_reporters),
  nrow(cleaned$unmatched_partners)
))
cat("STAGE_OK 04_clean_trade_data\n")
