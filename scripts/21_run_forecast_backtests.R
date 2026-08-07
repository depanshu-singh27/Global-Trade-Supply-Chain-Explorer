options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)

cfg <- load_config()
monthly_long <- safe_read_parquet_dt(file.path(cfg$paths$processed, "monthly_trade_long.parquet"))
selected <- safe_read_parquet_dt(file.path(cfg$paths$processed, "forecast_selected_series.parquet"))
if (is.null(monthly_long) || !nrow(monthly_long) || is.null(selected) || !nrow(selected)) {
  cat("Missing monthly series or selected stable series. Run script 20 first (or fixtures).\n")
  quit(status = 1)
}

include_prophet <- identical(tolower(Sys.getenv("GTSC_FORECAST_INCLUDE_PROPHET", "")), "true")
models <- c("seasonal_naive", "naive", "drift", "ets", "arima")
if (isTRUE(include_prophet) && isTRUE(prophet_availability()$available)) {
  models <- c(models, "prophet")
}
backtests <- run_forecast_backtests(
  monthly_long, selected, models = models,
  min_train = 24L, horizons = c(1L, 3L, 6L), step = 3L
)

if (!isTRUE(include_prophet) && isTRUE(prophet_availability()$available) && nrow(selected)) {
  sid0 <- selected$series_id[1]
  smoke <- run_series_backtest(
    monthly_long[series_id == sid0],
    models = "prophet",
    min_train = 36L, horizons = 1L, step = 24L
  )
  if (nrow(smoke)) {
    backtests <- data.table::rbindlist(list(backtests, smoke), fill = TRUE)
  }
  cat("PROPHET_SMOKE_OK rows=", nrow(smoke), "\n", sep = "")
}
train_map <- lapply(unique(selected$series_id), function(sid) {
  s <- monthly_long[series_id == sid]
  data.table::setorderv(s, "date")
  s$trade_value_usd
})
names(train_map) <- unique(selected$series_id)
metrics <- summarise_forecast_metrics(backtests, training_by_series = train_map)
selected_models <- select_forecast_models(metrics)

atomic_write_parquet_dt(backtests, file.path(cfg$paths$processed, "forecast_backtest_predictions.parquet"))
atomic_write_parquet_dt(metrics, file.path(cfg$paths$processed, "forecast_model_metrics.parquet"))
atomic_write_parquet_dt(selected_models, file.path(cfg$paths$processed, "forecast_selected_models.parquet"))
cat(
  "BACKTEST_OK predictions=", nrow(backtests),
  " metric_rows=", nrow(metrics),
  " selected_models=", sum(!is.na(selected_models$selected_model_id)), "\n",
  sep = ""
)
