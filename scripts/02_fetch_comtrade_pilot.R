root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R"); source("R/validation.R")
source("R/reference_data.R"); source("R/comtrade_client.R")

cfg <- load_config()
setwd(cfg$project_root)
ensure_data_dirs(cfg)

if (!comtrade_key_present()) {
  stop("COMTRADE_PRIMARY missing — cannot fetch Comtrade pilot data.", call. = FALSE)
}

log_msg("Fetching Comtrade pilot data")
res <- fetch_comtrade_pilot(cfg)
log_msg(sprintf("Comtrade pilot rows=%d requests_logged=%d", res$n_rows, res$n_requests))
cat("STAGE_OK 02_fetch_comtrade_pilot\n")
