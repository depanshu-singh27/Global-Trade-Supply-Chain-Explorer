root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R"); source("R/validation.R")
source("R/reference_data.R"); source("R/wdi_client.R")

cfg <- load_config()
setwd(cfg$project_root)
ensure_data_dirs(cfg)

log_msg("Fetching World Bank WDI pilot data")
res <- fetch_wdi_pilot(cfg)
log_msg(sprintf("WDI pilot rows=%d", res$n_rows))
cat("STAGE_OK 03_fetch_wdi_pilot\n")
