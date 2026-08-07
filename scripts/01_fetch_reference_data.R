root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R"); source("R/validation.R")
source("R/reference_data.R")

cfg <- load_config()
setwd(cfg$project_root)
ensure_data_dirs(cfg)

log_msg("Fetching / building reference data")
ref <- write_reference_tables(cfg)
log_msg(sprintf(
  "Reference tables written: countries=%d flows=%d frequency=%d hs=%d",
  nrow(ref$countries), nrow(ref$flows), nrow(ref$frequency), nrow(ref$hs)
))
cat("STAGE_OK 01_fetch_reference_data\n")
