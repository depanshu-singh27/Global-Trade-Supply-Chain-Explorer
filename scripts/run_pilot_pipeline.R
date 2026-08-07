root <- getwd()
if (!file.exists(file.path(root, "config.yml"))) {
  stop("Run this script from the project root (config.yml not found).", call. = FALSE)
}
if (file.exists("renv/activate.R")) source("renv/activate.R")

stages <- c(
  "scripts/00_check_environment.R",
  "scripts/01_fetch_reference_data.R",
  "scripts/02_fetch_comtrade_pilot.R",
  "scripts/03_fetch_wdi_pilot.R",
  "scripts/04_clean_trade_data.R",
  "scripts/05_build_processed_data.R"
)

run_stage <- function(path) {
  cat("\n==== Running ", path, " ====\n", sep = "")
  status <- system2("Rscript", shQuote(path), stdout = "", stderr = "")
  if (!identical(as.integer(status), 0L)) {
    stop("Pipeline halted: ", path, " exited with status ", status, call. = FALSE)
  }
  invisible(TRUE)
}

rscript <- file.path(R.home("bin"), "Rscript")
if (.Platform$OS.type == "windows") {
  rscript <- file.path(R.home("bin"), "Rscript.exe")
}

for (st in stages) {
  cat("\n==== Running ", st, " ====\n", sep = "")
  status <- system2(rscript, shQuote(file.path(root, st)))
  if (!identical(as.integer(status), 0L)) {
    cat("PIPELINE_FAIL at ", st, "\n", sep = "")
    quit(status = 1)
  }
}

source("R/utilities.R"); source("R/config.R"); source("R/data_access.R")
source("R/reference_data.R"); source("R/validation.R")
cfg <- load_config()
snap <- load_processed_snapshot(cfg)
vs <- validation_summary(snap)
cat("\n==== Pipeline summary ====\n")
cat("Trade rows: ", if (!is.null(snap$trade)) nrow(snap$trade) else 0, "\n", sep = "")
cat("WDI long rows: ", if (!is.null(snap$wdi_long)) nrow(snap$wdi_long) else 0, "\n", sep = "")
cat("Validation pass/warn/error: ",
    vs$n_pass, "/", vs$n_warning, "/", vs$n_error, "\n", sep = "")
cat("PIPELINE_OK\n")
quit(status = if (vs$n_error > 0) 1 else 0)
