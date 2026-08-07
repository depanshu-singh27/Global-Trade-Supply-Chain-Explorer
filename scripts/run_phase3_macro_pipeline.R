if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/utilities.R"); source("R/config.R")

cfg <- load_config(); setwd(cfg$project_root)
cat("=== run_phase3_macro_pipeline ===\n")
cat("NOTE: Does not call Comtrade. World Bank Indicators API only.\n")

run_script <- function(path) {
  cat("\n>> ", path, "\n", sep = "")
  status <- system2(
    command = Sys.which("Rscript"),
    args = shQuote(file.path(cfg$project_root, path)),
    stdout = "", stderr = ""
  )

  if (is.null(status) || (is.numeric(status) && status == 127L) || !nzchar(Sys.which("Rscript"))) {
    rscript <- "C:/Program Files/R/R-4.6.1/bin/Rscript.exe"
    status <- system2(rscript, shQuote(file.path(cfg$project_root, path)), stdout = "", stderr = "")
  }
  if (!identical(as.integer(status), 0L)) {
    stop(sprintf("Script failed: %s (status=%s)", path, status), call. = FALSE)
  }
  invisible(TRUE)
}

rscript <- commandArgs(trailingOnly = FALSE)
rscript <- sub("^--file=", "", rscript[grepl("^--file=", rscript)])

rscript_bin <- "C:/Program Files/R/R-4.6.1/bin/Rscript.exe"
if (!file.exists(rscript_bin)) rscript_bin <- Sys.which("Rscript")

run_one <- function(rel) {
  cat("\n>> ", rel, "\n", sep = "")
  status <- system2(rscript_bin, shQuote(file.path(cfg$project_root, rel)))
  if (!identical(as.integer(status), 0L)) {
    stop(sprintf("Script failed: %s (status=%s)", rel, status), call. = FALSE)
  }
}

run_one("scripts/11_plan_wdi_production.R")
run_one("scripts/12_fetch_wdi_production.R")

dry <- tolower(Sys.getenv("GTSC_WDI_DRY_RUN", "false")) %in% c("1", "true", "yes")
if (dry) {
  cat("DRY_RUN: stopping before build/enrich.\n")
  quit(status = 0)
}

run_one("scripts/13_build_macro_data.R")
run_one("scripts/14_enrich_trade_data.R")
cat("\nPHASE3_PIPELINE_OK\n")
quit(status = 0)
