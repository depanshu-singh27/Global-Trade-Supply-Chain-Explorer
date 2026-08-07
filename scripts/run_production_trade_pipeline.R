if (file.exists("renv/activate.R")) source("renv/activate.R")
root <- getwd()
if (!file.exists("config.yml")) stop("Run from project root.", call. = FALSE)

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

run_stage <- function(path) {
  cat("\n==== Running ", path, " ====\n", sep = "")
  status <- system2(rscript, shQuote(file.path(root, path)))
  if (!identical(as.integer(status), 0L)) {
    cat("PIPELINE_FAIL at ", path, "\n", sep = "")
    quit(status = 1L)
  }
}

run_stage("scripts/06_plan_production_ingestion.R")

Sys.setenv(GTSC_GLOBAL_STAGE = Sys.getenv("GTSC_GLOBAL_STAGE", "ranking"))
run_stage("scripts/07_fetch_global_hs85.R")
run_stage("scripts/08_select_analytical_universe.R")

Sys.setenv(GTSC_GLOBAL_STAGE = "full")
run_stage("scripts/07_fetch_global_hs85.R")

run_stage("scripts/09_fetch_detailed_top20.R")
run_stage("scripts/10_build_production_trade_data.R")

cat("\nPIPELINE_OK\n")
quit(status = 0L)
