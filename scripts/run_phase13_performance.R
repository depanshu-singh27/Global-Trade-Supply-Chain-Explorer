options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")

run_script <- function(path) {
  cat("=== ", basename(path), " ===\n", sep = "")
  rc <- system2(file.path(R.home("bin"), "Rscript"), args = shQuote(path), stdout = "", stderr = "")
  if (!identical(as.integer(rc), 0L)) stop("Failed: ", path, " status=", rc, call. = FALSE)
}

if (!nzchar(Sys.getenv("GTSC_PERF_MODE", ""))) Sys.setenv(GTSC_PERF_MODE = "smoke")
if (!nzchar(Sys.getenv("GTSC_PERF_ITERATIONS", ""))) Sys.setenv(GTSC_PERF_ITERATIONS = "3")
if (!nzchar(Sys.getenv("GTSC_PERF_WARMUP", ""))) Sys.setenv(GTSC_PERF_WARMUP = "1")
if (!nzchar(Sys.getenv("GTSC_PERF_PHASE", ""))) Sys.setenv(GTSC_PERF_PHASE = "baseline")

run_script(file.path(root, "scripts/23_generate_performance_fixtures.R"))
run_script(file.path(root, "scripts/24_profile_data_snapshot.R"))
run_script(file.path(root, "scripts/25_benchmark_analytics.R"))
run_script(file.path(root, "scripts/26_benchmark_shock_engine.R"))
run_script(file.path(root, "scripts/27_benchmark_shiny_server.R"))
run_script(file.path(root, "scripts/28_validate_performance.R"))
cat("PHASE13_PERF_PIPELINE_OK phase=", Sys.getenv("GTSC_PERF_PHASE"), "\n", sep = "")
