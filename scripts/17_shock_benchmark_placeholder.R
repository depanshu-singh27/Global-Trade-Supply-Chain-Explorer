options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)
for (f in c(
  "shock_formatters.R", "shock_scenario.R", "shock_validation.R",
  "shock_direct.R", "shock_substitution.R", "shock_propagation.R",
  "shock_aggregation.R", "shock_comparison.R", "shock_diagnostics.R",
  "shock_downloads.R"
)) {
  source(file.path(root, "R", f), local = FALSE)
}

cat("SHOCK_BENCHMARK_HARNESS_PLACEHOLDER\n")
cat("Target profile (Phase 13): up to 200 active country-commodity nodes,",
    "sparse dependency edges, representative direct shock,",
    "deterministic impact reranking.\n")
cat("Timing instrumentation remains disabled by default",
    "(run_shock_scenario(..., enable_timing = FALSE)).\n")
cat("No measured latency is claimed in Phase 10.\n")
quit(status = 0)
