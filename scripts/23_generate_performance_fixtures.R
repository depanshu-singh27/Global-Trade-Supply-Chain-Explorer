options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)

cfg <- normalise_performance_config()
out <- write_performance_fixtures(cfg)
cat(
  "PERF_FIXTURES_OK rows=", nrow(out$detailed),
  " nodes≈", out$meta$active_country_commodity_nodes,
  " checksum=", out$meta$fixture_checksum, "\n", sep = ""
)
