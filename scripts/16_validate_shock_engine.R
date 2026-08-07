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

fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) {
    cat("PASS ", label, "\n", sep = "")
  } else {
    cat("FAIL ", label, "\n", sep = "")
    fail <<- fail + 1L
  }
}

det <- data.table::data.table(
  year = c(2024L, 2024L, 2024L, 2024L),
  reporter_iso3 = c("DEU", "DEU", "IND", "IND"),
  reporter_name = c("Germany", "Germany", "India", "India"),
  partner_iso3 = c("CHN", "USA", "CHN", "USA"),
  partner_name = c("China", "USA", "China", "USA"),
  flow_code = c("M", "M", "M", "M"),
  flow_name = "Import",
  hs_code = c("8542", "8542", "8542", "8542"),
  commodity_description = "ICs",
  trade_value_usd = c(70, 30, 90, 10),
  reporter_gdp_current_usd = 4e12,
  observation_count = 1L
)

coverage <- list(
  production_status = "partial",
  represented_reporter_count = 2L,
  selected_reporter_count = 20L,
  universe_checksum = EXPECTED_UNIVERSE_CHECKSUM
)

sc <- list(
  scenario_name = "engine_validation_30pct_chn_8542",
  baseline_year_start = 2024L,
  baseline_year_end = 2024L,
  target_supplier_iso3 = "CHN",
  target_hs_codes = "8542",
  shock_size_pct = 30,
  substitution_mode = "capacity_constrained",
  substitution_capacity_pct = 25,
  propagation_mode = "direct_only",
  acknowledge_partial_coverage = TRUE,
  universe_version = EXPECTED_UNIVERSE_CHECKSUM
)

r1 <- run_shock_scenario(det, sc, coverage = coverage)
ok("engine runs", isTRUE(r1$ok))
ok("recon", isTRUE(r1$reconciliation$ok))
d <- sum(r1$edge_impacts$direct_disrupted_value_usd, na.rm = TRUE)
s <- sum(r1$edge_impacts$substitution_allocated_usd, na.rm = TRUE)
u <- sum(r1$edge_impacts$residual_unmet_value_usd, na.rm = TRUE)
ok("identity", abs(d - (s + u)) < 1e-6)
ok("no negatives", !any(r1$edge_impacts$residual_unmet_value_usd < 0, na.rm = TRUE))

r2 <- run_shock_scenario(det, sc, coverage = coverage)
ok("deterministic", identical(r1$result_hash, r2$result_hash))
ok("diagnostics unique keys", anyDuplicated(r1$diagnostics$metric) == 0L)
ok(
  "represented count once",
  sum(r1$diagnostics$metric == "represented_reporter_count") == 1L
)

sc0 <- sc
sc0$shock_size_pct <- 0
r0 <- run_shock_scenario(det, sc0, coverage = coverage)
ok("zero shock", abs(sum(r0$edge_impacts$direct_disrupted_value_usd, na.rm = TRUE)) < 1e-9)

sc100 <- sc
sc100$shock_size_pct <- 100
sc100$substitution_mode <- "none"
r100 <- run_shock_scenario(det, sc100, coverage = coverage)
ok("full shock no subst", {
  abs(sum(r100$edge_impacts$residual_unmet_value_usd, na.rm = TRUE) -
        sum(r100$baseline_targets$baseline_import_value_usd, na.rm = TRUE)) < 1e-6
})

ex_dir <- file.path(root, "data", "scenarios", "examples")
ex <- list.files(ex_dir, pattern = "\\.json$", full.names = TRUE)
ok("examples present", length(ex) > 0)
for (f in ex) {
  scx <- tryCatch(read_shock_scenario_file(f), error = function(e) NULL)
  ok(paste("example loads", basename(f)), !is.null(scx))
}

cat("ENGINE_VALIDATION_FAILURES=", fail, "\n", sep = "")
quit(status = if (fail > 0) 1 else 0)
