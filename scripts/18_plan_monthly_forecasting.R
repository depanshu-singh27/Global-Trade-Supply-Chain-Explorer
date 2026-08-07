options(shiny.autoload.r = FALSE)
root <- getwd()
if (file.exists("renv/activate.R")) source("renv/activate.R")
source("R/zzz_bootstrap.R")
source_project_r(root)

cfg <- load_config()
ensure_data_dirs(cfg)
dry <- identical(tolower(Sys.getenv("GTSC_MONTHLY_DRY_RUN", "")), "true")

snap <- load_processed_snapshot(cfg)
detailed <- snap$trade_detailed_enriched %||% snap$trade_detailed
coverage <- snap$detailed_coverage %||% trade_flow_coverage_status(snap)

if (is.null(detailed) || !nrow(detailed)) {
  cat("No detailed annual data; building plan from empty candidate set.\n")
  cand <- data.table::data.table()
} else {
  cand <- build_annual_forecast_candidates(detailed, top_n = 30L, coverage = coverage)
}

if (nrow(cand) && (anyNA(cand$reporter_code) || any(cand$reporter_code == ""))) {

  det <- data.table::as.data.table(detailed)
  if (all(c("reporter_iso3", "reporter_code") %in% names(det))) {
    rc <- unique(det[, .(reporter_iso3, reporter_code)])
    cand[rc, reporter_code := i.reporter_code, on = "reporter_iso3"]
  }
  if (all(c("partner_iso3", "partner_code") %in% names(det))) {
    pc <- unique(det[, .(partner_iso3, partner_code)])
    cand[pc, partner_code := i.partner_code, on = "partner_iso3"]
  }
}

plan <- plan_monthly_forecast_requests(
  cand,
  strategy = "full_period",
  universe_version = coverage$universe_checksum %||% EXPECTED_UNIVERSE_CHECKSUM
)
out <- write_monthly_plan(plan, cfg)
cat("MONTHLY_PLAN_OK requests=", nrow(plan), " series=", data.table::uniqueN(plan$series_id), "\n", sep = "")
cat("summary=", monthly_plan_summary_file(cfg), "\n", sep = "")
if (isTRUE(dry)) cat("DRY_RUN plan only\n")
