wdi_plan_file <- function(cfg = load_config()) {
  file.path(cfg[['paths']]$interim, "wdi_production_request_plan.parquet")
}

wdi_plan_summary_file <- function(cfg = load_config()) {
  file.path(cfg[['paths']]$interim, "wdi_production_request_plan_summary.json")
}

wdi_state_file <- function(cfg = load_config()) {
  file.path(cfg[['paths']]$interim, "wdi_production_state.parquet")
}

make_wdi_request_id <- function(indicator_code, country_batch_key, start_year, end_year, page) {
  key <- paste(
    "wdi_prod", indicator_code, country_batch_key,
    as.integer(start_year), as.integer(end_year), as.integer(page),
    sep = "|"
  )
  paste0("wdi_", sha256_short(key, n = 16L))
}

chunk_country_codes <- function(codes, chunk_size = 60L) {
  codes <- sort(unique(as.character(codes)))
  codes <- codes[nzchar(codes)]
  if (!length(codes)) return(list())
  split(codes, ceiling(seq_along(codes) / as.integer(chunk_size)))
}

build_wdi_production_plan <- function(cfg,
                                        country_iso3,
                                        indicators = NULL,
                                        start_year = 2019L,
                                        end_year = 2024L,
                                        chunk_size = 60L,
                                        per_page = 1000L) {
  if (is.null(indicators)) indicators <- cfg$wdi$indicators
  codes <- sort(unique(as.character(country_iso3)))
  codes <- codes[nzchar(codes)]
  if (!length(codes)) stop("No country codes for WDI plan.", call. = FALSE)

  chunks <- chunk_country_codes(codes, chunk_size = chunk_size)
  rows <- list()
  for (ind in indicators) {
    ind_code <- ind$code %||% ind[["code"]]
    ind_name <- ind$name %||% ind[["name"]] %||% ind_code
    for (ci in seq_along(chunks)) {
      batch <- chunks[[ci]]
      batch_key <- paste0("c", ci, "_", sha256_short(paste(batch, collapse = "-"), 8L))
      page <- 1L
      rid <- make_wdi_request_id(ind_code, batch_key, start_year, end_year, page)
      rows[[length(rows) + 1L]] <- data.table::data.table(
        request_id = rid,
        indicator_code = as.character(ind_code),
        indicator_name = as.character(ind_name),
        requested_country_codes = paste(batch, collapse = ";"),
        country_batch_key = batch_key,
        country_count = length(batch),
        start_year = as.integer(start_year),
        end_year = as.integer(end_year),
        page = as.integer(page),
        per_page = as.integer(per_page),
        purpose = "production_wdi_macro",
        plan_status = "active",
        planned_at = utc_now(),
        raw_output_path = file.path(
          cfg[['paths']]$raw, "wdi", "production",
          paste0(rid, ".json")
        )
      )
    }
  }
  data.table::rbindlist(rows, fill = TRUE)
}

summarise_wdi_plan <- function(plan_dt, request_delay_seconds = 0.4) {
  plan_dt <- data.table::as.data.table(plan_dt)
  active <- if ("plan_status" %in% names(plan_dt)) plan_dt[plan_status == "active"] else plan_dt
  list(
    planned_at = utc_now(),
    active_request_count = nrow(active),
    indicators = sort(unique(as.character(active$indicator_code))),
    country_count = {
      codes <- unique(unlist(strsplit(as.character(active$requested_country_codes), ";", fixed = TRUE)))
      length(codes[nzchar(codes)])
    },
    year_range = c(min(active$start_year), max(active$end_year)),
    batching_strategy = "one indicator per request; countries chunked (~60); page=1 planned; extra pages discovered at fetch",
    pagination_strategy = "follow meta$pages; append page>1 requests deterministically",
    configured_delay_seconds = as.numeric(request_delay_seconds),
    estimated_minimum_runtime_seconds = round(nrow(active) * as.numeric(request_delay_seconds), 1),
    note = "No API credentials required for World Bank Indicators API V2"
  )
}

append_wdi_pagination_requests <- function(plan_dt, parent_req, total_pages, cfg) {
  total_pages <- as.integer(total_pages %||% 1L)
  if (is.na(total_pages) || total_pages <= 1L) return(plan_dt)
  plan_dt <- data.table::as.data.table(plan_dt)
  extra <- list()
  for (p in seq.int(2L, total_pages)) {
    rid <- make_wdi_request_id(
      parent_req$indicator_code, parent_req$country_batch_key,
      parent_req$start_year, parent_req$end_year, p
    )
    if (rid %in% plan_dt$request_id) next
    extra[[length(extra) + 1L]] <- data.table::data.table(
      request_id = rid,
      indicator_code = parent_req$indicator_code,
      indicator_name = parent_req$indicator_name,
      requested_country_codes = parent_req$requested_country_codes,
      country_batch_key = parent_req$country_batch_key,
      country_count = parent_req$country_count,
      start_year = as.integer(parent_req$start_year),
      end_year = as.integer(parent_req$end_year),
      page = as.integer(p),
      per_page = as.integer(parent_req$per_page %||% 1000L),
      purpose = "production_wdi_macro_page",
      plan_status = "active",
      planned_at = utc_now(),
      raw_output_path = file.path(
        cfg[['paths']]$raw, "wdi", "production",
        paste0(rid, ".json")
      )
    )
  }
  if (!length(extra)) return(plan_dt)
  data.table::rbindlist(list(plan_dt, data.table::rbindlist(extra, fill = TRUE)), fill = TRUE)
}
