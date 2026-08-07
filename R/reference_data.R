build_country_reference <- function() {
  dt <- data.table::data.table(
    comtrade_code = c("842", "156", "276", "699", "826"),
    comtrade_name = c(
      "USA", "China", "Germany", "India", "United Kingdom"
    ),
    iso3 = c("USA", "CHN", "DEU", "IND", "GBR"),
    display_name = c(
      "United States", "China", "Germany", "India", "United Kingdom"
    ),
    wdi_code = c("USA", "CHN", "DEU", "IND", "GBR"),
    is_pilot_reporter = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    is_pilot_partner = c(TRUE, TRUE, TRUE, TRUE, TRUE)
  )
  data.table::setkey(dt, comtrade_code)
  dt
}

build_flow_reference <- function() {
  dt <- data.table::data.table(
    flow_code = c("M", "X", "XM", "RX", "RM"),
    flow_name = c("Import", "Export", "Re-export", "Re-import", "Re-import"),
    flow_name_alt = c("Imports", "Exports", "Re-exports", "Re-imports", "Re-imports")
  )
  data.table::setkey(dt, flow_code)
  dt
}

build_frequency_reference <- function() {
  dt <- data.table::data.table(
    frequency_code = c("A", "M"),
    frequency_name = c("Annual", "Monthly")
  )
  data.table::setkey(dt, frequency_code)
  dt
}

build_hs85_reference <- function() {
  dt <- data.table::data.table(
    hs_code = "85",
    hs_level = 2L,
    hs_revision = "HS",
    commodity_description = "Electrical machinery and equipment and parts thereof; sound recorders and reproducers; television image and sound recorders and reproducers; parts and accessories of such articles",
    chapter = "85"
  )
  data.table::setkey(dt, hs_code)
  dt
}

write_reference_tables <- function(cfg = load_config()) {
  ensure_data_dirs(cfg)
  countries <- build_country_reference()
  flows <- build_flow_reference()
  freq <- build_frequency_reference()
  hs <- build_hs85_reference()

  meta <- list(
    generated_at = utc_now(),
    source = "project curated pilot reference tables",
    note = "Comtrade numeric codes aligned to ISO-3 for pilot countries"
  )

  arrow::write_parquet(countries, file.path(cfg[['paths']]$reference, "country_reference.parquet"))
  arrow::write_parquet(flows, file.path(cfg[['paths']]$reference, "flow_reference.parquet"))
  arrow::write_parquet(freq, file.path(cfg[['paths']]$reference, "frequency_reference.parquet"))
  arrow::write_parquet(hs, file.path(cfg[['paths']]$reference, "hs85_reference.parquet"))
  write_json_atomic(meta, file.path(cfg[['paths']]$reference, "reference_metadata.json"))

  arrow::write_parquet(
    countries,
    file.path(cfg[['paths']]$processed, "country_reference.parquet")
  )

  list(
    countries = countries,
    flows = flows,
    frequency = freq,
    hs = hs,
    metadata = meta
  )
}

read_country_reference <- function(cfg = load_config()) {
  path <- file.path(cfg[['paths']]$processed, "country_reference.parquet")
  if (!file.exists(path)) {
    path <- file.path(cfg[['paths']]$reference, "country_reference.parquet")
  }
  if (!file.exists(path)) return(build_country_reference())
  data.table::as.data.table(arrow::read_parquet(path))
}
