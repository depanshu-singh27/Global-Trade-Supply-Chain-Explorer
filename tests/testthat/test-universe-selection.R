test_that("top reporter selection uses deterministic tie-breaking", {
  dt <- data.table::data.table(
    year = 2024L,
    partner_iso3 = rep("W00", 4),
    flow_code = c("M", "X", "M", "X"),
    reporter_code = c("1", "1", "2", "2"),
    reporter_iso3 = c("BBB", "BBB", "AAA", "AAA"),
    reporter_name = c("B", "B", "A", "A"),
    trade_value_usd = c(10, 5, 15, 0)
  )

  top <- select_top_reporters_from_global(trade_global_dt = dt, ranking_year = 2024L, top_n = 2L)
  expect_equal(top$reporter_iso3[1], "AAA")
  expect_equal(top$reporter_iso3[2], "BBB")
  expect_equal(top$rank[1], 1L)
  expect_equal(top$rank[2], 2L)
})

test_that("top reporter selection excludes known aggregate reporter ISO3 codes", {
  dt <- data.table::data.table(
    year = 2024L,
    partner_iso3 = rep("W00", 4),
    flow_code = c("M", "X", "M", "X"),
    reporter_code = c("97", "97", "842", "842"),
    reporter_iso3 = c("EUR", "EUR", "USA", "USA"),
    reporter_name = c("European Union", "European Union", "USA", "USA"),
    trade_value_usd = c(1000, 1000, 10, 10)
  )

  top <- select_top_reporters_from_global(
    trade_global_dt = dt,
    ranking_year = 2024L,
    top_n = 2L
  )
  expect_false(any(top$reporter_iso3 %in% c("EUR", "WLD", "W00", "ASE")))
  expect_true(all(top$reporter_entity_type == "country_or_economy"))
  expect_equal(top$reporter_iso3[1], "USA")
})

test_that("partner + HS4 selection excludes World and ranks deterministically", {

  reporters <- c("R1", "R2")
  flows <- c("M", "X")
  partners <- c("P1", "P2", "W00", "P3")
  partner_names <- c(P1 = "P1", P2 = "P2", W00 = "World", P3 = "P3")

  chapter <- data.table::CJ(
    year = 2024L,
    reporter_code = reporters,
    flow_code = flows,
    partner_code = partners
  )
  chapter[, `:=`(
    partner_iso3 = partner_code,
    partner_name = partner_names[partner_code],
    hs_code = "85",
    hs_level = 2L,
    commodity_description = "chapter85"
  )]
  chapter[, trade_value_usd := ifelse(partner_code == "P1", 10,
                                       ifelse(partner_code == "P2", 20,
                                              ifelse(partner_code == "W00", 999, 5)) +
                                       ifelse(reporter_code == "R1", 0, 1))]

  hs4_codes <- c("8501", "8502", "8503")
  hs4 <- data.table::CJ(
    year = 2024L,
    reporter_code = reporters,
    flow_code = flows,
    partner_code = partners,
    hs_code = hs4_codes
  )
  hs4[, `:=`(
    partner_iso3 = partner_code,
    partner_name = partner_names[partner_code],
    hs_level = 4L,
    commodity_description = "hs4"
  )]
  hs4[, trade_value_usd := ifelse(hs_code == "8502", 50,
                                   ifelse(hs_code == "8501", 30, 10)) +
                            ifelse(partner_code == "P1", 0,
                                    ifelse(partner_code == "P2", 5,
                                           ifelse(partner_code == "W00", 500, 1)))]

  bilateral <- data.table::rbindlist(list(chapter, hs4), fill = TRUE)
  country_iso3_set <- c("P1", "P2", "P3")
  top_reporters <- data.table::data.table(reporter_code = c("R1", "R2"))
  sel <- select_top_partners_and_hs4_from_bilateral(
    bilateral_dt = bilateral,
    top_reporter_codes = top_reporters$reporter_code,
    ranking_year = 2024L,
    top_partners_n = 2L,
    top_hs4_n = 2L,
    world_partner_iso3 = "W00",
    country_iso3_set = country_iso3_set
  )

  expect_false(any(sel$top_partners$partner_iso3 == "W00"))
  expect_equal(nrow(sel$top_partners), 2L)
  expect_equal(nrow(sel$top_hs4), 2L)

  expect_true(all(sel$top_partners$partner_rank == c(1L, 2L)))
  expect_true(all(sel$top_hs4$hs_rank == c(1L, 2L)))
})
