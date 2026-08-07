test_that("default network mode and year selection", {
  expect_equal(NW_DEFAULT_MODE, "exports")
  det <- make_network_detailed_fixture()
  prep <- prepare_detailed_trade(det)
  ch <- trade_flow_filter_choices(prep)
  expect_equal(ch$default_year, max(ch$years))
})

test_that("year-range and HS4 filtering", {
  det <- make_network_detailed_fixture()
  built <- construct_network_edges(det, mode = "exports", year_min = 2020, year_max = 2020,
                                   hs_codes = "8542", top_n = 50L)
  expect_true(all(built$observations$year == 2020))
  expect_true(all(built$observations$hs_code == "8542"))
})

test_that("aggregate and World partners excluded", {
  det <- make_network_detailed_fixture()
  prep <- prepare_detailed_trade(det)
  expect_false(any(prep$partner_iso3 %in% c("WLD", "EUR")))
  built <- construct_network_edges(det, mode = "exports", top_n = 100L)
  expect_false(any(built$eligible_edges$to_iso3 %in% c("WLD", "EUR", "W00")))
  expect_false(any(built$eligible_edges$from_iso3 %in% c("WLD", "EUR")))
})

test_that("export and import directions remain separate", {
  det <- make_network_detailed_fixture()
  ex <- construct_network_edges(det, mode = "exports", year_min = 2019, year_max = 2021, top_n = 100L)
  im <- construct_network_edges(det, mode = "imports", year_min = 2019, year_max = 2021, top_n = 100L)
  expect_true(nrow(ex$eligible_edges) > 0)
  expect_true(nrow(im$eligible_edges) > 0)

  expect_true(any(ex$eligible_edges$from_iso3 == "DEU" & ex$eligible_edges$to_iso3 == "CHN"))

  expect_true(any(im$eligible_edges$from_iso3 == "USA" & im$eligible_edges$to_iso3 == "DEU"))

  expect_false(identical(
    paste(ex$eligible_edges$from_iso3, ex$eligible_edges$to_iso3),
    paste(im$eligible_edges$from_iso3, im$eligible_edges$to_iso3)
  ))
})

test_that("duplicate-edge aggregation and non-negative edges", {
  det <- make_network_detailed_fixture()
  built <- construct_network_edges(det, mode = "exports", year_min = 2019, year_max = 2019,
                                   hs_codes = "8542", top_n = 100L)
  deu_chn <- built$eligible_edges[from_iso3 == "DEU" & to_iso3 == "CHN"]
  expect_equal(nrow(deu_chn), 1L)
  expect_equal(deu_chn$trade_value_usd, 200)
  expect_true(all(built$eligible_edges$trade_value_usd >= 0))
  expect_false(anyNA(built$eligible_edges$from_iso3))
  expect_false(anyNA(built$eligible_edges$to_iso3))
})

test_that("self-edge exclusion reconciles", {
  det <- make_network_detailed_fixture()
  raw <- build_directed_edges_from_rows(prepare_detailed_trade(det)[flow_code == "X"], "exports")
  agg <- aggregate_network_edges(raw, mode = "exports")
  self <- exclude_self_edges(agg)
  expect_true(self$excluded_count >= 1L)
  expect_true(self$excluded_value > 0)
  expect_false(any(self$edges$from_iso3 == self$edges$to_iso3))
  built <- construct_network_edges(det, mode = "exports", top_n = 100L)
  expect_equal(built$self_excluded_count, self$excluded_count)
  expect_equal(built$self_excluded_value, self$excluded_value)
})

test_that("top-edge selection, tie-break and coverage", {
  edges <- data.table::data.table(
    from_iso3 = c("A", "B", "C", "D"),
    to_iso3 = c("B", "C", "D", "A"),
    from_name = c("A", "B", "C", "D"),
    to_name = c("B", "C", "D", "A"),
    trade_value_usd = c(100, 100, 50, 10),
    observation_count = 1L,
    year_start = 2020L, year_end = 2020L,
    flow_mode = "exports", hs_scope = "all", source_reporter_count = 1L
  )
  sel <- select_top_network_edges(edges, top_n = 2L)
  expect_equal(sel$visible_n, 2L)
  expect_equal(sel$visible$from_iso3, c("A", "B"))
  expect_equal(sel$coverage_pct, 200 / 260 * 100, tolerance = 1e-8)
  z <- select_top_network_edges(edges[0], top_n = 2L)
  expect_true(is.na(z$coverage_pct) || identical(z$coverage_pct, NA_real_))
})

test_that("graph node uniqueness and endpoint validity", {
  det <- make_network_detailed_fixture()
  net <- build_full_trade_network(
    det, mode = "exports", year_min = 2019, year_max = 2021, top_n = 50L,
    represented_reporters = c("DEU", "IND", "KOR"),
    selected_reporters = c("DEU", "IND", "KOR", "ITA")
  )
  expect_equal(anyDuplicated(net$nodes$iso3), 0L)
  expect_true(all(net$edges$from_iso3 %in% net$nodes$iso3))
  expect_true(all(net$edges$to_iso3 %in% net$nodes$iso3))
  expect_true(any(net$nodes$reporting_status == "partner_only"))
  expect_true(any(net$nodes$represented_as_reporter))
})
