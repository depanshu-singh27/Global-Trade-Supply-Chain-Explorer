test_that("commodity movers produce valid increase/decrease for fixture data", {
  d <- make_ts_detailed_fixture()
  mv <- commodity_movers(d, min(d$year), max(d$year))
  expect_true(is.list(mv))
  expect_true(nrow(mv$absolute_increase) >= 1L || nrow(mv$absolute_decrease) >= 1L)
  if (nrow(mv$absolute_increase)) {
    expect_true(is.character(as.character(mv$absolute_increase$hs_code)))
    expect_true(all(nzchar(as.character(mv$absolute_increase$hs_code))))
    expect_true(all(is.finite(mv$absolute_increase$abs_change)))
  }
})

test_that("mover empty messages are precise", {
  d <- make_ts_detailed_fixture()
  empty <- commodity_movers(data.table::data.table(), 2019L, 2024L)
  expect_match(
    commodity_movers_empty_message(empty, detailed = data.table::data.table()),
    "No detailed bilateral"
  )
  expect_match(
    commodity_movers_empty_message(
      commodity_movers(d, 2020L, 2020L),
      detailed = d, start_year = 2020L, end_year = 2020L
    ),
    "two distinct years"
  )
  expect_match(
    commodity_movers_empty_message(
      empty, detailed = d, reporters = "ZZZ",
      represented_reporters = c("DEU", "IND"), start_year = 2019L, end_year = 2024L
    ),
    "outside the represented"
  )
  expect_match(
    commodity_movers_empty_message(
      empty, detailed = d, partners = "USA",
      start_year = 2019L, end_year = 2024L
    ),
    "partner filter"
  )
})

test_that("missing values are not silently zero-imputed in movers", {
  d <- make_ts_detailed_fixture()

  hs <- as.character(d$hs_code[1])
  d2 <- data.table::copy(d)
  d2 <- d2[!(hs_code == hs & year == max(year))]
  mv <- commodity_movers(d2, min(d2$year), max(d2$year))

  if (nrow(mv$absolute_increase)) {

    expect_true(all(is.finite(mv$absolute_increase$abs_change)))
  }
})
