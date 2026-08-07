test_that("monthly request plan is deterministic and credential-free", {
  bundle <- make_forecast_fixture_bundle(5L)
  p1 <- plan_monthly_forecast_requests(bundle$candidates, strategy = "full_period")
  p2 <- plan_monthly_forecast_requests(bundle$candidates, strategy = "full_period")
  expect_equal(p1$request_id, p2$request_id)
  expect_true(all(p1$frequency == "M"))
  expect_false(any(grepl("key|secret|token", names(p1), ignore.case = TRUE)))
  sm <- summarise_monthly_plan(p1)
  expect_false(isTRUE(sm$contains_credentials))
  expect_equal(sm$n_series, data.table::uniqueN(p1$series_id))
})

test_that("quota classifier and monthly parser", {
  cls <- classify_http_failure(403L, body_text = "call volume quota exceeded")
  expect_equal(cls$status, "quota_blocked")
  body <- jsonlite::toJSON(list(data = list(list(
    period = "202301", reporterISO = "DEU", partnerISO = "CHN",
    flowCode = "M", cmdCode = "8542", primaryValue = 1000,
    reporterCode = "276", partnerCode = "156", classificationCode = "HS"
  ))), auto_unbox = TRUE)
  parsed <- parse_monthly_comtrade_json(body)
  expect_true(isTRUE(parsed$ok))
  expect_equal(nrow(parsed$rows), 1L)
  expect_equal(parsed$rows$trade_value_usd, 1000)
})
