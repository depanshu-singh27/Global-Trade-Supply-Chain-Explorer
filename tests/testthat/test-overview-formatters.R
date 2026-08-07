test_that("safe currency formatting avoids NA/Inf/scientific display", {
  expect_equal(format_usd_compact(1.24e12), "US$1.2tn")
  expect_equal(format_usd_compact(842.6e9), "US$842.6bn")
  expect_equal(format_usd_compact(NA_real_), "Unavailable")
  expect_equal(format_usd_compact(Inf), "Unavailable")
  expect_equal(format_usd_compact(NaN), "Unavailable")
  expect_false(grepl("e\\+", format_usd_compact(1.5e10), ignore.case = TRUE))
})

test_that("safe percentage and missing-value formatting", {
  expect_equal(format_pct(14.3), "14.3%")
  expect_equal(format_pct(NA_real_), "Unavailable")
  expect_equal(format_pct(Inf), "Unavailable")
  expect_equal(missing_label(), "Unavailable")
  expect_equal(format_yoy_delta(NA_real_), "No prior-year comparison")
  expect_true(grepl("^\\+", format_yoy_delta(3.2)))
})

test_that("sanitize_chart_numeric removes Inf/NaN", {
  x <- sanitize_chart_numeric(c(1, Inf, -Inf, NaN, NA_real_, 5))
  expect_true(all(is.na(x[2:5])))
  expect_equal(x[c(1, 6)], c(1, 5))
})
