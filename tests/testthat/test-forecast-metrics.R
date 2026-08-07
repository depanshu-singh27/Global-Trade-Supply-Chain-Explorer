test_that("forecast metrics helpers alias file coverage", {
  expect_true(is.function(forecast_mae))
  expect_true(is.function(forecast_mase))
  expect_true(is.function(forecast_smape))
})
