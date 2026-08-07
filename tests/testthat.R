library(testthat)
testthat::test_dir(
  "tests/testthat",
  reporter = c("progress", "summary")
)
