test_that("secret and absolute-path scanners detect unsafe content", {
  hits <- scan_text_for_secrets("COMTRADE_PRIMARY=abc123456789012345", "t")
  expect_true(any(vapply(hits, function(h) h$check == "comtrade_env", logical(1))))
  hits2 <- scan_text_for_secrets("path C:\\\\Users\\\\alice\\\\secret", "t")
  expect_true(any(vapply(hits2, function(h) h$check == "absolute_path", logical(1))))
  hits3 <- scan_text_for_secrets("https://api.example.com?api_key=abcdef1234567890", "t")
  expect_true(any(vapply(hits3, function(h) h$check == "authenticated_url", logical(1))))
})

test_that("bundle secret scan rejects .Renviron", {
  d <- file.path(tempdir(), paste0("sec-", as.integer(Sys.time())))
  dir.create(d)
  writeLines("ok", file.path(d, "x.json"))
  writeLines("COMTRADE_PRIMARY=no", file.path(d, ".Renviron"))
  res <- scan_bundle_for_secrets(d)
  expect_false(res$ok)
})

test_that("health payload contains no secrets and check is side-effect free", {
  payload <- health_payload(normalise_runtime_config(list(runtime_profile = "demo")), TRUE)
  txt <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  expect_false(grepl("COMTRADE", txt, ignore.case = TRUE))
  expect_true(health_check_is_side_effect_free())
})
