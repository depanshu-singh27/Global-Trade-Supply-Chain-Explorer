test_that("runtime profile normalisation and defaults", {
  with_temp_env(c(
    GTSC_RUNTIME_PROFILE = "demo",
    GTSC_PUBLIC_MODE = "true",
    GTSC_ALLOW_SCENARIO_WRITES = "false",
    GTSC_READ_ONLY_MODE = "true",
    GTSC_HOST = "0.0.0.0",
    GTSC_PORT = "3838"
  ), {
    cfg <- normalise_runtime_config()
    expect_identical(cfg$runtime_profile, "demo")
    expect_true(cfg$public_mode)
    expect_true(cfg$read_only_mode)
    expect_false(cfg$allow_scenario_writes)
    expect_false(runtime_allows_scenario_persistence(cfg))
    expect_false(isTRUE(cfg$comtrade_key_required))
    expect_identical(cfg$host, "0.0.0.0")
    expect_identical(cfg$port, 3838L)
  })
})

test_that("valid profiles accepted; unknown rejected; no silent demo fallback rule", {
  expect_silent(validate_runtime_config(list(
    runtime_profile = "release", host = "127.0.0.1", port = 3838,
    data_root = "data/processed", scenario_root = "data/scenarios",
    performance_root = "data/performance"
  )))
  expect_silent(validate_runtime_config(list(
    runtime_profile = "external", host = "127.0.0.1", port = 3838,
    data_root = "/opt/gtsc/data", scenario_root = "/opt/gtsc/scenarios",
    performance_root = "/opt/gtsc/performance"
  )))
  expect_error(
    validate_runtime_config(list(
      runtime_profile = "prod", host = "127.0.0.1", port = 3838,
      data_root = "data/processed", scenario_root = "data/scenarios",
      performance_root = "data/performance"
    )),
    "Unknown runtime profile"
  )
})

test_that("safe host/port/root validation and unsafe rejection", {
  expect_error(
    validate_runtime_config(list(
      runtime_profile = "demo", host = "bad host", port = 3838,
      data_root = "data/processed", scenario_root = "data/scenarios",
      performance_root = "data/performance"
    )),
    "Invalid GTSC_HOST"
  )
  expect_error(
    validate_runtime_config(list(
      runtime_profile = "demo", host = "0.0.0.0", port = 99999,
      data_root = "data/processed", scenario_root = "data/scenarios",
      performance_root = "data/performance"
    )),
    "GTSC_PORT"
  )
  expect_error(
    validate_runtime_config(list(
      runtime_profile = "demo", host = "0.0.0.0", port = 3838,
      data_root = "../etc", scenario_root = "data/scenarios",
      performance_root = "data/performance"
    )),
    "Unsafe path"
  )
  expect_error(validate_release_output_root("../../tmp"), "Unsafe")
})

test_that("environment allowlist and COMTRADE not required", {
  al <- release_env_allowlist()
  expect_true("GTSC_RUNTIME_PROFILE" %in% al)
  expect_false("COMTRADE_PRIMARY" %in% al)
  expect_true("COMTRADE_PRIMARY" %in% release_forbidden_env())
  info <- validate_env_allowlist_usage()
  expect_false(isTRUE(info$comtrade_required))
})

test_that("startup log redaction omits COMTRADE_PRIMARY", {
  line <- runtime_log("INFO", "test", list(
    COMTRADE_PRIMARY = "should-not-appear",
    profile = "demo"
  ))
  expect_false(grepl("should-not-appear", line))
  expect_true(grepl("demo", line))
  expect_error(assert_no_comtrade_in_logs("COMTRADE_PRIMARY=abc123secret"), "must not contain")
})
