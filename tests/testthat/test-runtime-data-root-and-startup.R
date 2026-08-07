test_that("relative paths are only resolved against the project root", {
  expect_false(is_absolute_path("data/release"))
  expect_false(is_absolute_path("data/release/demo"))
  expect_false(is_absolute_path("release"))
  expect_true(is_absolute_path("/opt/gtsc/data"))
  expect_true(is_absolute_path("C:/opt/gtsc"))
  expect_true(is_absolute_path("C:\\opt\\gtsc"))
  expect_true(is_absolute_path("\\\\server\\share"))

  expect_identical(resolve_project_path("data/release", "/root"), "/root/data/release")
  expect_identical(resolve_project_path("/abs/data", "/root"), "/abs/data")
})

test_that("release output root resolves independently of the working directory", {
  root <- release_test_root()
  expected <- gsub(
    "\\\\", "/",
    normalizePath(file.path(root, "data", "release"), winslash = "/", mustWork = FALSE)
  )

  sandbox <- withr::local_tempdir()
  withr::with_dir(sandbox, {
    expect_false(dir.exists("data/release"))
    expect_identical(validate_release_output_root("data/release", root), expected)
    expect_identical(release_paths(root)$output, expected)
    expect_identical(release_paths(root)$demo, file.path(expected, "demo"))
  })

  expect_error(validate_release_output_root("../../tmp", root), "Unsafe")
  expect_error(validate_release_output_root("", root), "empty")
  expect_error(
    validate_release_output_root(file.path(sandbox, "release"), root),
    "Rejected unsafe release output root"
  )
})

test_that("demo bundle builds from a safe temporary destination", {
  root <- release_test_root()
  withr::local_envvar(list(
    GTSC_DATA_ROOT = NA, GTSC_RELEASE_OUTPUT_DIR = NA, GTSC_RUNTIME_PROFILE = "demo"
  ))
  dest <- file.path(withr::local_tempdir(), "data", "release", "demo")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  sandbox <- withr::local_tempdir()
  res <- withr::with_dir(sandbox, build_demo_bundle(dest_dir = dest, root = root))
  expect_true(file.exists(res$manifest_path))
  expect_identical(res$manifest$runtime_profile, "demo")
})

test_that("app_server resolves its data root from GTSC_DATA_ROOT in demo mode", {
  root <- release_test_root()
  withr::local_envvar(list(
    GTSC_RUNTIME_PROFILE = "demo",
    GTSC_DATA_ROOT = "data/release/demo",
    GTSC_PUBLIC_MODE = "true",
    GTSC_READ_ONLY_MODE = "true",
    GTSC_ALLOW_SCENARIO_WRITES = "false"
  ))
  rt <- normalise_runtime_config()
  cfg <- apply_runtime_paths_to_config(load_config(root = root), rt)

  expect_identical(rt$data_root, "data/release/demo")

  expect_identical(
    cfg[["paths"]]$processed,
    file.path(root, "data", "release", "demo")
  )
  sandbox <- withr::local_tempdir()
  withr::with_dir(sandbox, {
    cfg2 <- apply_runtime_paths_to_config(load_config(root = root), normalise_runtime_config())
    expect_identical(cfg2[["paths"]]$processed, cfg[["paths"]]$processed)
  })

  withr::local_envvar(list(GTSC_DATA_ROOT = "/opt/gtsc/app/data/release/demo"))
  cfg3 <- apply_runtime_paths_to_config(load_config(root = root), normalise_runtime_config())
  expect_identical(cfg3[["paths"]]$processed, "/opt/gtsc/app/data/release/demo")
})

test_that("no module server reads a hardcoded processed data path", {
  root <- release_test_root()
  mods <- list.files(file.path(root, "R"), pattern = "^mod_.*\\.R$", full.names = TRUE)
  expect_true(length(mods) >= 9L)
  offenders <- character()
  for (f in mods) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    if (grepl('"data/processed|\'data/processed|data/release/current', txt)) {
      offenders <- c(offenders, basename(f))
    }
  }
  expect_identical(offenders, character())
})

test_that("mandatory demo snapshot tables load with non-zero rows", {
  root <- release_test_root()
  skip_if_not(dir.exists(file.path(root, "data", "release", "demo")))
  withr::local_envvar(list(
    GTSC_RUNTIME_PROFILE = "demo", GTSC_DATA_ROOT = "data/release/demo"
  ))
  cfg <- apply_runtime_paths_to_config(load_config(root = root), normalise_runtime_config())
  snap <- suppressWarnings(app_snapshot(cfg))

  expect_silent(assert_mandatory_snapshot_tables(snap))
  report <- snapshot_table_report(snap)
  for (nm in MANDATORY_SNAPSHOT_TABLES) {
    expect_true(nm %in% report$table, info = nm)
    expect_gt(report[table == nm]$rows[[1]], 0L)
  }
  expect_true(isTRUE(snap$overview_available))
  expect_true(isTRUE(snap$trade_flows_available))
  expect_true(isTRUE(snap$map_available))
})

test_that("startup fails loudly for an empty mandatory bundle", {
  empty <- list(production_manifest = list(), countries = data.table::data.table())
  expect_error(assert_mandatory_snapshot_tables(empty), "Mandatory snapshot tables unusable")
  expect_error(assert_mandatory_snapshot_tables(empty), "country_year_analytics")

  zero <- list()
  for (nm in MANDATORY_SNAPSHOT_TABLES) zero[[nm]] <- data.table::data.table()
  expect_error(assert_mandatory_snapshot_tables(zero), "Empty:")
})

test_that("the snapshot is built once per process and shared by sessions", {
  root <- release_test_root()
  skip_if_not(dir.exists(file.path(root, "data", "release", "demo")))
  withr::local_envvar(list(
    GTSC_RUNTIME_PROFILE = "demo", GTSC_DATA_ROOT = "data/release/demo"
  ))
  cfg <- apply_runtime_paths_to_config(load_config(root = root), normalise_runtime_config())

  first <- suppressWarnings(app_snapshot(cfg))
  t0 <- Sys.time()
  second <- app_snapshot(cfg)
  cached_seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_identical(first$loaded_at, second$loaded_at)
  expect_lt(cached_seconds, 0.5)
})

test_that("every module server is registered for a session", {
  root <- release_test_root()
  skip_if_not(dir.exists(file.path(root, "data", "release", "demo")))
  withr::local_envvar(list(
    GTSC_RUNTIME_PROFILE = "demo", GTSC_DATA_ROOT = "data/release/demo"
  ))
  cfg <- apply_runtime_paths_to_config(load_config(root = root), normalise_runtime_config())
  snap <- suppressWarnings(app_snapshot(cfg))

  module_fns <- c(
    "mod_overview_server", "mod_trade_flows_server", "mod_trade_balance_map_server",
    "mod_time_series_server", "mod_network_server", "mod_dependency_server",
    "mod_shock_simulator_server", "mod_forecasting_server", "mod_data_quality_server"
  )
  for (fn in module_fns) expect_true(exists(fn, mode = "function"), info = fn)

  suppressWarnings(shiny::testServer(app_server(cfg, snap), {
    session$setInputs(main_nav = "Executive Overview")
    expect_length(session$userData$gtsc_registered_modules, 9L)
    expect_identical(
      session$userData$gtsc_registered_modules,
      c("overview", "trade_flows", "trade_balance", "time_series", "network",
        "dependency", "shock", "forecast", "data_quality")
    )
  }))
})

test_that("cached map geometry loads without the expensive topology re-check", {
  root <- release_test_root()
  skip_if_not(requireNamespace("sf", quietly = TRUE))
  skip_if_not(dir.exists(file.path(root, "data", "release", "demo")))
  withr::local_envvar(list(
    GTSC_RUNTIME_PROFILE = "demo", GTSC_DATA_ROOT = "data/release/demo"
  ))
  cfg <- apply_runtime_paths_to_config(load_config(root = root), normalise_runtime_config())
  skip_if_not(file.exists(map_geometry_cache_path(cfg)))

  geom <- load_map_geometry(cfg)
  t0 <- Sys.time()
  geom_cached <- load_map_geometry(cfg)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_true(inherits(geom_cached, "sf"))
  expect_gt(nrow(geom_cached), 0L)
  expect_true(isTRUE(attr(geom_cached, "leaflet_geometry_prepared")))

  expect_lt(elapsed, 5)

  expect_silent(load_map_geometry(cfg, validate_cached = TRUE))
})
