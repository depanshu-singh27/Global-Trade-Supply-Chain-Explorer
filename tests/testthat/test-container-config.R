test_that("Dockerfile is non-root, exposes 3838, has healthcheck and entrypoint order", {
  root <- release_test_root()
  df <- paste(readLines(file.path(root, "Dockerfile")), collapse = "\n")
  expect_true(grepl("USER gtsc|USER 10001|useradd.*gtsc", df))
  expect_true(grepl("EXPOSE 3838", df))
  expect_false(grepl("EXPOSE (?!3838)\\d+", df, perl = TRUE) &&
                 grepl("EXPOSE [0-9]+.*EXPOSE", df))
  expect_true(grepl("HEALTHCHECK", df))
  expect_true(grepl(
    "ARG[[:space:]]+R_VER[[:space:]]*=[[:space:]]*4\\.6\\.1",
    df
  ))
  expect_true(grepl(
    "ARG[[:space:]]+ROCKER_DIGEST[[:space:]]*=",
    df
  ))
  expect_true(grepl(
    "FROM rocker/r-ver:${R_VER}@${ROCKER_DIGEST}",
    df,
    fixed = TRUE
  ))
  expect_true(grepl("entrypoint\\.sh", df))
  ep <- paste(readLines(file.path(root, "docker/entrypoint.sh")), collapse = "\n")
  expect_true(grepl("validate_runtime_profile_or_stop", ep))
  expect_true(grepl("refusing_root_runtime|id -u", ep))
  expect_true(grepl("missing_bundle_manifest", ep))
  expect_true(grepl("unset COMTRADE_PRIMARY|COMTRADE_PRIMARY", ep))
  expect_false(grepl("source.*\\.Renviron|readRenviron", ep))
})

test_that("dockerignore excludes secrets/raw/renv library and keeps source/lock", {
  root <- release_test_root()
  di <- readLines(file.path(root, ".dockerignore"))
  txt <- paste(di, collapse = "\n")
  expect_true(any(grepl("\\.Renviron", di)))
  expect_true(any(grepl("data/raw", di)))
  expect_true(any(grepl("renv/library", di)))
  expect_true(any(grepl("^\\.git$", di) | grepl("^\\.git$", trimws(di))))

  expect_false(any(grepl("^R$", di)))
  expect_false(any(grepl("^renv\\.lock$", di)))
  expect_false(any(grepl("^app\\.R$", di)))
})

test_that("compose demo/release/health/read-only and no secrets", {
  root <- release_test_root()
  yml <- paste(readLines(file.path(root, "docker-compose.yml")), collapse = "\n")
  expect_true(grepl("profiles:\\s*\\[\"demo\"\\]|profiles: \\[\"demo\"\\]", yml))
  expect_true(grepl("release", yml))
  expect_true(grepl("healthcheck", yml, ignore.case = TRUE))
  expect_true(grepl("read_only:\\s*true", yml))
  expect_false(grepl("COMTRADE_PRIMARY\\s*=", yml))
  expect_false(grepl("subscription_key\\s*=", yml, ignore.case = TRUE))
  envex <- paste(readLines(file.path(root, "docker/runtime.env.example")), collapse = "\n")
  expect_false(grepl("COMTRADE_PRIMARY\\s*=", envex))
})

test_that("CI workflows exist, offline, no COMTRADE requirement, gated publish", {
  root <- release_test_root()
  r <- paste(readLines(file.path(root, ".github/workflows/r-tests.yml")), collapse = "\n")
  c <- paste(readLines(file.path(root, ".github/workflows/container-build.yml")), collapse = "\n")
  s <- paste(readLines(file.path(root, ".github/workflows/security-scan.yml")), collapse = "\n")
  expect_true(grepl("testthat|test_dir|test-dir", r))
  expect_false(grepl("09_fetch_detailed|19_fetch_monthly|run_phase3_macro|COMTRADE_PRIMARY", r))
  expect_true(grepl("demo", r, ignore.case = TRUE))
  expect_true(grepl("docker build", c, ignore.case = TRUE))
  expect_false(grepl("docker push", c) && !grepl("if:.*tags|release.*tag|github.ref_type", c))

  if (grepl("docker push", c)) {
    expect_true(grepl("tags|ref_type|release", c))
  }
  expect_true(grepl("trivy|gitleaks|secret", s, ignore.case = TRUE))
})

test_that("gitattributes forces LF for shell scripts", {
  root <- release_test_root()
  ga <- paste(readLines(file.path(root, ".gitattributes"), warn = FALSE), collapse = "\n")
  expect_true(grepl("\\*\\.sh\\s+text\\s+eol=lf", ga))
})

test_that("container shell scripts are LF-compatible (no CR)", {
  root <- release_test_root()
  scripts <- c(
    file.path(root, "docker", "entrypoint.sh"),
    file.path(root, "docker", "healthcheck.sh")
  )
  for (p in scripts) {
    raw <- readBin(p, what = "raw", n = file.info(p)$size)
    expect_false(any(raw == as.raw(0x0d)), info = basename(p))
    expect_true(any(raw == as.raw(0x0a)), info = basename(p))
  }
})

test_that("baked health resource is not rewritten; no permission warning", {
  tmp <- tempfile("gtsc-health-")
  dir.create(file.path(tmp, "www"), recursive = TRUE)
  baked <- file.path(tmp, "www", "__gtsc_health__")
  writeLines('{"status":"ok","baked":true}', baked, useBytes = TRUE)
  before <- readLines(baked, warn = FALSE)
  mtime0 <- file.info(baked)$mtime
  cfg <- normalise_runtime_config(list(
    runtime_profile = "demo", host = "0.0.0.0", port = 3838L,
    data_root = "data/processed", scenario_root = "data/scenarios",
    performance_root = "data/performance", read_only_mode = TRUE,
    public_mode = TRUE, allow_scenario_writes = FALSE
  ))

  path <- write_health_www_resource(tmp, cfg)
  expect_identical(normalizePath(path, winslash = "/"), normalizePath(baked, winslash = "/"))
  expect_identical(readLines(baked, warn = FALSE), before)
  expect_equal(as.numeric(file.info(baked)$mtime), as.numeric(mtime0), tolerance = 1)

  src <- paste(readLines(file.path(release_test_root(), "R", "runtime_health.R"), warn = FALSE),
               collapse = "\n")
  expect_true(grepl("file\\.exists\\(path\\)", src))
  expect_false(grepl("health_www_not_writable_using_existing", src))
})

test_that("entrypoint skips health rewrite when baked file exists", {
  root <- release_test_root()
  ep <- paste(readLines(file.path(root, "docker", "entrypoint.sh"), warn = FALSE), collapse = "\n")
  expect_true(grepl("file\\.exists\\(\"www/__gtsc_health__\"\\)", ep) ||
                grepl("!file.exists\\(\"www/__gtsc_health__\"\\)", ep))
  expect_true(grepl("write_health_www_resource", ep))
})
