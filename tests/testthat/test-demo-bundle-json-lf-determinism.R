test_that("demo bundle JSON files are LF-only UTF-8 and checksums match", {
  root <- release_test_root()
  bundle_dir <- file.path(root, "data", "release", "demo")

  expect_true(file.exists(file.path(bundle_dir, "release_bundle_manifest.json")))
  man <- safe_read_json(file.path(bundle_dir, "release_bundle_manifest.json"))
  expect_true(!is.null(man$files))

  json_entries <- Filter(
    function(ent) grepl("\\.json$", ent$path %||% ent$relative_path %||% "", ignore.case = TRUE),
    man$files %||% list()
  )
  expect_true(length(json_entries) >= 1L)

  for (i in seq_along(json_entries)) {
    ent <- json_entries[[i]]
    rel <- ent$path %||% ent$relative_path %||% ""
    p <- file.path(bundle_dir, rel)
    expect_true(file.exists(p), info = paste("missing:", rel))

    raw <- readBin(p, what = "raw", n = as.integer(file.info(p)$size))
    expect_false(any(raw == as.raw(0x0d)), info = paste("CR byte present:", rel))

    parsed <- tryCatch(
      jsonlite::fromJSON(p, simplifyVector = FALSE),
      error = function(e) e
    )
    expect_false(inherits(parsed, "error"),
                  info = paste("invalid JSON/encoding:", rel))

    expect_identical(file_sha256(p), ent$sha256 %||% ent$checksum,
                     info = paste("checksum mismatch:", rel))
  }

  tmp <- file.path(tempdir(), paste0("gtsc-demo-json-lf-", as.integer(Sys.time())))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

  fns <- list.files(bundle_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  ok <- vapply(fns, function(from) file.copy(from, file.path(tmp, basename(from))), logical(1))
  expect_true(all(ok), info = "failed to copy some demo bundle files")

  vr <- validate_release_bundle(tmp, expected_profile = "demo")
  expect_false(any(vr$status == "fail"), info = paste(vr[vr$status == "fail"]$check, collapse = ","))
})
