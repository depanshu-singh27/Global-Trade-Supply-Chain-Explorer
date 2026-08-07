dockerfile_lines <- function() {
  readLines(file.path(release_test_root(), "Dockerfile"), warn = FALSE)
}

test_that("Dockerfile keeps an owner-write bit on the root-owned R library", {
  lines <- dockerfile_lines()

  a_w_library <- grepl("a-w", lines, fixed = TRUE) &
    grepl("/opt/gtsc/renv/library", lines, fixed = TRUE)
  expect_false(any(a_w_library))

  expect_true(any(grepl("chmod -R u+w,go-w /opt/gtsc/renv/library", lines, fixed = TRUE)))

  expect_true(any(grepl("chown -R root:root /opt/gtsc/renv/library", lines, fixed = TRUE)))
  chown_library_to_user <- grepl("chown", lines, fixed = TRUE) &
    grepl("gtsc:gtsc", lines, fixed = TRUE) &
    grepl("/opt/gtsc/renv/library", lines, fixed = TRUE)
  expect_false(any(chown_library_to_user))

  expect_true(any(grepl("chmod -R a-w /opt/gtsc/app", lines, fixed = TRUE)))
  expect_true(any(grepl("chmod u+w /opt/gtsc/app/www", lines, fixed = TRUE)))

  expect_true(any(grepl("install -d -o gtsc -g gtsc -m 700 /tmp/gtsc", lines, fixed = TRUE)))

  expect_true(any(grepl("^USER gtsc", lines)))
  expect_false(any(grepl("chmod\\s+(-R\\s+)?777", lines)))
  expect_false(any(grepl("^USER root", lines)))
})

test_that("Dockerfile runs the asset-permission smoke test as the runtime user", {
  lines <- dockerfile_lines()

  user_idx <- grep("^USER gtsc", lines)
  run_idx <- grep("^RUN Rscript /opt/gtsc/permission_smoke\\.R", lines)
  copy_idx <- grep("permission_smoke\\.R", lines)

  expect_true(length(user_idx) >= 1L)
  expect_equal(length(run_idx), 1L)
  expect_true(length(copy_idx) >= 2L)

  expect_true(run_idx[[1]] > min(user_idx))
})

test_that("permission smoke script exercises copy and overwrite of bslib/shiny assets", {
  p <- file.path(release_test_root(), "docker", "permission_smoke.R")
  expect_true(file.exists(p))
  lines <- readLines(p, warn = FALSE)
  txt <- paste(lines, collapse = "\n")

  expect_true(grepl("bootstrap.bundle.min.js", txt, fixed = TRUE))
  expect_true(grepl("selectize.min.js", txt, fixed = TRUE))
  expect_true(grepl("package = \"bslib\"", txt, fixed = TRUE))
  expect_true(grepl("package = \"shiny\"", txt, fixed = TRUE))

  expect_equal(sum(grepl("file.copy(", lines, fixed = TRUE)), 2L)
  expect_true(grepl("overwrite = TRUE", txt, fixed = TRUE))
  expect_true(grepl("copy.mode = TRUE", txt, fixed = TRUE))

  expect_true(grepl("must not be writable by the runtime user", txt, fixed = TRUE))
  expect_true(grepl("TMPDIR not writable", txt, fixed = TRUE))
  expect_true(grepl("copied file is not writable", txt, fixed = TRUE))
  expect_true(grepl("quit(status = 1L)", txt, fixed = TRUE))
})

test_that("container R helper script is LF-only", {
  p <- file.path(release_test_root(), "docker", "permission_smoke.R")
  raw <- readBin(p, what = "raw", n = as.integer(file.info(p)$size))
  expect_false(any(raw == as.raw(0x0d)))
  expect_true(any(raw == as.raw(0x0a)))
})
