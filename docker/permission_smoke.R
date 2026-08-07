.libPaths(c(Sys.getenv("R_LIBS_USER"), .libPaths()))

fail <- function(...) {
  message("PERMISSION_SMOKE_FAIL: ", ...)
  quit(status = 1L)
}

lib <- Sys.getenv("RENV_PATHS_LIBRARY", unset = "/opt/gtsc/renv/library")
tmp_root <- Sys.getenv("TMPDIR", unset = "/tmp/gtsc")

if (!dir.exists(lib)) fail("library directory missing: ", lib)
if (file.access(lib, mode = 2L) == 0L) {
  fail("package library must not be writable by the runtime user: ", lib)
}

if (!dir.exists(tmp_root)) fail("TMPDIR missing: ", tmp_root)
if (file.access(tmp_root, mode = 2L) != 0L) fail("TMPDIR not writable: ", tmp_root)

assets <- c(
  "bootstrap.bundle.min.js" =
    system.file("lib/bs5/dist/js/bootstrap.bundle.min.js", package = "bslib"),
  "selectize.min.js" =
    system.file("www/shared/selectize/js/selectize.min.js", package = "shiny")
)
absent <- names(assets)[!nzchar(assets) | !file.exists(assets)]
if (length(absent)) fail("asset not found: ", paste(absent, collapse = ", "))

dest_dir <- file.path(tempdir(), "gtsc-permission-smoke")
dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
if (file.access(dest_dir, mode = 2L) != 0L) {
  fail("temporary destination not writable: ", dest_dir)
}

for (nm in names(assets)) {
  src <- assets[[nm]]
  dst <- file.path(dest_dir, nm)

  first <- file.copy(src, dst, overwrite = TRUE, copy.mode = TRUE)
  if (!isTRUE(first)) fail("first copy failed: ", nm)
  if (file.access(dst, mode = 2L) != 0L) fail("copied file is not writable: ", nm)

  second <- file.copy(src, dst, overwrite = TRUE, copy.mode = TRUE)
  if (!isTRUE(second)) fail("overwrite copy failed: ", nm)
}

unlink(dest_dir, recursive = TRUE, force = TRUE)
message("PERMISSION_SMOKE_OK")
