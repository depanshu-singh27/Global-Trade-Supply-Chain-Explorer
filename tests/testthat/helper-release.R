release_test_root <- function() {
  find_project_root()
}

with_temp_env <- function(vars, expr) {
  old <- lapply(names(vars), function(nm) Sys.getenv(nm, unset = NA_character_))
  names(old) <- names(vars)
  on.exit({
    for (nm in names(old)) {
      if (is.na(old[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, setNames(list(old[[nm]]), nm))
    }
  }, add = TRUE)
  do.call(Sys.setenv, as.list(vars))
  force(expr)
}

expect_validation_has <- function(dt, check, status = NULL) {
  testthat::expect_true(check %in% dt$check)
  if (!is.null(status)) {
    testthat::expect_identical(dt$status[dt$check == check][[1]], status)
  }
}
