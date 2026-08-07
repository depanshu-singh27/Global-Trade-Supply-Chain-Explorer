root_guess <- getwd()
if (file.exists(file.path(root_guess, "renv/activate.R"))) {
  source(file.path(root_guess, "renv/activate.R"), local = FALSE)
}
source(file.path(root_guess, "R", "utilities.R"), local = FALSE)
source(file.path(root_guess, "R", "config.R"), local = FALSE)

root <- find_project_root(root_guess)
setwd(root)

mandatory_fail <- FALSE
report <- function(item, ok, detail = "", mandatory = FALSE) {
  status <- if (ok) "OK" else if (mandatory) "FAIL" else "WARN"
  if (!ok && mandatory) mandatory_fail <<- TRUE
  cat(sprintf("%-28s [%s] %s\n", item, status, detail))
}

cat("=== Environment check ===\n")
report("R version", TRUE, paste(R.version$major, R.version$minor, sep = "."))
report("Project root", file.exists(file.path(root, "config.yml")), root, TRUE)

req_dirs <- c("R", "scripts", "data/raw", "data/interim", "data/processed",
              "data/reference", "data/cache", "tests", "docs", "www")
missing_dirs <- req_dirs[!dir.exists(file.path(root, req_dirs))]
report("Required directories", length(missing_dirs) == 0,
       if (length(missing_dirs)) paste(missing_dirs, collapse = ", ") else "all present",
       TRUE)

pkgs <- c("shiny", "bslib", "data.table", "plotly", "leaflet", "DT", "igraph",
          "networkD3", "arrow", "fst", "httr2", "jsonlite", "yaml", "memoise",
          "cachem", "testthat", "renv")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
report("Required packages", length(missing_pkgs) == 0,
       if (length(missing_pkgs)) paste(missing_pkgs, collapse = ", ") else "all loadable",
       TRUE)

arrow_ok <- requireNamespace("arrow", quietly = TRUE)
report("Arrow", arrow_ok, if (arrow_ok) "available" else "missing", TRUE)

has_make <- nzchar(Sys.which("make")) || nzchar(Sys.which("Rtools")) ||
  dir.exists("C:/rtools40") || dir.exists("C:/rtools43") || dir.exists("C:/rtools44")
report("Build tools", has_make, if (has_make) "detected" else "not clearly detected")

report("COMTRADE_PRIMARY", comtrade_key_present(),
       if (comtrade_key_present()) "present" else "missing", TRUE)

git_ok <- nzchar(Sys.which("git"))
report("Git", git_ok, if (git_ok) system2("git", "--version", stdout = TRUE)[1] else "not found")

docker_bin <- Sys.which("docker")
docker_ok <- nzchar(docker_bin)
docker_detail <- "not found"
if (docker_ok) {
  docker_detail <- tryCatch(
    system2("docker", "--version", stdout = TRUE, stderr = TRUE)[1],
    error = function(e) "found but version check failed"
  )
}
report("Docker", docker_ok, docker_detail)

cat("=== End environment check ===\n")
if (mandatory_fail) {
  cat("RESULT: FAIL (mandatory checks failed)\n")
  quit(status = 1)
}
cat("RESULT: PASS\n")
quit(status = 0)
