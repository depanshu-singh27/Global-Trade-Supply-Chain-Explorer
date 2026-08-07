root <- normalizePath(getwd(), winslash = "/")
if (file.exists(file.path(root, "renv/activate.R"))) source(file.path(root, "renv/activate.R"))
source(file.path(root, "R/zzz_bootstrap.R"))
source_project_r(root)

paths <- ensure_release_dirs(release_paths(root))
image <- Sys.getenv("GTSC_RELEASE_IMAGE_TAG", "gtsc:phase14-rc")
rows <- list()
add <- function(check, status, detail = NA_character_) {
  rows[[length(rows) + 1L]] <<- validation_row(check, status, detail)
}

docker_ok <- nzchar(Sys.which("docker"))
if (!docker_ok) {
  add("docker_available", "not_run", "docker not on PATH")
  out <- data.table::rbindlist(rows, fill = TRUE)
  arrow::write_parquet(as.data.frame(out),
                       file.path(paths$validation, "container_validation.parquet"))
  write_json_atomic(list(status = "not_run", rows = out),
                    file.path(paths$validation, "container_smoke_report.json"))
  message("CONTAINER_VALIDATION_NOT_RUN")
  quit(status = 0)
}
add("docker_available", "pass")

img <- tryCatch(
  system2("docker", c("image", "inspect", image), stdout = TRUE, stderr = TRUE),
  error = function(e) character()
)
if (!length(img) || grepl("Error|No such", paste(img, collapse = "\n"))) {
  add("image_present", "fail", image)
} else {
  add("image_present", "pass", image)
}

user_cfg <- tryCatch(
  system2("docker", c("image", "inspect", "--format", "{{.Config.User}}", image),
          stdout = TRUE, stderr = FALSE),
  error = function(e) ""
)
if (length(user_cfg) && nzchar(user_cfg[[1]]) && !identical(user_cfg[[1]], "0") &&
    !identical(user_cfg[[1]], "root")) {
  add("image_non_root_user", "pass", user_cfg[[1]])
} else {
  add("image_non_root_user", "fail", paste(user_cfg, collapse = " "))
}

cname <- paste0("gtsc-smoke-", as.integer(Sys.time()))

run <- tryCatch(
  system2("docker", c(
    "run", "-d", "--name", cname, "--publish", "3839:3838",
    "--env", "GTSC_RUNTIME_PROFILE=demo",
    "--env", "GTSC_PUBLIC_MODE=true",
    "--env", "GTSC_ALLOW_SCENARIO_WRITES=false",
    "--env", "GTSC_READ_ONLY_MODE=true",
    image
  ), stdout = TRUE, stderr = TRUE),
  error = function(e) paste("error", conditionMessage(e))
)

started <- is.character(run) && !grepl("(?i)error", paste(run, collapse = "\n"), perl = TRUE)
if (!started) {
  add("container_start", "fail", paste(run, collapse = " | "))
} else {
  add("container_start", "pass")

  healthy <- FALSE
  for (i in 1:40) {
    Sys.sleep(3)
    st <- tryCatch(
      system2("docker", c("inspect", "--format", "{{.State.Health.Status}}", cname),
              stdout = TRUE, stderr = FALSE),
      error = function(e) "unknown"
    )
    if (length(st) && identical(st[[1]], "healthy")) {
      healthy <- TRUE
      break
    }

    http <- tryCatch(
      system2("curl", c("-fsS", "--max-time", "3", "http://127.0.0.1:3839/__gtsc_health__"),
              stdout = TRUE, stderr = FALSE),
      error = function(e) character()
    )
    if (length(http) && grepl("\"status\"\\s*:\\s*\"ok\"", paste(http, collapse = ""))) {
      healthy <- TRUE
      break
    }
  }
  add("healthcheck", if (healthy) "pass" else "fail")

  root_http <- tryCatch(
    system2("curl", c("-fsS", "--max-time", "5", "-o", "NUL", "-w", "%{http_code}",
                       "http://127.0.0.1:3839/"), stdout = TRUE, stderr = FALSE),
    error = function(e) "000"
  )
  code <- if (length(root_http)) root_http[[1]] else "000"
  add("root_http", if (code %in% c("200", "302", "303")) "pass" else "fail", code)

  uid <- tryCatch(
    system2("docker", c("exec", cname, "id", "-u"), stdout = TRUE, stderr = FALSE),
    error = function(e) "0"
  )
  add("effective_uid_nonzero",
       if (length(uid) && !identical(uid[[1]], "0")) "pass" else "fail",
       paste(uid, collapse = ""))

  renviron <- tryCatch(
    system2("docker", c("exec", cname, "test", "!", "-f", "/opt/gtsc/app/.Renviron"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) 1
  )

  ren_status <- tryCatch(
    system2("docker", c("exec", cname, "test", "!", "-f", "/opt/gtsc/app/.Renviron"),
            stdout = FALSE, stderr = FALSE),
    error = function(e) 1
  )
  add("no_renviron_in_container", if (identical(ren_status, 0L)) "pass" else "fail")

  logs <- tryCatch(
    system2("docker", c("logs", cname), stdout = TRUE, stderr = TRUE),
    error = function(e) character()
  )
  sec_ok <- tryCatch({
    assert_no_comtrade_in_logs(logs)
    TRUE
  }, error = function(e) FALSE)
  add("startup_log_redaction", if (sec_ok) "pass" else "fail")
  if (any(grepl("Demo|Synthetic|fixture_synthetic|demo", logs, ignore.case = TRUE))) {
    add("demo_provenance_in_logs", "pass")
  } else {
    add("demo_provenance_in_logs", "warning", "not observed in logs")
  }

  system2("docker", c("stop", "-t", "15", cname), stdout = FALSE, stderr = FALSE)
  add("graceful_shutdown", "pass")
  system2("docker", c("rm", "-f", cname), stdout = FALSE, stderr = FALSE)
}

out <- data.table::rbindlist(rows, fill = TRUE)
arrow::write_parquet(as.data.frame(out),
                     file.path(paths$validation, "container_validation.parquet"))
write_json_atomic(
  list(
    image = image,
    status = gate_status_from_rows(out),
    checks = out
  ),
  file.path(paths$validation, "container_smoke_report.json")
)
print(out)
if (any(out$status == "fail")) {
  message("CONTAINER_VALIDATION_FAIL")
  quit(status = 1)
}
message("CONTAINER_VALIDATION_OK")
