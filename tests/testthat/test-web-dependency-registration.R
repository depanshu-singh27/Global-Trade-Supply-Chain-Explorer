test_that("initial UI dependencies are registered and materialised", {
  root <- find_project_root()
  cfg <- load_config(root = root)
  ui <- app_ui(cfg)

  existing_paths <- shiny::resourcePaths()
  static_root <- tempfile("gtsc-web-dependencies-")
  dir.create(static_root, recursive = TRUE)

  new_prefixes <- character()

  on.exit({
    for (prefix in unique(new_prefixes)) {
      if (prefix %in% names(shiny::resourcePaths())) {
        shiny::removeResourcePath(prefix)
      }
    }
    unlink(static_root, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  report <- register_app_web_dependencies(
    ui,
    static_root = static_root
  )

  new_prefixes <- setdiff(
    report$prefix,
    names(existing_paths)
  )

  expect_gt(nrow(report), 0L)
  expect_true(
    all(c("jquery", "shiny-javascript") %in% report$name)
  )
  expect_true(all(report$prefix %in% names(shiny::resourcePaths())))
  expect_true(all(dir.exists(report$directory)))
  expect_true(all(report$materialised))

  jquery_prefix <- report$prefix[report$name == "jquery"][[1L]]
  shiny_prefix <- report$prefix[
    report$name == "shiny-javascript"
  ][[1L]]

  expect_true(
    file.exists(
      file.path(static_root, jquery_prefix, "jquery.min.js")
    )
  )
  expect_true(
    file.exists(
      file.path(static_root, shiny_prefix, "shiny.min.js")
    )
  )
})

test_that("Docker enables static dependency materialisation", {
  dockerfile <- readLines(
    file.path(find_project_root(), "Dockerfile"),
    warn = FALSE
  )

  expect_true(any(grepl(
    "GTSC_MATERIALIZE_WEB_DEPS=true",
    dockerfile,
    fixed = TRUE
  )))
})
