register_app_web_dependencies <- function(ui, static_root = NULL) {
  rendered <- htmltools::renderTags(ui)

  shiny_runtime_dependencies <- shiny:::shinyDependencies()
  shiny_runtime_dependencies <- Filter(
    function(dependency) inherits(dependency, "html_dependency"),
    shiny_runtime_dependencies
  )

  dependencies <- htmltools::resolveDependencies(
    c(
      list(
        shiny:::jqueryDependency(),
        shiny:::shinyDependencyCSS(gte_bs_theme())
      ),
      shiny_runtime_dependencies,
      rendered$dependencies
    ),
    resolvePackageDir = TRUE
  )

  materialise <- !is.null(static_root)

  if (materialise) {
    static_root <- normalizePath(
      static_root,
      winslash = "/",
      mustWork = FALSE
    )

    if (!dir.exists(static_root)) {
      dir.create(
        static_root,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }

    if (!dir.exists(static_root) ||
        file.access(static_root, mode = 2L) != 0L) {
      stop(
        "Static web-dependency root is not writable: ",
        static_root,
        call. = FALSE
      )
    }
  }

  old_dir_version <- getOption("htmltools.dir.version")
  on.exit(
    options(htmltools.dir.version = old_dir_version),
    add = TRUE
  )
  options(htmltools.dir.version = TRUE)

  rows <- lapply(dependencies, function(dependency) {
    source_path <- dependency$src[["file"]]

    if (is.null(source_path) ||
        !length(source_path) ||
        !nzchar(as.character(source_path[[1L]]))) {
      return(NULL)
    }

    served_dependency <- dependency

    if (materialise) {
      served_dependency <- htmltools::copyDependencyToDir(
        dependency,
        outputDir = static_root,
        mustWork = TRUE
      )
    }

    web_dependency <- shiny::createWebDependency(served_dependency)

    href <- as.character(
      web_dependency$src[["href"]] %||% ""
    )
    prefix <- sub("^/+", "", href)
    prefix <- sub("/.*$", "", prefix)

    mappings <- shiny::resourcePaths()
    directory <- unname(mappings[[prefix]] %||% "")

    if (!nzchar(prefix) ||
        !nzchar(directory) ||
        !dir.exists(directory)) {
      stop(
        "Failed to register web dependency: ",
        dependency$name %||% "<unnamed>",
        call. = FALSE
      )
    }

    data.frame(
      name = as.character(dependency$name %||% ""),
      version = as.character(dependency$version %||% ""),
      prefix = prefix,
      directory = directory,
      materialised = materialise,
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)

  report <- if (length(rows)) {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  } else {
    data.frame(
      name = character(),
      version = character(),
      prefix = character(),
      directory = character(),
      materialised = logical(),
      stringsAsFactors = FALSE
    )
  }

  required <- c("jquery", "shiny-javascript")
  missing_required <- setdiff(required, report$name)

  if (length(missing_required)) {
    stop(
      "Core browser dependencies were not discovered: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  if (exists("runtime_log", mode = "function")) {
    runtime_log(
      "INFO",
      "web_dependencies_registered",
      list(
        dependency_count = nrow(report),
        materialised = materialise,
        static_root = if (materialise) static_root else "<disabled>",
        prefixes = paste(report$prefix, collapse = ",")
      )
    )
  }

  report
}

app_ui <- function(cfg) {
  style_path <- file.path(
    cfg$project_root %||% find_project_root(),
    "www",
    "styles.css"
  )

  style_version <- if (file.exists(style_path)) {
    unname(tools::md5sum(style_path))
  } else {
    "1"
  }

  bslib::page_navbar(
    title = cfg$app$name %||% "Global Trade & Supply Chain Explorer",
    id = "main_nav",
    theme = gte_bs_theme(),
    header = shiny::tags$head(
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = paste0("styles.css?v=", style_version)
      )
    ),
    bslib::nav_panel("Executive Overview", mod_overview_ui("overview")),
    bslib::nav_panel("Trade Flows", mod_trade_flows_ui("trade_flows")),
    bslib::nav_panel("Trade Balance Map", mod_trade_balance_map_ui("trade_balance")),
    bslib::nav_panel("Time Series", mod_time_series_ui("time_series")),
    bslib::nav_panel("Trade Network", mod_network_ui("network")),
    bslib::nav_panel("Dependency Explorer", mod_dependency_ui("dependency")),
    bslib::nav_panel("Shock Simulator", mod_shock_simulator_ui("shock")),
    bslib::nav_panel("Forecasting", mod_forecasting_ui("forecast")),
    bslib::nav_panel("Data Quality", mod_data_quality_ui("data_quality"))
  )
}
