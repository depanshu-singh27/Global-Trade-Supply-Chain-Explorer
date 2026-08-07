test_that("reporting_status_label accepts vectors (EXPR regression)", {
  status <- c(
    "represented_reporter", "partner_only", "both",
    "selected_reporter_missing", "unknown", NA_character_
  )
  labs <- reporting_status_label(status)
  expect_equal(length(labs), length(status))
  expect_equal(labs[1], "Represented reporter")
  expect_equal(labs[2], "Partner only")
  expect_equal(labs[3], "Reporter and partner")
  expect_false(any(is.na(labs)))
})

test_that("prepare_network_plotly no longer throws EXPR length error", {
  snap <- make_network_snap_fixture("partial")
  d <- snap$trade_detailed_enriched
  cov <- snap$detailed_coverage
  net <- build_full_trade_network(
    d,
    mode = "exports",
    year_min = 2020L,
    year_max = 2020L,
    top_n = 40L,
    selected_reporters = cov$selected_reporters,
    selected_partners = character(),
    represented_reporters = cov$represented_reporters
  )
  expect_true(nrow(net$nodes) >= 1L)
  prep <- prepare_network_plotly(net, colour_metric = "reporting_status")
  expect_true(is.list(prep))
  expect_true("plot_colour" %in% names(prep$nodes))
  fig <- network_plotly(net)
  expect_true(inherits(fig, "plotly") || inherits(fig, "htmlwidget"))
})

test_that("network plotly axis ranges are centred and layout uses domain constrain", {
  nodes <- data.table::data.table(
    x = c(0, 1, 2),
    y = c(10, 12, 14)
  )
  rng <- network_plotly_axis_ranges(nodes, pad_frac = 0.2)
  expect_equal(mean(rng$x), mean(range(nodes$x)), tolerance = 1e-9)
  expect_equal(mean(rng$y), mean(range(nodes$y)), tolerance = 1e-9)
  expect_gt(diff(rng$x), diff(range(nodes$x)))
  expect_gt(diff(rng$y), diff(range(nodes$y)))

  nodes2 <- data.table::data.table(x = c(-5, -4, 10), y = c(0, 1, 2))
  rng2 <- network_plotly_axis_ranges(nodes2, pad_frac = 0.1)
  expect_equal(mean(rng2$x), mean(range(nodes2$x)), tolerance = 1e-9)
  expect_equal(mean(rng2$y), mean(range(nodes2$y)), tolerance = 1e-9)

  snap <- make_network_snap_fixture("partial")
  net <- build_full_trade_network(
    snap$trade_detailed_enriched,
    mode = "exports",
    year_min = 2020L,
    year_max = 2020L,
    top_n = 40L,
    selected_reporters = snap$detailed_coverage$selected_reporters,
    selected_partners = character(),
    represented_reporters = snap$detailed_coverage$represented_reporters
  )
  fig <- network_plotly(net)
  built <- plotly::plotly_build(fig)
  lay <- built$x$layout
  expect_identical(lay$xaxis$scaleanchor, "y")
  expect_identical(lay$xaxis$constrain, "domain")
  expect_identical(lay$xaxis$constraintoward, "center")
  expect_identical(lay$yaxis$constrain, "domain")
  expect_identical(lay$yaxis$constraintoward, "middle")
  expect_false(isTRUE(lay$xaxis$autorange))
  expect_false(isTRUE(lay$yaxis$autorange))
  expect_equal(length(lay$xaxis$range), 2L)
  expect_equal(length(lay$yaxis$range), 2L)

  expect_equal(
    mean(lay$xaxis$range),
    mean(range(net$nodes$x, na.rm = TRUE)),
    tolerance = 1e-6
  )
  expect_equal(
    mean(lay$yaxis$range),
    mean(range(net$nodes$y, na.rm = TRUE)),
    tolerance = 1e-6
  )
})

test_that("scalar layout and mode inputs are normalised", {
  expect_silent(layout_network_coordinates(
    igraph::make_empty_graph(n = 0, directed = TRUE),
    method = c("fr", "kk")
  ))
  g <- igraph::make_graph(c("A", "B"), directed = TRUE)
  coords <- layout_network_coordinates(g, method = c("circle", "fr"))
  expect_equal(nrow(coords), 2L)
})

test_that("network module renders plot without EXPR error", {
  snap <- shiny::reactiveVal(make_network_snap_fixture("partial"))
  cfg <- shiny::reactive(list())
  shiny::testServer(mod_network_server, args = list(snap = snap, cfg = cfg), {
    session$setInputs(
      year_mode = "latest",
      year_min = 2020L,
      year_max = 2020L,
      mode = "exports",
      focus = "__ALL__",
      ego_order = "1",
      partners = c("__ALL__", "USA"),
      hs = "__ALL__",
      top_n = "50",
      size_metric = "total_strength",
      colour_metric = "reporting_status",
      layout = "fr",
      scale = "auto",
      rank_metric = "total_strength"
    )
    net <- network_result()
    expect_true(is.list(net))
    expect_silent(prepare_network_plotly(net))
    session$setInputs(mode = "imports", layout = "kk", colour_metric = "universe_membership")
    expect_equal(network_result()$built$mode, "imports")
    session$setInputs(year_mode = "full", focus = "DEU", hs = "8542")
    expect_true(is.list(network_result()))
  })
})
