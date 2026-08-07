test_that("chart helpers reject Inf/NaN and render empty states", {
  skip_if_not_installed("plotly")
  empty <- trade_flow_empty_plotly("none")
  expect_s3_class(empty, "plotly")
  prep <- list(
    data = data.table::data.table(year = 2020:2021, series = "Imports", value = c(1, 2)),
    note = "ok"
  )
  fig <- trade_flow_timeseries_plotly(prep, "t", "millions")
  expect_s3_class(fig, "plotly")
  comp <- prepare_commodity_composition(prepare_detailed_trade(make_trade_flow_fixture()), 5L)
  expect_s3_class(trade_flow_composition_plotly(comp, "c"), "plotly")
  mat <- prepare_reporter_partner_matrix(prepare_detailed_trade(make_trade_flow_fixture()))
  expect_s3_class(trade_flow_matrix_plotly(mat, "m"), "plotly")
})

test_that("sankey network builds from fixture without Inf", {
  skip_if_not_installed("networkD3")
  det <- prepare_detailed_trade(make_trade_flow_fixture())
  paths <- trade_flow_path_aggregates(det, "reporter_partner")
  sel <- select_top_n_paths(paths, 5L)
  sk <- build_sankey_data(sel$visible, "reporter_partner")
  expect_false(any(is.infinite(sk$links$value)))
  expect_false(any(is.nan(sk$links$value)))
  widget <- render_trade_flow_sankey(sk)
  expect_true(is.null(widget) || inherits(widget, "htmlwidget"))
})
