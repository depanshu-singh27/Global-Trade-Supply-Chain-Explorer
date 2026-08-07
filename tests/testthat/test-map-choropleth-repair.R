test_that("continuous palette produces multiple visible fill colours", {
  skip_if_not_installed("leaflet")
  set.seed(1)
  vals <- c(rnorm(100, 0, 1e9), 5e11, -4e11)
  meta <- build_map_color_meta(vals, "trade_balance", "continuous")
  n_cols <- map_unique_fill_colours(vals, meta)
  expect_gt(n_cols, 5L)
  expect_true(meta$domain[1] < 0 && meta$domain[2] > 0)
  expect_true(all(abs(meta$domain) < max(abs(vals)) * 1.01))
})

test_that("sequential palette domain is winsorised", {
  vals <- c(seq(1e6, 5e7, length.out = 40), 1e12)
  meta <- build_map_color_meta(vals, "imports", "continuous")
  expect_lt(meta$domain[2], 1e12)
  expect_gt(map_unique_fill_colours(vals, meta), 1L)
})

test_that("map click updates selected economy from shape_click", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("sf")
  cy <- prepare_map_analytics(make_map_analytics_fixture())
  g <- make_synthetic_map_geometry()
  shiny::testServer(mod_trade_balance_map_server, args = list(
    snap = shiny::reactiveVal(list(
      map_analytics = cy,
      country_year_analytics = cy,
      map_geometry = g,
      geographic_crosswalk = build_geographic_crosswalk(cy$reporter_iso3, g),
      pipeline_status = list(global_trade = "complete", detailed_trade = "partial", macro = "complete")
    )),
    cfg = shiny::reactive(list())
  ), {
    session$setInputs(year = "2024", metric = "trade_balance", classify = "continuous",
                      country = "", region = "__ALL__")
    session$setInputs(choropleth_shape_click = list(id = "DEU"))
    expect_equal(selected_iso(), "DEU")
    j <- joined()
    expect_true(inherits(j$sf, "sf"))
    expect_true(sum(!is.na(j$sf$map_value)) >= 1L)
    meta <- color_meta()
    expect_gt(map_unique_fill_colours(j$sf$map_value, meta), 1L)
  })
})

test_that("join_map_data preserves sf and does not multiply rows", {
  skip_if_not_installed("sf")
  cy <- prepare_map_analytics(make_map_analytics_fixture())
  g <- make_synthetic_map_geometry()
  y <- filter_map_year(cy, year = 2024L, region = "__ALL__", geometry = g)
  j <- join_map_data(y, g, "trade_balance")
  expect_true(inherits(j$sf, "sf"))
  expect_equal(nrow(j$sf), nrow(g))
  expect_false(any(duplicated(j$sf$map_iso3)))
})
