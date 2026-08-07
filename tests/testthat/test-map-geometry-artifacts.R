test_that("prepare_leaflet_geometry preserves multipart rings", {
  skip_if_not_installed("sf")

  square <- function(xmin, ymin, xmax, ymax) {
    sf::st_polygon(list(matrix(
      c(
        xmin, ymin, xmax, ymin, xmax, ymax,
        xmin, ymax, xmin, ymin
      ),
      ncol = 2,
      byrow = TRUE
    )))
  }
  multipart <- sf::st_multipolygon(list(
    unclass(square(0, 0, 1, 1)),
    unclass(square(3, 0, 4, 1))
  ))
  input <- sf::st_sf(
    map_iso3 = "TST",
    geometry = sf::st_sfc(multipart, crs = 4326)
  )

  output <- prepare_leaflet_geometry(input)

  expect_s3_class(output, "sf")
  expect_equal(nrow(output), 1L)
  expect_equal(as.character(sf::st_geometry_type(output)), "MULTIPOLYGON")
  expect_equal(length(sf::st_geometry(output)[[1]]), 2L)
})

test_that("duplicate ISO3 rows union only within country", {
  skip_if_not_installed("sf")

  square <- function(xmin) {
    sf::st_polygon(list(matrix(
      c(xmin, 0, xmin + 1, 0, xmin + 1, 1, xmin, 1, xmin, 0),
      ncol = 2,
      byrow = TRUE
    )))
  }
  input <- sf::st_sf(
    map_iso3 = c("AAA", "AAA", "BBB"),
    map_name = c("Alpha", "Alpha", "Beta"),
    geometry = sf::st_sfc(square(0), square(3), square(10), crs = 4326)
  )

  output <- prepare_leaflet_geometry(input)

  expect_equal(nrow(output), 2L)
  expect_equal(anyDuplicated(output$map_iso3), 0L)
  expect_equal(length(sf::st_geometry(output[output$map_iso3 == "AAA", ])[[1]]), 2L)
  expect_equal(length(sf::st_geometry(output[output$map_iso3 == "BBB", ])[[1]]), 1L)
})

test_that("antimeridian rings are split without world-spanning edges", {
  skip_if_not_installed("sf")

  dateline_polygon <- sf::st_polygon(list(matrix(
    c(179, -10, -179, -10, -179, 10, 179, 10, 179, -10),
    ncol = 2,
    byrow = TRUE
  )))
  input <- sf::st_sf(
    map_iso3 = "DAT",
    geometry = sf::st_sfc(dateline_polygon, crs = 4326)
  )

  output <- prepare_leaflet_geometry(input)
  diagnostics <- validate_leaflet_geometry(output)
  coords <- sf::st_coordinates(output)

  expect_equal(as.character(sf::st_geometry_type(output)), "MULTIPOLYGON")
  expect_true(all(coords[, "X"] >= -180 & coords[, "X"] <= 180))
  expect_false(any(diagnostics$world_spanning_edge))
  expect_lte(diagnostics$max_ring_longitude_jump, 180)
})

test_that("bundled Natural Earth priority countries are Leaflet safe", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rnaturalearth")

  geometry <- build_map_geometry()
  diagnostics <- validate_leaflet_geometry(geometry)
  priority <- c("RUS", "CAN", "USA", "FRA", "NOR", "FJI", "NZL")
  checked <- diagnostics[map_iso3 %in% priority]

  expect_setequal(checked$map_iso3, priority)
  expect_true(all(checked$valid))
  expect_false(any(checked$empty))
  expect_false(any(checked$world_spanning_edge))
  expect_equal(anyDuplicated(geometry$map_iso3), 0L)
  expect_true(all(
    as.character(sf::st_geometry_type(geometry)) %in%
      c("POLYGON", "MULTIPOLYGON")
  ))
  expect_true(all(c("RUS", "CAN", "FJI") %in% checked$map_iso3))
})

test_that("map widget disables world copies and tile wrapping", {
  source_text <- paste(
    readLines(file.path(TEST_ROOT, "R", "mod_trade_balance_map.R")),
    collapse = "\n"
  )

  expect_match(source_text, "worldCopyJump\\s*=\\s*FALSE")
  expect_match(source_text, "providerTileOptions\\(noWrap\\s*=\\s*TRUE\\)")
  expect_match(source_text, "setMaxBounds\\(-180,\\s*-60,\\s*180,\\s*85\\)")
  expect_match(source_text, "fitBounds\\(-180,\\s*-60,\\s*180,\\s*85\\)")
  expect_match(source_text, "trade-map-selection-style")
})
