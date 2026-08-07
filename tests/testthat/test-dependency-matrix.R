test_that("reporter-supplier matrix construction and reconciliation", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019)
  mat <- build_reporter_supplier_matrix(built$shares, metric = "partner_share")
  expect_true(mat$n_rows > 0 && mat$n_cols > 0)
  expect_equal(nrow(mat$long), mat$n_observed)

  sums <- mat$long[, .(s = sum(partner_share, na.rm = TRUE)), by = reporter_iso3]
  expect_true(all(abs(sums$s - 1) < 1e-8))

  expect_true(any(is.na(as.matrix(mat$wide[, -1, with = FALSE]))) || mat$n_observed < mat$n_rows * mat$n_cols)
})

test_that("country-commodity node IDs and same-HS4 edges", {
  expect_equal(make_country_commodity_node_id("DEU", "8542"), "DEU::8542")
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019)
  sp <- build_country_commodity_sparse_matrix(built$shares, max_nodes = 200L)
  expect_true(sp$n_nodes > 0)
  expect_true(sp$n_edges > 0)

  from_hs <- vapply(strsplit(sp$edges$from_node, "::", fixed = TRUE), `[`, character(1), 2)
  to_hs <- vapply(strsplit(sp$edges$to_node, "::", fixed = TRUE), `[`, character(1), 2)
  expect_equal(from_hs, to_hs)
  expect_equal(from_hs, sp$edges$hs_code)
})

test_that("sparse matrix dimensions, density, no padding", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019)
  sp <- build_country_commodity_sparse_matrix(built$shares, max_nodes = 200L)
  expect_true(sp$n_nodes < 200L)
  expect_equal(nrow(sp$nodes), sp$n_nodes)
  expect_true(is.finite(sp$density) || is.na(sp$density))
  expect_false(isTRUE(sp$capped))
})

test_that("200-node cap and deterministic ranking", {

  reps <- sprintf("R%02d", 1:30)
  parts <- sprintf("P%02d", 1:30)
  hs <- sprintf("%04d", 8500:8510)
  grid <- data.table::CJ(reporter_iso3 = reps[1:10], partner_iso3 = parts[1:10], hs_code = hs[1:5])
  grid[, `:=`(
    reporter_name = reporter_iso3, partner_name = partner_iso3,
    commodity_description = hs_code,
    year_start = 2020L, year_end = 2020L,
    partner_import_value = as.numeric(seq_len(.N)),
    observation_count = 1L,
    reporter_gdp_current_usd = NA_real_
  )]
  shares <- build_dependency_shares(grid)
  nodes <- rank_active_dependency_nodes(shares, max_nodes = 200L)
  expect_true(nrow(nodes) <= 200L)
  nodes2 <- rank_active_dependency_nodes(shares, max_nodes = 200L)
  expect_equal(nodes$node_id, nodes2$node_id)
  sp <- build_country_commodity_sparse_matrix(shares, max_nodes = 50L)
  expect_true(sp$n_nodes <= 50L)
  expect_true(isTRUE(sp$capped) || sp$n_eligible_nodes_before_cap <= 50L)
})

test_that("display matrix keeps missing distinct from zero", {
  edges <- data.table::data.table(
    from_node = "A::1", to_node = "B::1", weight = 0.5
  )
  mat <- sparse_edges_to_display_matrix(edges, c("A::1", "B::1"), c("A::1", "B::1"))
  expect_equal(mat["A::1", "B::1"], 0.5)
  expect_true(is.na(mat["A::1", "A::1"]))
  expect_false(identical(mat["B::1", "A::1"], 0))
})

test_that("safe downloads without secrets", {
  det <- make_dependency_fixture()
  built <- construct_dependency_table(det, year_min = 2019, year_max = 2019)
  sp <- build_country_commodity_sparse_matrix(built$shares)
  tmp <- tempfile(fileext = ".csv")
  write_dependency_csv(built$shares, tmp)
  txt <- paste(readLines(tmp), collapse = "\n")
  expect_false(grepl("secret|raw_file|/cache/", txt, ignore.case = TRUE))
  tmp2 <- tempfile(fileext = ".mtx")
  write_dependency_mtx(sp, tmp2)
  mtxt <- paste(readLines(tmp2), collapse = "\n")
  expect_true(grepl("MatrixMarket", mtxt))
  expect_false(grepl("secret|request_id", mtxt, ignore.case = TRUE))
  fn <- dependency_download_filename("dependency_matrix_reporter_supplier", "2024")
  expect_equal(fn, "dependency_matrix_reporter_supplier_2024.csv")
})
