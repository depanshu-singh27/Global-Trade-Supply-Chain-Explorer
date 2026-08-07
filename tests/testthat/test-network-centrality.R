test_that("strength, degree and finite PageRank/betweenness", {
  det <- make_network_detailed_fixture()
  net <- build_full_trade_network(
    det, mode = "exports", year_min = 2019, year_max = 2021, top_n = 50L,
    represented_reporters = c("DEU", "IND", "KOR"),
    selected_reporters = c("DEU", "IND", "KOR")
  )
  expect_true(nrow(net$nodes) > 0)
  expect_true(all(is.finite(net$nodes$total_strength) | is.na(net$nodes$total_strength)))
  expect_false(any(is.infinite(net$nodes$pagerank), na.rm = TRUE))
  expect_false(any(is.nan(net$nodes$pagerank), na.rm = TRUE))
  expect_false(any(is.infinite(net$nodes$betweenness), na.rm = TRUE))
  expect_false(any(is.nan(net$nodes$betweenness), na.rm = TRUE))

  tot_edge <- sum(net$edges$trade_value_usd, na.rm = TRUE)
  expect_equal(sum(net$nodes$out_strength, na.rm = TRUE), tot_edge, tolerance = 1e-6)
  expect_equal(sum(net$nodes$in_strength, na.rm = TRUE), tot_edge, tolerance = 1e-6)
  expect_true(all(net$nodes$degree >= 0))
  expect_true(all(net$nodes$in_degree >= 0))
  expect_true(all(net$nodes$out_degree >= 0))
})

test_that("weighted distance transformation is positive and finite for valid weights", {
  d <- trade_value_to_distance(c(100, 50, 25, 0, NA, -1))
  expect_true(all(d[1:3] > 0 & is.finite(d[1:3])))
  expect_true(all(is.na(d[4:6])))
})

test_that("components, density, reciprocity and concentration", {
  det <- make_network_detailed_fixture()
  net <- build_full_trade_network(
    det, mode = "exports", year_min = 2019, year_max = 2021, top_n = 50L,
    represented_reporters = c("DEU", "IND", "KOR")
  )
  s <- net$stats
  expect_true(is.finite(s$directed_density) || is.na(s$directed_density))
  expect_true(s$weak_component_count >= 1L)
  expect_true(is.finite(s$top5_edge_share_pct))
  expect_true(is.finite(s$top5_node_strength_share_pct))
  expect_true(is.finite(s$edge_hhi))
  expect_true(s$edge_hhi > 0 && s$edge_hhi <= 1)
  expect_true(is.finite(s$reciprocity) || is.na(s$reciprocity))
})

test_that("community assignment or safe unavailable", {
  det <- make_network_detailed_fixture()
  net <- build_full_trade_network(
    det, mode = "exports", year_min = 2019, year_max = 2021, top_n = 50L,
    represented_reporters = c("DEU", "IND", "KOR")
  )
  expect_true("community" %in% names(net$nodes))

  expect_false(any(is.infinite(net$nodes$community), na.rm = TRUE))
})

test_that("deterministic layout seed", {
  det <- make_network_detailed_fixture()
  net1 <- build_full_trade_network(det, mode = "exports", top_n = 50L, layout = "fr",
                                   represented_reporters = c("DEU", "IND"))
  net2 <- build_full_trade_network(det, mode = "exports", top_n = 50L, layout = "fr",
                                   represented_reporters = c("DEU", "IND"))
  expect_equal(net1$nodes$x, net2$nodes$x)
  expect_equal(net1$nodes$y, net2$nodes$y)
})

test_that("ego-network one and two step extraction", {
  det <- make_network_detailed_fixture()
  full <- build_full_trade_network(det, mode = "exports", top_n = 50L,
                                   represented_reporters = c("DEU", "IND", "KOR"))
  ego1 <- build_full_trade_network(det, mode = "exports", top_n = 50L, focus_iso3 = "DEU",
                                   ego_order = 1L, represented_reporters = c("DEU", "IND", "KOR"))
  ego2 <- build_full_trade_network(det, mode = "exports", top_n = 50L, focus_iso3 = "DEU",
                                   ego_order = 2L, represented_reporters = c("DEU", "IND", "KOR"))
  expect_true("DEU" %in% ego1$nodes$iso3)
  expect_true(nrow(ego1$nodes) <= nrow(full$nodes))
  expect_true(nrow(ego2$nodes) >= nrow(ego1$nodes))
})

test_that("selected-node profile and corridor rankings", {
  det <- make_network_detailed_fixture()
  net <- build_full_trade_network(det, mode = "exports", top_n = 50L,
                                  represented_reporters = c("DEU", "IND", "KOR"))
  prof <- selected_node_profile(net$nodes, net$edges, "DEU", detailed = det)
  expect_equal(prof$iso3, "DEU")
  expect_true(is.data.frame(prof$outbound) || data.table::is.data.table(prof$outbound))
  corridors <- rank_network_corridors(net$edges, top_n = 5L)
  expect_true(nrow(corridors) <= 5L)
  expect_true(all(diff(corridors$trade_value_usd) <= 0))
})

test_that("centrality ranking deterministic ties", {
  nodes <- data.table::data.table(
    iso3 = c("BBB", "AAA", "CCC"),
    display_name = c("B", "A", "C"),
    reporting_status = "partner_only",
    total_strength = c(10, 10, 5),
    pagerank = c(0.2, 0.2, 0.1)
  )
  r <- rank_network_nodes(nodes, "total_strength", top_n = 3L)
  expect_equal(r$iso3[1:2], c("AAA", "BBB"))
})

test_that("downloads and GraphML contain no secrets or paths", {
  det <- make_network_detailed_fixture()
  net <- build_full_trade_network(det, mode = "exports", top_n = 50L,
                                  represented_reporters = c("DEU", "IND"))
  tmp_csv <- tempfile(fileext = ".csv")
  write_network_csv(net$nodes, tmp_csv)
  txt <- paste(readLines(tmp_csv), collapse = "\n")
  expect_false(grepl("secret|raw_file|/cache/", txt, ignore.case = TRUE))

  tmp_g <- tempfile(fileext = ".graphml")
  write_network_graphml(net$graph, tmp_g)
  gtxt <- paste(readLines(tmp_g), collapse = "\n")
  expect_false(grepl("secret|raw_file|/cache/|request_id", gtxt, ignore.case = TRUE))
  expect_true(grepl("graphml", gtxt, ignore.case = TRUE))

  fn <- network_download_filename("trade_network_nodes", "exports", "2024")
  expect_equal(fn, "trade_network_nodes_exports_2024.csv")
})

test_that("partial and future complete status reading", {
  snap_p <- make_network_snap_fixture("partial")
  expect_equal(snap_p$detailed_coverage$production_status, "partial")
  expect_equal(snap_p$detailed_coverage$represented_reporter_count, 3L)
  snap_c <- make_network_snap_fixture("complete")
  expect_equal(snap_c$detailed_coverage$production_status, "complete")
  stale <- trade_flow_coverage_status(snap_p, expected_checksum = "uv_other")
  expect_true(isTRUE(stale$checksum_stale))
})

test_that("empty network and accessibility summary", {
  empty <- data.table::data.table(
    year = integer(), reporter_iso3 = character(), reporter_name = character(),
    partner_iso3 = character(), partner_name = character(), flow_code = character(),
    hs_code = character(), trade_value_usd = numeric()
  )
  net <- build_full_trade_network(empty, mode = "exports", top_n = 10L)
  expect_equal(net$built$visible_n, 0L)
  summary <- network_accessibility_summary(net$built, net$stats, net$nodes, "exports")
  expect_true(is.character(summary) && nzchar(summary))
})

test_that("source row-count preservation through construction", {
  det <- make_network_detailed_fixture()
  n0 <- nrow(det)
  built <- construct_network_edges(det, mode = "exports", top_n = 100L)
  expect_equal(nrow(det), n0)
  expect_true(built$source_rows <= n0)
})
