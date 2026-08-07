empty_impact_paths <- function() {
  data.table::data.table(
    scenario_id = character(),
    path_id = character(),
    parent_path_id = character(),
    depth = integer(),
    original_shocked_supplier = character(),
    source_node = character(),
    affected_reporter = character(),
    hs_code = character(),
    baseline_edge_value_usd = numeric(),
    dependency_share = numeric(),
    shock_input_value_usd = numeric(),
    substitution_allocated_usd = numeric(),
    residual_unmet_value_usd = numeric(),
    propagated_value_usd = numeric(),
    cumulative_path_value_usd = numeric(),
    path_status = character(),
    universe_version = character()
  )
}

apply_shock_propagation <- function(substituted,
                                      baseline,
                                      scenario) {
  sc <- normalize_shock_scenario(scenario)
  edges <- data.table::as.data.table(substituted$edges)
  group_summary <- data.table::as.data.table(substituted$group_summary)
  paths <- empty_impact_paths()

  if (identical(sc$propagation_mode, "direct_only") || !nrow(group_summary)) {
    return(list(edges = edges, group_summary = group_summary, paths = paths))
  }

  max_depth <- if (identical(sc$propagation_mode, "first_order")) {
    1L
  } else {
    as.integer(sc$maximum_propagation_steps %||% 1L)
  }
  if (max_depth < 1L) {
    return(list(edges = edges, group_summary = group_summary, paths = paths))
  }

  bl <- data.table::as.data.table(baseline)

  path_rows <- list()
  path_counter <- 0L
  visited_edges <- character()

  queue <- group_summary[residual_unmet_value_usd > 0,
                         .(source_reporter = reporter_iso3, hs_code,
                           residual = residual_unmet_value_usd,
                           depth = 0L,
                           original_shocked_supplier = NA_character_,
                           parent_path_id = NA_character_,
                           cumulative = residual_unmet_value_usd)]

  if (nrow(queue) && nrow(edges)) {
    orig <- edges[is_targeted == TRUE,
                  .(original_shocked_supplier = supplier_iso3[1]),
                  by = .(reporter_iso3, hs_code)]
    queue <- merge(queue, orig,
                   by.x = c("source_reporter", "hs_code"),
                   by.y = c("reporter_iso3", "hs_code"),
                   all.x = TRUE)
    if ("original_shocked_supplier.y" %in% names(queue)) {
      queue[, original_shocked_supplier := original_shocked_supplier.y]
      queue[, original_shocked_supplier.y := NULL]
      if ("original_shocked_supplier.x" %in% names(queue)) {
        queue[, original_shocked_supplier.x := NULL]
      }
    }
  }

  while (nrow(queue)) {
    item <- queue[1]
    queue <- queue[-1]
    depth <- as.integer(item$depth) + 1L
    if (depth > max_depth) next

    upstream <- as.numeric(item$residual) *
      as.numeric(sc$propagation_pass_through) *
      (as.numeric(sc$propagation_decay) ^ (depth - 1L))
    if (!is.finite(upstream) || upstream < as.numeric(sc$minimum_propagated_value_usd)) next

    out_edges <- bl[
      supplier_iso3 == item$source_reporter &
        hs_code == item$hs_code &
        baseline_import_value_usd > 0
    ]
    if (!nrow(out_edges)) next
    out_edges <- out_edges[supplier_share >= as.numeric(sc$minimum_dependency_share) |
                             is.na(as.numeric(sc$minimum_dependency_share))]
    if (!nrow(out_edges)) next

    data.table::setorderv(out_edges, c("baseline_import_value_usd", "reporter_iso3"), c(-1L, 1L))
    tot <- sum(out_edges$baseline_import_value_usd, na.rm = TRUE)
    if (!is.finite(tot) || tot <= 0) next

    for (i in seq_len(nrow(out_edges))) {
      edge_key <- paste(out_edges$supplier_iso3[i], out_edges$reporter_iso3[i],
                        out_edges$hs_code[i], depth, sep = ">")

      base_key <- paste(out_edges$supplier_iso3[i], out_edges$reporter_iso3[i],
                        out_edges$hs_code[i], sep = ">")
      if (base_key %in% visited_edges) next
      visited_edges <- c(visited_edges, base_key)

      share <- out_edges$baseline_import_value_usd[i] / tot
      prop <- upstream * share
      prop <- min(prop, out_edges$baseline_import_value_usd[i])
      if (!is.finite(prop) || prop < as.numeric(sc$minimum_propagated_value_usd)) next

      path_counter <- path_counter + 1L
      pid <- sprintf("p%04d", path_counter)
      path_rows[[length(path_rows) + 1L]] <- data.table::data.table(
        scenario_id = sc$scenario_id,
        path_id = pid,
        parent_path_id = item$parent_path_id %||% NA_character_,
        depth = depth,
        original_shocked_supplier = item$original_shocked_supplier %||% NA_character_,
        source_node = item$source_reporter,
        affected_reporter = out_edges$reporter_iso3[i],
        hs_code = out_edges$hs_code[i],
        baseline_edge_value_usd = out_edges$baseline_import_value_usd[i],
        dependency_share = out_edges$supplier_share[i],
        shock_input_value_usd = upstream,
        substitution_allocated_usd = 0,
        residual_unmet_value_usd = prop,
        propagated_value_usd = prop,
        cumulative_path_value_usd = as.numeric(item$cumulative %||% 0) + prop,
        path_status = "propagated",
        universe_version = sc$universe_version
      )

      edges[
        reporter_iso3 == out_edges$reporter_iso3[i] &
          hs_code == out_edges$hs_code[i] &
          supplier_iso3 == out_edges$supplier_iso3[i],
        `:=`(
          direct_disrupted_value_usd = direct_disrupted_value_usd + prop,
          residual_unmet_value_usd = residual_unmet_value_usd + prop,
          post_shock_supplier_value_usd = pmax(0, post_shock_supplier_value_usd - prop),
          post_shock_observed_import_value_usd =
            pmax(0, post_shock_observed_import_value_usd - prop)
        )
      ]

      if (depth < max_depth && identical(sc$propagation_mode, "multi_step")) {
        queue <- rbind(
          queue,
          data.table::data.table(
            source_reporter = out_edges$reporter_iso3[i],
            hs_code = out_edges$hs_code[i],
            residual = prop,
            depth = depth,
            original_shocked_supplier = item$original_shocked_supplier,
            parent_path_id = pid,
            cumulative = as.numeric(item$cumulative %||% 0) + prop
          ),
          fill = TRUE
        )
      }
    }
  }

  if (length(path_rows)) {
    paths <- data.table::rbindlist(path_rows, fill = TRUE)
  }
  list(edges = edges, group_summary = group_summary, paths = paths)
}
