EXPECTED_UNIVERSE_CHECKSUM <- "uv_262deb46e00d2f216a5a"

prepare_detailed_trade <- function(dt) {
  dt <- data.table::as.data.table(dt)

  if (isTRUE(attr(dt, "gtsc_prepared_detailed"))) {
    return(dt)
  }

  if (!nrow(dt)) return(dt)
  if (!"hs_code" %in% names(dt) && "cmd_code" %in% names(dt)) {
    data.table::setnames(dt, "cmd_code", "hs_code")
  }
  needed <- c(
    "year", "reporter_iso3", "partner_iso3",
    "flow_code", "hs_code", "trade_value_usd"
  )
  missing <- setdiff(needed, names(dt))
  if (length(missing)) {
    stop("Detailed trade missing columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (!"reporter_name" %in% names(dt)) {
    dt[, reporter_name := as.character(reporter_iso3)]
  }
  if (!"partner_name" %in% names(dt)) {
    dt[, partner_name := as.character(partner_iso3)]
  }
  dt[, `:=`(
    year = as.integer(year),
    reporter_iso3 = as.character(reporter_iso3),
    partner_iso3 = as.character(partner_iso3),
    flow_code = as.character(flow_code),
    hs_code = as.character(hs_code),
    trade_value_usd = sanitize_chart_numeric(trade_value_usd)
  )]
  if (!"commodity_description" %in% names(dt)) {
    dt[, commodity_description := NA_character_]
  }
  if (!"flow_name" %in% names(dt)) {
    dt[, flow_name := flow_label(flow_code)]
  }

  pn <- as.character(dt$partner_name)
  pn[is.na(pn)] <- ""
  dt <- dt[
    !is.na(partner_iso3) &
      nchar(partner_iso3) == 3L &
      !(partner_iso3 %in% TF_AGGREGATE_PARTNER_ISO3) &
      !grepl("World|Areas,?\\s*nes|Special Categories|Bunkers|Free Zones",
             pn, ignore.case = TRUE)
  ]
  dt <- dt[
    !is.na(reporter_iso3) &
      nchar(reporter_iso3) == 3L &
      !(reporter_iso3 %in% TF_AGGREGATE_PARTNER_ISO3)
  ]
  data.table::setkeyv(
    dt,
    c(
      "year",
      "reporter_iso3",
      "partner_iso3",
      "flow_code",
      "hs_code"
    )
  )

  attr(dt, "gtsc_prepared_detailed") <- TRUE

  dt
}

universe_iso3_from_snap <- function(universe) {
  if (is.null(universe)) return(character())
  tr <- universe$top_reporters
  if (is.data.frame(tr) || data.table::is.data.table(tr)) {
    return(unique(as.character(tr$reporter_iso3)))
  }
  if (is.list(tr) && length(tr)) {
    return(unique(vapply(tr, function(x) as.character(x$reporter_iso3 %||% NA), character(1))))
  }
  character()
}

partner_iso3_from_universe <- function(universe) {
  if (is.null(universe)) return(character())
  tp <- universe$top_partners
  if (is.data.frame(tp) || data.table::is.data.table(tp)) {
    return(unique(as.character(tp$partner_iso3)))
  }
  character()
}

hs4_from_universe <- function(universe) {
  if (is.null(universe)) return(character())
  th <- universe$top_hs4
  if (is.data.frame(th) || data.table::is.data.table(th)) {
    return(unique(as.character(th$hs_code)))
  }
  character()
}

trade_flow_coverage_status <- function(snap, expected_checksum = EXPECTED_UNIVERSE_CHECKSUM) {
  man <- snap$production_manifest %||% list()
  uni <- snap$analytical_universe %||% list()
  det <- snap$trade_detailed_enriched %||% snap$trade_detailed
  if (!is.null(det) && nrow(det)) det <- prepare_detailed_trade(det)

  selected <- universe_iso3_from_snap(uni)
  if (!length(selected)) {
    selected <- as.character(snap$top_reporters$reporter_iso3 %||% character())
  }
  represented <- if (!is.null(det) && nrow(det)) {
    sort(unique(as.character(det$reporter_iso3)))
  } else {
    character()
  }
  missing_reps <- sort(setdiff(selected, represented))
  checksum <- as.character(
    uni$universe_checksum %||% uni$universe_version %||%
      man$universe_version %||% NA_character_
  )
  status <- as.character(
    man$production_status %||%
      snap$pipeline_status$detailed_trade %||%
      if (length(represented) == 0L) "unavailable" else
        if (length(missing_reps) == 0L && length(selected)) "complete" else "partial"
  )
  n_warn <- 0L
  for (v in list(snap$production_validation, snap$phase3_validation)) {
    if (!is.null(v) && nrow(v)) n_warn <- n_warn + sum(v$status == "warning", na.rm = TRUE)
  }
  ingested <- if (!is.null(det) && nrow(det) && "ingested_at" %in% names(det)) {
    max(as.character(det$ingested_at), na.rm = TRUE)
  } else {
    NA_character_
  }
  years <- if (!is.null(det) && nrow(det)) range(det$year, na.rm = TRUE) else c(NA_integer_, NA_integer_)
  list(
    production_status = status,
    selected_reporter_count = length(selected),
    represented_reporter_count = length(represented),
    missing_reporter_count = length(missing_reps),
    selected_reporters = sort(selected),
    represented_reporters = represented,
    missing_reporters = missing_reps,
    universe_checksum = checksum,
    checksum_stale = !is.na(checksum) && nzchar(checksum) &&
      !identical(checksum, expected_checksum),
    year_range = years,
    latest_ingested_at = ingested,
    validation_warnings = n_warn,
    n_rows = if (!is.null(det)) nrow(det) else 0L,
    request_summary = list(
      planned = man$planned_request_count %||% NA_integer_,
      active = man$active_request_count %||% NA_integer_,
      succeeded = man$succeeded_request_count %||% NA_integer_,
      quota_blocked = man$quota_blocked_count %||% NA_integer_
    )
  )
}

coverage_pending_request_count <- function(coverage) {
  rs <- coverage$request_summary %||% list()
  planned <- suppressWarnings(as.integer(rs$planned %||% NA_integer_)[1])
  active <- suppressWarnings(as.integer(rs$active %||% NA_integer_)[1])
  quota <- suppressWarnings(as.integer(rs$quota_blocked %||% NA_integer_)[1])
  vals <- c(planned, active, quota)
  if (all(is.na(vals))) return(NA_integer_)
  as.integer(sum(vals, na.rm = TRUE))
}

coverage_is_selected_universe_complete <- function(coverage) {
  selected <- suppressWarnings(as.integer(coverage$selected_reporter_count %||% 0L)[1])
  represented <- suppressWarnings(as.integer(coverage$represented_reporter_count %||% 0L)[1])
  missing <- suppressWarnings(as.integer(
    coverage$missing_reporter_count %||% max(0L, selected - represented)
  )[1])
  if (is.na(selected) || selected <= 0L) return(FALSE)
  if (is.na(represented) || represented < selected) return(FALSE)
  if (!is.na(missing) && missing > 0L) return(FALSE)
  status <- as.character(coverage$production_status %||% "")[1]
  pending <- coverage_pending_request_count(coverage)
  if (identical(status, "complete")) return(TRUE)
  if (!is.na(pending) && pending > 0L) return(FALSE)
  isTRUE(represented == selected)
}

selected_universe_disclaimer <- function() {
  paste(
    "This remains a selected analytical universe and is not complete bilateral trade coverage",
    "for every global reporter, partner and HS4 commodity."
  )
}

detailed_coverage_notice <- function(coverage, context = "generic") {
  represented <- suppressWarnings(as.integer(coverage$represented_reporter_count %||% 0L)[1])
  selected <- suppressWarnings(as.integer(coverage$selected_reporter_count %||% 0L)[1])
  if (is.na(represented)) represented <- 0L
  if (is.na(selected)) selected <- 0L
  complete <- coverage_is_selected_universe_complete(coverage)
  disclaimer <- selected_universe_disclaimer()

  if (isTRUE(complete)) {
    lead <- sprintf(
      "Detailed coverage is complete for the selected %d-reporter analytical universe.",
      selected
    )
    domain <- switch(
      as.character(context),
      time_series = paste(
        "Commodity and bilateral analyses use the completed selected-universe detailed observations.",
        "Global and single-economy trends use the completed World-partner country dataset."
      ),
      bilateral = paste(
        "Bilateral explorer results use the completed selected-universe detailed observations.",
        "Totals below are from the selected universe only — not complete global totals."
      ),
      network = "Network measures describe the completed selected-universe detailed trade network.",
      dependency = "Dependency results cover completed selected-universe reported imports.",
      shock = "Shock results cover completed selected-universe detailed import relationships.",
      forecast = paste(
        "Annual detailed relationships used for forecast candidate selection cover the",
        "completed selected-universe reporters."
      ),
      overview = paste(
        "Detailed bilateral data for the selected analytical universe is complete.",
        "Global country-level results on this page use the completed World-partner country dataset."
      ),
      paste(
        "Detailed bilateral analyses use the completed selected-universe observations."
      )
    )
    return(paste(lead, domain, disclaimer))
  }

  partial_lead <- switch(
    as.character(context),
    time_series = sprintf(
      paste(
        "Commodity and bilateral analyses currently use available detailed observations for",
        "%d of %d selected reporting economies."
      ),
      represented, selected
    ),
    bilateral = sprintf(
      "Bilateral explorer currently covers %d of %d selected reporting economies.",
      represented, selected
    ),
    network = sprintf(
      paste(
        "Network measures currently describe the available detailed trade network for",
        "%d of %d selected reporting economies."
      ),
      represented, selected
    ),
    dependency = sprintf(
      paste(
        "Dependency results currently cover available reported imports for",
        "%d of %d selected reporting economies."
      ),
      represented, selected
    ),
    shock = sprintf(
      paste(
        "Shock results currently cover available detailed import relationships for",
        "%d of %d selected reporting economies."
      ),
      represented, selected
    ),
    forecast = sprintf(
      paste(
        "Monthly forecast candidates are derived from available annual detailed import/export",
        "relationships for %d of %d selected reporting economies."
      ),
      represented, selected
    ),
    overview = sprintf(
      paste(
        "Detailed bilateral coverage is partial (%d of %d selected reporting economies).",
        "Global country-level results on this page use the completed global dataset."
      ),
      represented, selected
    ),
    sprintf(
      "Detailed coverage currently includes %d of %d selected reporting economies.",
      represented, selected
    )
  )
  pending_txt <- switch(
    as.character(context),
    network = "Centrality and connectivity results may change when remaining selected-universe Comtrade requests are completed.",
    dependency = "Rankings and concentration measures may change when remaining selected-universe Comtrade requests are completed.",
    shock = "Estimated impacts may change when remaining selected-universe Comtrade requests are completed.",
    forecast = "Rankings and series coverage may change when remaining selected-universe Comtrade requests are completed.",
    bilateral = "Results update automatically as remaining selected-universe Comtrade requests are completed.",
    time_series = "Coverage updates automatically when remaining selected-universe Comtrade requests are completed.",
    "Coverage updates automatically when remaining selected-universe Comtrade requests are completed."
  )
  if (identical(as.character(context), "overview")) {
    return(paste(partial_lead, disclaimer))
  }
  if (identical(as.character(context), "bilateral")) {
    return(paste(
      partial_lead, pending_txt,
      "Totals below are from available detailed observations only — not complete top-20 or global totals.",
      disclaimer
    ))
  }
  if (identical(as.character(context), "time_series")) {
    return(paste(
      partial_lead, pending_txt,
      "Global and single-economy trends use the completed World-partner country dataset.",
      disclaimer
    ))
  }
  paste(partial_lead, pending_txt, disclaimer)
}

trade_flow_filter_choices <- function(dt, universe = NULL) {
  dt <- prepare_detailed_trade(dt)
  cov_selected <- universe_iso3_from_snap(universe)
  represented <- if (nrow(dt)) sort(unique(dt$reporter_iso3)) else character()
  partners <- if (nrow(dt)) {
    unique(dt[, .(partner_iso3, partner_name)])[order(partner_name, partner_iso3)]
  } else {
    data.table::data.table(partner_iso3 = character(), partner_name = character())
  }
  hs <- if (nrow(dt)) {
    unique(dt[, .(hs_code, commodity_description)])[order(hs_code)]
  } else {
    data.table::data.table(hs_code = character(), commodity_description = character())
  }
  years <- if (nrow(dt)) sort(unique(as.integer(dt$year))) else integer()
  list(
    years = years,
    default_year = if (length(years)) max(years) else NA_integer_,
    reporters = represented,
    reporter_labels = {
      if (!nrow(dt)) character()
      else {
        labs <- unique(dt[, .(reporter_iso3, reporter_name)])[order(reporter_name)]
        setNames(labs$reporter_iso3, paste0(labs$reporter_name, " (", labs$reporter_iso3, ")"))
      }
    },
    partners = partners$partner_iso3,
    partner_labels = if (nrow(partners)) {
      setNames(partners$partner_iso3, paste0(partners$partner_name, " (", partners$partner_iso3, ")"))
    } else character(),
    hs_codes = hs$hs_code,
    hs_labels = if (nrow(hs)) {
      desc <- ifelse(is.na(hs$commodity_description) | !nzchar(hs$commodity_description),
                     hs$hs_code,
                     paste0(hs$hs_code, " — ", substr(hs$commodity_description, 1, 60)))
      setNames(hs$hs_code, desc)
    } else character(),
    selected_universe = cov_selected,
    missing_reporters = sort(setdiff(cov_selected, represented))
  )
}

filter_detailed_trade <- function(dt,
                                    year_min = NULL,
                                    year_max = NULL,
                                    reporters = NULL,
                                    partners = NULL,
                                    flows = c("M", "X"),
                                    hs_codes = NULL) {
  out <- prepare_detailed_trade(dt)
  if (!nrow(out)) return(out)
  if (!is.null(year_min) && !is.na(year_min)) out <- out[year >= as.integer(year_min)]
  if (!is.null(year_max) && !is.na(year_max)) out <- out[year <= as.integer(year_max)]
  if (!is.null(reporters) && length(reporters) && !all(reporters %in% c("__ALL__", ""))) {
    reps <- setdiff(as.character(reporters), c("__ALL__", ""))
    if (length(reps)) out <- out[reporter_iso3 %in% reps]
  }
  if (!is.null(partners) && length(partners) && !all(partners %in% c("__ALL__", ""))) {
    pars <- setdiff(as.character(partners), c("__ALL__", ""))
    if (length(pars)) out <- out[partner_iso3 %in% pars]
  }
  flows <- as.character(flows)
  if (length(flows)) out <- out[flow_code %in% flows]
  if (!is.null(hs_codes) && length(hs_codes) && !all(hs_codes %in% c("__ALL__", ""))) {
    hs <- setdiff(as.character(hs_codes), c("__ALL__", ""))
    if (length(hs)) out <- out[hs_code %in% hs]
  }
  out
}

trade_flow_kpis <- function(filtered, sankey_coverage = NULL) {
  dt <- data.table::as.data.table(filtered)
  tot <- if (nrow(dt)) sum(dt$trade_value_usd, na.rm = TRUE) else 0
  imp <- if (nrow(dt)) sum(dt[flow_code == "M"]$trade_value_usd, na.rm = TRUE) else 0
  exp <- if (nrow(dt)) sum(dt[flow_code == "X"]$trade_value_usd, na.rm = TRUE) else 0
  list(
    filtered_trade_value = tot,
    imports_value = imp,
    exports_value = exp,
    n_observations = nrow(dt),
    n_reporters = if (nrow(dt)) data.table::uniqueN(dt$reporter_iso3) else 0L,
    n_partners = if (nrow(dt)) data.table::uniqueN(dt$partner_iso3) else 0L,
    n_hs4 = if (nrow(dt)) data.table::uniqueN(dt$hs_code) else 0L,
    sankey_coverage_pct = sankey_coverage$coverage_pct %||% NA_real_,
    scope_note = "Based on the current detailed bilateral dataset (may be partial)."
  )
}

trade_flow_path_aggregates <- function(filtered, grouping = "reporter_partner_commodity") {
  dt <- data.table::as.data.table(filtered)
  if (!nrow(dt)) {
    return(data.table::data.table(
      reporter_iso3 = character(), partner_iso3 = character(), hs_code = character(),
      flow_code = character(), value = numeric(),
      reporter_name = character(), partner_name = character(),
      commodity_description = character()
    ))
  }
  keys <- switch(
    as.character(grouping),
    "reporter_partner" = c("reporter_iso3", "partner_iso3", "flow_code"),
    "reporter_commodity" = c("reporter_iso3", "hs_code", "flow_code"),
    "reporter_commodity_partner" = c("reporter_iso3", "hs_code", "partner_iso3", "flow_code"),
    c("reporter_iso3", "partner_iso3", "hs_code", "flow_code")
  )
  agg <- dt[, .(
    value = sum(trade_value_usd, na.rm = TRUE),
    reporter_name = reporter_name[1],
    partner_name = partner_name[1],
    commodity_description = commodity_description[1]
  ), by = keys]

  for (col in c("partner_iso3", "hs_code")) {
    if (!col %in% names(agg)) agg[, (col) := NA_character_]
  }
  data.table::setorderv(agg, c("value", "reporter_iso3", "partner_iso3", "hs_code", "flow_code"),
                        order = c(-1L, 1L, 1L, 1L, 1L))
  agg
}

select_top_n_paths <- function(paths, top_n = 20L, min_value = NULL) {
  paths <- data.table::as.data.table(paths)
  total <- if (nrow(paths)) sum(paths$value, na.rm = TRUE) else 0
  if (!nrow(paths) || total <= 0) {
    return(list(
      visible = paths[0],
      other_value = 0,
      total_value = total,
      coverage_pct = NA_real_,
      n_visible = 0L,
      n_total = 0L
    ))
  }
  keep <- paths
  if (!is.null(min_value) && is.finite(min_value) && min_value > 0) {
    keep <- keep[value >= as.numeric(min_value)]
  }
  top_n <- as.integer(top_n)
  if (!is.na(top_n) && top_n > 0L && nrow(keep) > top_n) {
    keep <- keep[seq_len(top_n)]
  }
  vis <- sum(keep$value, na.rm = TRUE)
  other <- total - vis
  list(
    visible = keep,
    other_value = other,
    total_value = total,
    coverage_pct = if (total > 0) 100 * vis / total else NA_real_,
    n_visible = nrow(keep),
    n_total = nrow(paths)
  )
}

sankey_node_id <- function(role, code, flow = NULL) {
  role <- as.character(role)
  code <- as.character(code)
  if (!is.null(flow) && !is.na(flow) && nzchar(flow)) {
    sprintf("%s_%s__%s", role, code, flow)
  } else {
    sprintf("%s_%s", role, code)
  }
}

build_sankey_data <- function(visible_paths,
                                grouping = "reporter_partner_commodity",
                                include_other = FALSE,
                                other_value = 0,
                                both_flows = FALSE) {
  paths <- data.table::as.data.table(visible_paths)
  if (!nrow(paths) && !(isTRUE(include_other) && other_value > 0)) {
    return(list(
      nodes = data.frame(name = character(), role = character(), stringsAsFactors = FALSE),
      links = data.frame(source = integer(), target = integer(), value = numeric(),
                        flow_code = character(), stringsAsFactors = FALSE),
      visible_value = 0,
      edge_table = data.table::data.table()
    ))
  }

  edges <- list()
  add_edge <- function(src_role, src, tgt_role, tgt, value, flow, src_lab, tgt_lab) {
    edges[[length(edges) + 1L]] <<- data.table::data.table(
      source_id = sankey_node_id(src_role, src, if (both_flows) flow else NULL),
      target_id = sankey_node_id(tgt_role, tgt, NULL),
      source_label = src_lab,
      target_label = tgt_lab,
      value = as.numeric(value),
      flow_code = as.character(flow),
      source_role = src_role,
      target_role = tgt_role
    )
  }

  if (nrow(paths)) {
    for (i in seq_len(nrow(paths))) {
      r <- paths[i]
      flow <- r$flow_code
      rlab <- paste0(r$reporter_name %||% r$reporter_iso3, " [reporter]")
      if (isTRUE(both_flows)) {
        rlab <- paste0(rlab, " · ", flow_label(flow))
      }
      plab <- paste0(r$partner_name %||% r$partner_iso3, " [partner]")
      clab <- paste0(r$hs_code, " [HS4]")
      if (!is.na(r$commodity_description) && nzchar(r$commodity_description)) {
        clab <- paste0(r$hs_code, " — ", substr(r$commodity_description, 1, 40), " [HS4]")
      }
      g <- as.character(grouping)
      if (identical(g, "reporter_partner")) {
        add_edge("reporter", r$reporter_iso3, "partner", r$partner_iso3, r$value, flow, rlab, plab)
      } else if (identical(g, "reporter_commodity")) {
        add_edge("reporter", r$reporter_iso3, "commodity", r$hs_code, r$value, flow, rlab, clab)
      } else if (identical(g, "reporter_commodity_partner")) {
        add_edge("reporter", r$reporter_iso3, "commodity", r$hs_code, r$value, flow, rlab, clab)
        add_edge("commodity", r$hs_code, "partner", r$partner_iso3, r$value, flow, clab, plab)
      } else {

        add_edge("reporter", r$reporter_iso3, "partner", r$partner_iso3, r$value, flow, rlab, plab)
        add_edge("partner", r$partner_iso3, "commodity", r$hs_code, r$value, flow, plab, clab)
      }
    }
  }

  if (isTRUE(include_other) && is.finite(other_value) && other_value > 0) {
    add_edge("reporter", "OTHER", "other", "OTHER", other_value, "X",
             "Other (excluded by top-N) [reporter]", "Other [remainder]")
  }

  edge_dt <- data.table::rbindlist(edges, fill = TRUE)
  edge_dt <- edge_dt[, .(
    value = sum(value, na.rm = TRUE),
    source_label = source_label[1],
    target_label = target_label[1],
    flow_code = flow_code[1],
    source_role = source_role[1],
    target_role = target_role[1]
  ), by = .(source_id, target_id)]
  edge_dt <- edge_dt[value > 0 & is.finite(value)]

  node_ids <- unique(c(edge_dt$source_id, edge_dt$target_id))
  labels <- character(length(node_ids))
  roles <- character(length(node_ids))
  names(labels) <- node_ids
  names(roles) <- node_ids
  for (i in seq_len(nrow(edge_dt))) {
    labels[[edge_dt$source_id[i]]] <- edge_dt$source_label[i]
    labels[[edge_dt$target_id[i]]] <- edge_dt$target_label[i]
    roles[[edge_dt$source_id[i]]] <- edge_dt$source_role[i]
    roles[[edge_dt$target_id[i]]] <- edge_dt$target_role[i]
  }
  nodes <- data.frame(
    name = unname(labels[node_ids]),
    id = node_ids,
    role = unname(roles[node_ids]),
    stringsAsFactors = FALSE
  )
  idx <- setNames(seq_len(nrow(nodes)) - 1L, nodes$id)
  links <- data.frame(
    source = as.integer(idx[edge_dt$source_id]),
    target = as.integer(idx[edge_dt$target_id]),
    value = as.numeric(edge_dt$value),
    flow_code = as.character(edge_dt$flow_code),
    stringsAsFactors = FALSE
  )

  ok <- !is.na(links$source) & !is.na(links$target) & links$value >= 0
  links <- links[ok, , drop = FALSE]
  list(
    nodes = nodes,
    links = links,
    visible_value = sum(paths$value, na.rm = TRUE),
    edge_table = edge_dt
  )
}

prepare_trade_flow_timeseries <- function(filtered,
                                            max_series = 6L,
                                            mode = "auto") {
  dt <- data.table::as.data.table(filtered)
  if (!nrow(dt)) {
    return(list(
      data = data.table::data.table(),
      series_col = "series",
      remainder_value = 0,
      note = "No observations for the current filters."
    ))
  }
  n_rep <- data.table::uniqueN(dt$reporter_iso3)
  n_par <- data.table::uniqueN(dt$partner_iso3)
  n_hs <- data.table::uniqueN(dt$hs_code)

  if (identical(mode, "auto")) {
    if (n_rep == 1L && n_par == 1L) {
      mode <- "flows"
    } else if (n_rep == 1L && n_par > 1L) {
      mode <- "top_partners"
    } else if (n_rep == 1L && n_hs > 1L) {
      mode <- "top_commodities"
    } else {
      mode <- "flows"
    }
  }

  if (identical(mode, "flows")) {
    ser <- dt[, .(value = sum(trade_value_usd, na.rm = TRUE)), by = .(year, flow_code)]
    ser[, series := flow_label(flow_code)]
  } else if (identical(mode, "top_partners")) {
    ranks <- dt[, .(tot = sum(trade_value_usd, na.rm = TRUE)), by = partner_iso3]
    data.table::setorderv(ranks, c("tot", "partner_iso3"), c(-1L, 1L))
    top <- ranks[seq_len(min(as.integer(max_series), nrow(ranks)))]$partner_iso3
    dt2 <- dt[partner_iso3 %in% top]
    ser <- dt2[, .(value = sum(trade_value_usd, na.rm = TRUE)),
               by = .(year, series = partner_iso3)]
  } else {
    ranks <- dt[, .(tot = sum(trade_value_usd, na.rm = TRUE)), by = hs_code]
    data.table::setorderv(ranks, c("tot", "hs_code"), c(-1L, 1L))
    top <- ranks[seq_len(min(as.integer(max_series), nrow(ranks)))]$hs_code
    dt2 <- dt[hs_code %in% top]
    ser <- dt2[, .(value = sum(trade_value_usd, na.rm = TRUE)),
               by = .(year, series = hs_code)]
  }
  ser[, value := sanitize_chart_numeric(value)]
  data.table::setorderv(ser, c("series", "year"))
  total_all <- sum(dt$trade_value_usd, na.rm = TRUE)
  rem <- total_all - sum(ser$value, na.rm = TRUE)
  list(
    data = ser,
    series_col = "series",
    mode = mode,
    remainder_value = max(0, rem),
    note = sprintf("Time series mode: %s. Values are current US dollars; missing years are not interpolated.", mode)
  )
}

prepare_commodity_composition <- function(filtered, top_n = 12L, include_other = TRUE) {
  dt <- data.table::as.data.table(filtered)
  if (!nrow(dt)) {
    return(list(
      data = data.table::data.table(),
      total = 0,
      other_value = 0,
      coverage_pct = NA_real_
    ))
  }
  agg <- dt[, .(
    value = sum(trade_value_usd, na.rm = TRUE),
    commodity_description = commodity_description[1]
  ), by = hs_code]
  data.table::setorderv(agg, c("value", "hs_code"), c(-1L, 1L))
  total <- sum(agg$value, na.rm = TRUE)
  top_n <- as.integer(top_n)
  if (nrow(agg) > top_n) {
    keep <- agg[seq_len(top_n)]
    other <- sum(agg$value[seq.int(top_n + 1L, nrow(agg))], na.rm = TRUE)
    if (isTRUE(include_other) && other > 0) {
      keep <- rbind(
        keep,
        data.table::data.table(
          hs_code = "OTHER",
          value = other,
          commodity_description = "Other HS4 (outside top-N)"
        ),
        fill = TRUE
      )
    }
  } else {
    keep <- agg
    other <- 0
  }
  keep[, share_pct := if (total > 0) 100 * value / total else NA_real_]
  list(
    data = keep,
    total = total,
    other_value = other,
    coverage_pct = if (total > 0) 100 * (total - other) / total else NA_real_
  )
}

prepare_reporter_partner_matrix <- function(filtered, measure = "trade_value") {
  dt <- data.table::as.data.table(filtered)
  empty <- list(
    long = data.table::data.table(),
    wide = NULL,
    reporters = character(),
    partners = character(),
    measure = measure
  )
  if (!nrow(dt)) return(empty)

  if (identical(measure, "imports")) {
    base <- dt[flow_code == "M"]
    val_expr <- quote(sum(trade_value_usd, na.rm = TRUE))
  } else if (identical(measure, "exports")) {
    base <- dt[flow_code == "X"]
    val_expr <- quote(sum(trade_value_usd, na.rm = TRUE))
  } else if (identical(measure, "observation_count")) {
    base <- dt
    val_expr <- quote(.N)
  } else {
    base <- dt
    val_expr <- quote(sum(trade_value_usd, na.rm = TRUE))
  }
  if (!nrow(base)) return(empty)

  long <- base[, .(
    value = eval(val_expr),
    observed = TRUE
  ), by = .(reporter_iso3, partner_iso3)]

  reps <- sort(unique(dt$reporter_iso3))
  pars <- sort(unique(dt$partner_iso3))
  grid <- data.table::CJ(reporter_iso3 = reps, partner_iso3 = pars, unique = TRUE)
  long <- long[grid, on = .(reporter_iso3, partner_iso3)]
  long[is.na(observed), observed := FALSE]

  long[observed == FALSE, value := NA_real_]
  long[, value := sanitize_chart_numeric(value)]

  r_ord <- long[!is.na(value), .(tot = sum(value, na.rm = TRUE)), by = reporter_iso3]
  data.table::setorderv(r_ord, c("tot", "reporter_iso3"), c(-1L, 1L))
  p_ord <- long[!is.na(value), .(tot = sum(value, na.rm = TRUE)), by = partner_iso3]
  data.table::setorderv(p_ord, c("tot", "partner_iso3"), c(-1L, 1L))

  list(
    long = long,
    reporters = r_ord$reporter_iso3,
    partners = p_ord$partner_iso3,
    measure = measure,
    observed_total = sum(long$value, na.rm = TRUE)
  )
}

download_safe_columns <- function(dt) {
  dt <- data.table::as.data.table(dt)
  drop <- grep("path|url|header|secret|token|key|raw_file|cache", names(dt),
               ignore.case = TRUE, value = TRUE)
  if (length(drop)) dt[, (drop) := NULL]
  dt
}
