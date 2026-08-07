identify_substitute_suppliers <- function(edges, reporter_iso3, hs_code, shocked_suppliers) {
  dt <- data.table::as.data.table(edges)
  dt[
    reporter_iso3 == reporter_iso3 &
      hs_code == hs_code &
      !(supplier_iso3 %in% shocked_suppliers) &
      baseline_import_value_usd > 0
  ]
}

allocate_proportional_substitution <- function(disrupted_value, substitutes) {
  sub <- data.table::as.data.table(substitutes)
  if (!is.finite(disrupted_value) || disrupted_value <= 0 || !nrow(sub)) {
    return(data.table::data.table(
      supplier_iso3 = character(), allocated_usd = numeric(), weight = numeric()
    ))
  }
  data.table::setorderv(sub, c("baseline_import_value_usd", "supplier_iso3"), c(-1L, 1L))
  tot <- sum(sub$baseline_import_value_usd, na.rm = TRUE)
  if (!is.finite(tot) || tot <= 0) {
    return(data.table::data.table(
      supplier_iso3 = character(), allocated_usd = numeric(), weight = numeric()
    ))
  }
  sub[, weight := baseline_import_value_usd / tot]
  sub[, allocated_usd := disrupted_value * weight]

  residual <- disrupted_value - sum(sub$allocated_usd)
  if (is.finite(residual) && abs(residual) > 0) {
    sub$allocated_usd[1] <- sub$allocated_usd[1] + residual
  }
  sub[, .(supplier_iso3, allocated_usd, weight)]
}

allocate_capacity_constrained_substitution <- function(disrupted_value,
                                                         substitutes,
                                                         capacity_pct,
                                                         max_share = 1,
                                                         group_post_total = NULL) {
  sub <- data.table::as.data.table(substitutes)
  if (!is.finite(disrupted_value) || disrupted_value <= 0 || !nrow(sub)) {
    return(list(
      allocation = data.table::data.table(
        supplier_iso3 = character(), allocated_usd = numeric(),
        capacity_usd = numeric(), weight = numeric()
      ),
      allocated_total = 0,
      unused_capacity = 0
    ))
  }
  data.table::setorderv(sub, c("baseline_import_value_usd", "supplier_iso3"), c(-1L, 1L))
  cap_mult <- as.numeric(capacity_pct %||% 0) / 100
  sub[, capacity_usd := pmax(0, baseline_import_value_usd * cap_mult)]
  tot_cap <- sum(sub$capacity_usd, na.rm = TRUE)
  request <- min(disrupted_value, tot_cap)
  if (!is.finite(request) || request <= 0) {
    return(list(
      allocation = sub[, .(supplier_iso3, allocated_usd = 0, capacity_usd, weight = 0)],
      allocated_total = 0,
      unused_capacity = tot_cap
    ))
  }

  tot_w <- sum(sub$capacity_usd, na.rm = TRUE)
  sub[, weight := if (tot_w > 0) capacity_usd / tot_w else 0]
  sub[, allocated_usd := pmin(capacity_usd, request * weight)]

  remaining <- request - sum(sub$allocated_usd)
  if (is.finite(remaining) && remaining > SHOCK_VALUE_TOLERANCE) {
    for (i in seq_len(nrow(sub))) {
      room <- sub$capacity_usd[i] - sub$allocated_usd[i]
      if (room > 0 && remaining > 0) {
        take <- min(room, remaining)
        sub$allocated_usd[i] <- sub$allocated_usd[i] + take
        remaining <- remaining - take
      }
    }
  }

  max_share <- as.numeric(max_share %||% 1)
  if (is.finite(max_share) && max_share < 1) {
    post_base <- sum(sub$baseline_import_value_usd, na.rm = TRUE)

    approx_total <- if (is.finite(group_post_total %||% NA_real_)) {
      as.numeric(group_post_total) + sum(sub$allocated_usd)
    } else {
      post_base + sum(sub$allocated_usd)
    }
    if (is.finite(approx_total) && approx_total > 0) {
      for (i in seq_len(nrow(sub))) {
        max_val <- max_share * approx_total
        cur <- sub$baseline_import_value_usd[i] + sub$allocated_usd[i]
        if (cur > max_val + SHOCK_VALUE_TOLERANCE) {
          allowed_alloc <- max(0, max_val - sub$baseline_import_value_usd[i])
          sub$allocated_usd[i] <- min(sub$allocated_usd[i], allowed_alloc)
        }
      }
    }
  }
  allocated_total <- sum(sub$allocated_usd, na.rm = TRUE)
  list(
    allocation = sub[, .(supplier_iso3, allocated_usd, capacity_usd, weight)],
    allocated_total = allocated_total,
    unused_capacity = max(0, tot_cap - allocated_total)
  )
}

apply_substitution <- function(direct_result, scenario) {
  sc <- normalize_shock_scenario(scenario)
  edges <- data.table::copy(data.table::as.data.table(direct_result$edges))
  if (!nrow(edges)) {
    edges[, `:=`(
      substitution_allocated_usd = numeric(),
      residual_unmet_value_usd = numeric(),
      substitution_rate = numeric(),
      post_shock_observed_import_value_usd = numeric()
    )]
    return(list(edges = edges, group_summary = data.table::data.table()))
  }
  edges[, `:=`(
    substitution_received_usd = 0,
    substitution_allocated_usd = 0,
    residual_unmet_value_usd = 0,
    substitution_rate = NA_real_
  )]

  groups <- edges[direct_disrupted_value_usd > 0,
                  .(direct_disrupted_value_usd = sum(direct_disrupted_value_usd, na.rm = TRUE),
                    shocked_suppliers = list(unique(supplier_iso3))),
                  by = .(reporter_iso3, hs_code)]
  if (!nrow(groups)) {
    edges[, residual_unmet_value_usd := 0]
    edges[, post_shock_observed_import_value_usd := post_shock_supplier_value_usd]
    return(list(edges = edges, group_summary = data.table::data.table()))
  }

  summaries <- list()
  for (gi in seq_len(nrow(groups))) {
    rep <- groups$reporter_iso3[gi]
    hs <- groups$hs_code[gi]
    disrupted <- groups$direct_disrupted_value_usd[gi]
    shocked <- groups$shocked_suppliers[[gi]]
    subs <- edges[
      reporter_iso3 == rep & hs_code == hs &
        !(supplier_iso3 %in% shocked) &
        baseline_import_value_usd > 0
    ]
    allocated_total <- 0
    unused_cap <- 0
    if (identical(sc$substitution_mode, "none")) {
      allocated_total <- 0
    } else if (identical(sc$substitution_mode, "proportional") ||
               identical(sc$substitution_mode, "diversification")) {

      if (identical(sc$substitution_mode, "diversification") && nrow(subs)) {
        subs2 <- data.table::copy(subs)
        data.table::setorderv(subs2, c("supplier_share", "supplier_iso3"), c(1L, 1L))
        inv <- 1 / pmax(subs2$supplier_share, 1e-12)
        inv[!is.finite(inv)] <- 0
        wtot <- sum(inv)
        if (wtot > 0) {
          alloc <- data.table::data.table(
            supplier_iso3 = subs2$supplier_iso3,
            allocated_usd = disrupted * (inv / wtot),
            weight = inv / wtot
          )
        } else {
          alloc <- allocate_proportional_substitution(disrupted, subs)
        }
      } else {
        alloc <- allocate_proportional_substitution(disrupted, subs)
      }
      allocated_total <- sum(alloc$allocated_usd, na.rm = TRUE)
      if (nrow(alloc)) {
        for (j in seq_len(nrow(alloc))) {
          edges[
            reporter_iso3 == rep & hs_code == hs & supplier_iso3 == alloc$supplier_iso3[j],
            substitution_received_usd := substitution_received_usd + alloc$allocated_usd[j]
          ]
        }
      }
    } else {

      post_remaining <- sum(
        edges[reporter_iso3 == rep & hs_code == hs]$post_shock_supplier_value_usd,
        na.rm = TRUE
      )
      cap <- allocate_capacity_constrained_substitution(
        disrupted, subs,
        capacity_pct = sc$substitution_capacity_pct,
        max_share = sc$maximum_substitute_supplier_share,
        group_post_total = post_remaining
      )
      allocated_total <- cap$allocated_total
      unused_cap <- cap$unused_capacity
      if (nrow(cap$allocation)) {
        for (j in seq_len(nrow(cap$allocation))) {
          edges[
            reporter_iso3 == rep & hs_code == hs &
              supplier_iso3 == cap$allocation$supplier_iso3[j],
            substitution_received_usd :=
              substitution_received_usd + cap$allocation$allocated_usd[j]
          ]
        }
      }
    }
    residual <- max(0, disrupted - allocated_total)

    tgt <- edges[reporter_iso3 == rep & hs_code == hs & direct_disrupted_value_usd > 0]
    if (nrow(tgt)) {
      w <- tgt$direct_disrupted_value_usd / sum(tgt$direct_disrupted_value_usd)
      edges[
        reporter_iso3 == rep & hs_code == hs & direct_disrupted_value_usd > 0,
        `:=`(
          substitution_allocated_usd = allocated_total * w,
          residual_unmet_value_usd = residual * w
        )
      ]
    }
    summaries[[gi]] <- data.table::data.table(
      reporter_iso3 = rep,
      hs_code = hs,
      direct_disrupted_value_usd = disrupted,
      substitution_allocated_value_usd = allocated_total,
      residual_unmet_value_usd = residual,
      unused_substitution_capacity_usd = unused_cap,
      substitution_rate = if (disrupted > 0) allocated_total / disrupted else NA_real_
    )
  }

  edges[, post_shock_observed_import_value_usd := sanitize_chart_numeric(
    post_shock_supplier_value_usd + substitution_received_usd
  )]
  edges[, substitution_rate := data.table::fifelse(
    direct_disrupted_value_usd > 0,
    substitution_allocated_usd / direct_disrupted_value_usd,
    NA_real_
  )]

  for (col in c(
    "direct_disrupted_value_usd", "substitution_allocated_usd",
    "residual_unmet_value_usd", "substitution_received_usd",
    "post_shock_observed_import_value_usd"
  )) {
    edges[get(col) < 0, (col) := 0]
  }
  list(
    edges = edges,
    group_summary = data.table::rbindlist(summaries, fill = TRUE)
  )
}
