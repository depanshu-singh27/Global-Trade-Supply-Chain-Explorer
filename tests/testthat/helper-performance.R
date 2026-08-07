make_tiny_perf_cfg <- function(...) {
  cfg <- normalise_performance_config(list(
    benchmark_mode = "smoke",
    iterations = 2L,
    warmup_iterations = 1L,
    synthetic_seed = 42L,
    synthetic_reporters = 5L,
    synthetic_partners = 5L,
    synthetic_hs4 = 5L,
    synthetic_years = 2L,
    synthetic_active_nodes = 50L,
    synthetic_edge_density = 0.4,
    phase_label = "test",
    output_root = "data/performance"
  ))
  dots <- list(...)
  for (nm in names(dots)) cfg[[nm]] <- dots[[nm]]
  cfg
}
