MAP_GEOMETRY_FILENAME <- "ne_countries_simplified.rds"
MAP_CROSSWALK_FILENAME <- "geographic_crosswalk.parquet"
MAP_GEOMETRY_PREP_VERSION <- 2L

AGGREGATE_ISO3_MAP <- c("EUR", "WLD", "W00", "ASE", "S19", "X99")

prepare_leaflet_geometry <- function(geometry_sf, iso_col = "map_iso3") {
  if (!inherits(geometry_sf, "sf")) {
    stop("Map geometry must inherit from sf.", call. = FALSE)
  }
  if (!iso_col %in% names(geometry_sf)) {
    stop("Map geometry is missing ISO3 column: ", iso_col, call. = FALSE)
  }
  if (is.na(sf::st_crs(geometry_sf))) {
    stop("Map geometry must have a defined CRS.", call. = FALSE)
  }

  geometry_sf <- geometry_sf[
    !is.na(geometry_sf[[iso_col]]) & nzchar(as.character(geometry_sf[[iso_col]])),
    ,
    drop = FALSE
  ]
  geometry_sf <- sf::st_make_valid(geometry_sf)
  geometry_sf <- sf::st_transform(geometry_sf, 4326)
  geometry_sf <- sf::st_wrap_dateline(
    geometry_sf,
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
    quiet = TRUE
  )
  if (any(sf::st_geometry_type(geometry_sf) == "GEOMETRYCOLLECTION")) {
    geometry_sf <- sf::st_collection_extract(geometry_sf, "POLYGON")
  }
  geometry_sf <- sf::st_cast(geometry_sf, "MULTIPOLYGON", warn = FALSE)

  iso <- as.character(geometry_sf[[iso_col]])
  if (anyDuplicated(iso)) {
    groups <- split(seq_len(nrow(geometry_sf)), iso)
    geometry_sf <- do.call(rbind, lapply(groups, function(idx) {
      attrs <- sf::st_drop_geometry(geometry_sf[idx[1L], , drop = FALSE])
      geom <- sf::st_union(sf::st_geometry(geometry_sf)[idx])
      sf::st_sf(attrs, geometry = geom)
    }))
    rownames(geometry_sf) <- NULL
    geometry_sf <- sf::st_make_valid(geometry_sf)
    geometry_sf <- sf::st_wrap_dateline(
      geometry_sf,
      options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
      quiet = TRUE
    )
    if (any(sf::st_geometry_type(geometry_sf) == "GEOMETRYCOLLECTION")) {
      geometry_sf <- sf::st_collection_extract(geometry_sf, "POLYGON")
    }
    geometry_sf <- sf::st_cast(geometry_sf, "MULTIPOLYGON", warn = FALSE)
  }

  attr(geometry_sf, "leaflet_geometry_prepared") <- TRUE
  geometry_sf
}

leaflet_geometry_diagnostics <- function(geometry_sf, iso_col = "map_iso3") {
  if (!inherits(geometry_sf, "sf") || !iso_col %in% names(geometry_sf)) {
    stop("Valid sf geometry and ISO3 column are required.", call. = FALSE)
  }
  rows <- lapply(seq_len(nrow(geometry_sf)), function(i) {
    coords <- sf::st_coordinates(sf::st_geometry(geometry_sf)[i])
    group_cols <- intersect(colnames(coords), c("L1", "L2", "L3"))
    max_jump <- 0
    if (nrow(coords) > 1L) {
      if (length(group_cols)) {
        ring <- interaction(
          as.data.frame(coords[, group_cols, drop = FALSE]),
          drop = TRUE
        )
        jumps <- vapply(
          split(coords[, "X"], ring),
          function(x) if (length(x) > 1L) max(abs(diff(x))) else 0,
          numeric(1)
        )
        max_jump <- max(jumps, 0, na.rm = TRUE)
      } else {
        max_jump <- max(abs(diff(coords[, "X"])), 0, na.rm = TRUE)
      }
    }
    bbox <- sf::st_bbox(geometry_sf[i, , drop = FALSE])
    data.table::data.table(
      map_iso3 = as.character(geometry_sf[[iso_col]][i]),
      geometry_type = as.character(sf::st_geometry_type(geometry_sf)[i]),
      valid = isTRUE(sf::st_is_valid(geometry_sf)[i]),
      empty = isTRUE(sf::st_is_empty(geometry_sf)[i]),
      xmin = as.numeric(bbox[["xmin"]]),
      xmax = as.numeric(bbox[["xmax"]]),
      bbox_width = as.numeric(bbox[["xmax"]] - bbox[["xmin"]]),
      max_ring_longitude_jump = max_jump,
      world_spanning_edge = is.finite(max_jump) && max_jump > 180
    )
  })
  data.table::rbindlist(rows)
}

validate_leaflet_geometry <- function(geometry_sf, iso_col = "map_iso3",
                                      mapped_iso3 = NULL) {
  if (!inherits(geometry_sf, "sf")) {
    stop("Prepared map geometry is not an sf object.", call. = FALSE)
  }
  if (!identical(sf::st_crs(geometry_sf)$epsg, 4326L)) {
    stop("Prepared map geometry must use EPSG:4326.", call. = FALSE)
  }
  if (!iso_col %in% names(geometry_sf) ||
      anyDuplicated(as.character(geometry_sf[[iso_col]]))) {
    stop("Prepared map geometry must contain one row per ISO3.", call. = FALSE)
  }
  types <- as.character(sf::st_geometry_type(geometry_sf))
  if (any(!types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Prepared map geometry contains non-polygon features.", call. = FALSE)
  }
  if (any(!sf::st_is_valid(geometry_sf))) {
    stop("Prepared map geometry contains invalid features.", call. = FALSE)
  }
  check_rows <- rep(TRUE, nrow(geometry_sf))
  if (!is.null(mapped_iso3)) {
    check_rows <- as.character(geometry_sf[[iso_col]]) %in% as.character(mapped_iso3)
  }
  if (any(sf::st_is_empty(geometry_sf)[check_rows])) {
    stop("Prepared map geometry contains empty mapped features.", call. = FALSE)
  }
  coords <- sf::st_coordinates(sf::st_geometry(geometry_sf))
  if (any(coords[, "X"] < -180 - 1e-8 | coords[, "X"] > 180 + 1e-8)) {
    stop("Prepared map geometry has longitude outside [-180, 180].", call. = FALSE)
  }
  diagnostics <- leaflet_geometry_diagnostics(geometry_sf, iso_col)
  if (any(diagnostics$world_spanning_edge)) {
    stop(
      "Prepared map geometry has unwrapped antimeridian edges for: ",
      paste(diagnostics[world_spanning_edge == TRUE]$map_iso3, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(diagnostics)
}

build_map_geometry <- function(simplify_dTolerance = 0.08) {
  if (!requireNamespace("sf", quietly = TRUE) ||
      !requireNamespace("rnaturalearth", quietly = TRUE)) {
    stop("sf and rnaturalearth are required to build map geometry.", call. = FALSE)
  }
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  iso <- as.character(world$iso_a3)
  iso_eh <- as.character(world$iso_a3_eh)
  bad <- is.na(iso) | !nzchar(iso) | iso %in% c("-99", "NUL", "ATA")
  iso[bad & !is.na(iso_eh) & nzchar(iso_eh) & !iso_eh %in% c("-99")] <- iso_eh[bad & !is.na(iso_eh) & nzchar(iso_eh) & !iso_eh %in% c("-99")]
  world$map_iso3 <- iso
  nm <- as.character(world$name_long)
  if (all(is.na(nm) | !nzchar(nm))) nm <- as.character(world$admin)
  miss <- is.na(nm) | !nzchar(nm)
  nm[miss] <- as.character(world$name[miss])
  world$map_name <- nm
  world$region_wb <- as.character(world$region_wb)
  rw <- as.character(world$region_wb)
  rw[is.na(rw)] <- ""
  keep <- !is.na(world$map_iso3) & nchar(world$map_iso3) == 3L &
    !(world$map_iso3 %in% c("-99", "ATA")) &
    !(tolower(rw) %in% c("antarctica"))
  world <- world[keep, c("map_iso3", "map_name", "region_wb", "geometry")]
  world <- prepare_leaflet_geometry(world)
  if (is.finite(simplify_dTolerance) && simplify_dTolerance > 0) {
    world <- sf::st_simplify(world, dTolerance = simplify_dTolerance, preserveTopology = TRUE)

    world <- prepare_leaflet_geometry(world)
  }
  world$geometry_source <- "natural_earth_medium_rnaturalearthdata"
  world$geometry_licence <- "Natural Earth (public domain terms)"
  rownames(world) <- NULL
  validate_leaflet_geometry(world)
  attr(world, "map_geometry_prep_version") <- MAP_GEOMETRY_PREP_VERSION
  world
}

map_geometry_cache_path <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- load_config()
  file.path(cfg$paths$processed, MAP_GEOMETRY_FILENAME)
}

map_crosswalk_path <- function(cfg = NULL) {
  if (is.null(cfg)) cfg <- load_config()
  file.path(cfg$paths$processed, MAP_CROSSWALK_FILENAME)
}

load_map_geometry <- function(cfg = load_config(), rebuild = FALSE,
                              validate_cached = FALSE) {
  path <- map_geometry_cache_path(cfg)
  if (!isTRUE(rebuild) && file.exists(path)) {
    geom <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(geom) && inherits(geom, "sf") && nrow(geom) > 0) {
      current <- identical(
        attr(geom, "map_geometry_prep_version"),
        MAP_GEOMETRY_PREP_VERSION
      )
      can_rebuild <- requireNamespace("sf", quietly = TRUE) &&
        requireNamespace("rnaturalearth", quietly = TRUE)
      if (current) {
        attr(geom, "leaflet_geometry_prepared") <- TRUE
        if (isTRUE(validate_cached)) validate_leaflet_geometry(geom)
        return(geom)
      }
      if (!can_rebuild) {
        geom <- prepare_leaflet_geometry(geom)
        validate_leaflet_geometry(geom)
        return(geom)
      }

    }
  }
  if (!requireNamespace("sf", quietly = TRUE) ||
      !requireNamespace("rnaturalearth", quietly = TRUE)) {
    return(NULL)
  }
  geom <- tryCatch(build_map_geometry(), error = function(e) {
    warning("Map geometry build failed: ", conditionMessage(e), call. = FALSE)
    NULL
  })
  if (is.null(geom)) return(NULL)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch(saveRDS(geom, path), error = function(e) {
    warning("Could not cache map geometry: ", conditionMessage(e), call. = FALSE)
  })
  geom
}

build_geographic_crosswalk <- function(trade_iso3, geometry) {
  trade_iso3 <- unique(as.character(trade_iso3))
  trade_iso3 <- trade_iso3[!is.na(trade_iso3) & nchar(trade_iso3) == 3L]
  trade_iso3 <- setdiff(trade_iso3, AGGREGATE_ISO3_MAP)
  map_iso <- if (!is.null(geometry) && inherits(geometry, "sf")) {
    unique(as.character(geometry$map_iso3))
  } else {
    character()
  }
  matched <- intersect(trade_iso3, map_iso)
  unmatched <- setdiff(trade_iso3, map_iso)
  geom_only <- setdiff(map_iso, trade_iso3)
  rbind(
    data.table::data.table(
      source_iso3 = matched,
      map_iso3 = matched,
      country_name = NA_character_,
      geometry_match_status = "matched",
      match_method = "iso3_exact",
      exclusion_reason = NA_character_
    ),
    data.table::data.table(
      source_iso3 = unmatched,
      map_iso3 = NA_character_,
      country_name = NA_character_,
      geometry_match_status = "unmatched",
      match_method = "none",
      exclusion_reason = "no_natural_earth_polygon"
    ),
    if (length(geom_only)) {
      data.table::data.table(
        source_iso3 = NA_character_,
        map_iso3 = geom_only,
        country_name = NA_character_,
        geometry_match_status = "geometry_only",
        match_method = "geometry_present",
        exclusion_reason = "no_trade_observation_in_source"
      )
    } else {
      data.table::data.table()
    },
    fill = TRUE
  )
}

persist_geographic_crosswalk <- function(crosswalk, cfg = load_config()) {
  path <- map_crosswalk_path(cfg)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch({
    arrow::write_parquet(data.table::as.data.table(crosswalk), path)
  }, error = function(e) {
    warning("Could not write geographic crosswalk: ", conditionMessage(e), call. = FALSE)
  })
  invisible(path)
}

make_synthetic_map_geometry <- function(iso3 = c("DEU", "USA", "CHN", "IND")) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(NULL)
  }
  polys <- lapply(seq_along(iso3), function(i) {
    x0 <- (i - 1) * 2
    sf::st_polygon(list(matrix(c(x0, 0, x0 + 1, 0, x0 + 1, 1, x0, 1, x0, 0),
                               ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(
    map_iso3 = iso3,
    map_name = iso3,
    region_wb = c("Europe & Central Asia", "North America",
                  "East Asia & Pacific", "South Asia")[seq_along(iso3)],
    geometry = sf::st_sfc(polys, crs = 4326),
    geometry_source = "synthetic_test",
    geometry_licence = "test-only"
  )
}
