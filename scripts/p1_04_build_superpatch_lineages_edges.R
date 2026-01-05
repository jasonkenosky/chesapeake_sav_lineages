# ==============================================================================
# Script Title: p1_04_build_superpatch_lineages_edges
# Purpose: Build spatiotemporal lineage graph for yearly superpatch polygons using
#          IoU/Jaccard as the primary continuity criterion, with a conservative
#          weak-link fallback (buffer overlap or short distance only when IoU == 0).
# Author: Jason Kenosky
# Last Updated: 2026-01-02
#
# Description:
#   - Reads yearly superpatch layers (one .gpkg per year) produced by script 03.
#   - Assigns a global polygon ID (poly_id) to every polygon-year observation.
#   - Builds a forward-only edge list between adjacent survey years using:
#       * Strong link: IoU >= iou_strong_threshold (default 0.10)
#       * Weak link: ONLY when IoU == 0, allow if buffered overlap exists OR
#                    edge-to-edge distance <= distance_weak_threshold (default 20m)
#     Special rule: allow 1987 -> 1989 links (1988 survey hiatus). No other gaps.
#   - Computes connected components of the full graph; assigns lineage_id after
#     the full graph is built (order-independent).
#   - Computes predecessor/successor counts to support event typing downstream.
#
# Inputs:
#   data_processed/vims_superpatches/superpatches_YYYY.gpkg
#
# Outputs:
#   	- data_processed/vims_superpatch_lineages/superpatch_edges.csv
#.    - data_processed/vims_superpatch_lineages/superpatch_nodes.csv
#.    - qc/<SCRIPT_ID>_qc_latest.csv (+ timestamped)
#
# Note: This script has a number of QC checks to ensure the "lineage" graph is solid.
# ==============================================================================

# ------------------------------------------------
# 0. Housekeeping
# ------------------------------------------------
rm(list = ls())
gc()

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

# ------------------------------------------------
# 1. Load packages
# ------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(glue)
  library(fs)
  library(here)
  library(tibble)
  library(sf)
  library(readr)
  library(lwgeom)
  library(igraph)
})

# ------------------------------------------------
# 2. Script configuration (directories, parameters)
# ------------------------------------------------
SCRIPT_ID <- "p1_04_build_superpatch_lineages_edges"

project_root_directory <- here::here()

data_processed_directory <- file.path(project_root_directory, "data_processed")
logs_directory <- file.path(project_root_directory, "logs")
qc_directory   <- file.path(project_root_directory, "qc")

superpatch_input_directory <- file.path(data_processed_directory, "vims_superpatches")
lineage_output_directory   <- file.path(data_processed_directory, "vims_superpatch_lineages")

nodes_gpkg_path <- file.path(lineage_output_directory, "superpatch_nodes.gpkg")
nodes_layer_name <- "superpatch_nodes"

fs::dir_create(logs_directory, recurse = TRUE)
fs::dir_create(qc_directory, recurse = TRUE)
fs::dir_create(lineage_output_directory, recurse = TRUE)

qc_latest_file <- file.path(qc_directory, glue("{SCRIPT_ID}_qc_latest.csv"))

# --- Spatial + continuity parameters -----------------------------------------
target_epsg <- 26918

# Primary continuity metric: IoU/Jaccard on UNBUFFERED polygons
iou_strong_threshold <- 0.10
iou_sensitivity_values <- c(0.05, 0.10, 0.20)

# Uncertainty buffer distance (meters)
buffer_distance_m <- 10
buffer_sensitivity_values <- c(5, 10, 20)

# Distance-only continuity (edge-to-edge distance, meters)
distance_weak_threshold_m <- 20
distance_sensitivity_values <- c(10, 20, 30)

# Weak links allowed only when IoU == 0, and only if:
#   buffered overlap exists OR distance <= distance_weak_threshold_m

# 1988 hiatus exception: allow 1987 -> 1989
gap_exception_from_year <- 1987L
gap_exception_to_year   <- 1989L

# ------------------------------------------------
# 3. Logging setup
# ------------------------------------------------
logger_helpers_file <- here::here("R", "logger_helpers.R")
if (!file.exists(logger_helpers_file)) stop("Missing logger helpers: ", logger_helpers_file)
source(logger_helpers_file)

log_meta <- NULL
log_meta <- start_log(SCRIPT_ID, logs_directory)

on.exit({
  if (!is.null(log_meta)) stop_log(log_meta)
}, add = TRUE)

log_section(glue("{SCRIPT_ID} - START"))
log_info(glue("Project root: {project_root_directory}"))
log_info(glue("Working dir:  {getwd()}"))
log_info(glue("Log file:     {log_meta$log_file}"))

qc_timestamp <- gsub(glue("{SCRIPT_ID}_|\\.log$"), "", basename(log_meta$log_file))
qc_run_file  <- file.path(qc_directory, glue("{SCRIPT_ID}_qc_{qc_timestamp}.csv"))

# ---- Timing helper fallback --------------------------------------------------
if (!exists("with_timing")) {
  with_timing <- function(label, expr) {
    start_time <- Sys.time()
    result <- eval(expr)
    end_time <- Sys.time()
    elapsed_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))
    if (exists("log_info")) log_info(glue("{label} | {round(elapsed_seconds, 3)} sec"))
    result
  }
}

# ------------------------------------------------
# 4. Inputs (validate early)
# ------------------------------------------------
log_section("INPUTS")

if (!dir_exists(superpatch_input_directory)) {
  log_error(glue("Missing superpatch directory: {superpatch_input_directory}"))
  stop("Missing directory: ", superpatch_input_directory)
}

superpatch_gpkg_files <- fs::dir_ls(superpatch_input_directory, type = "file", regexp = "\\.gpkg$")
if (length(superpatch_gpkg_files) == 0) {
  log_error("No superpatch gpkg files found.")
  stop("No gpkg files found in: ", superpatch_input_directory)
}
log_info(glue("Superpatch gpkg files discovered: {length(superpatch_gpkg_files)}"))

# ------------------------------------------------
# 5. Helpers (script-local only)
# ------------------------------------------------
make_qc_row <- function(metric_name, metric_value, notes = NA_character_) {
  tibble::tibble(
    timestamp = Sys.time(),
    script_id = SCRIPT_ID,
    metric    = metric_name,
    value     = as.character(metric_value),
    notes     = notes
  )
}

write_qc_tables <- function(qc_table, latest_file, run_file) {
  fs::dir_create(dirname(latest_file), recurse = TRUE)
  readr::write_csv(qc_table, latest_file)
  readr::write_csv(qc_table, run_file)
  invisible(TRUE)
}

extract_year_from_filename <- function(file_path) {
  file_name <- basename(file_path)
  matched_year <- regmatches(file_name, regexpr("(19|20)\\d{2}", file_name))
  if (length(matched_year) == 1 && nchar(matched_year) == 4) return(as.integer(matched_year))
  NA_integer_
}

safe_write_gpkg_layer <- function(sf_object, gpkg_path, layer_name) {
  fs::dir_create(dirname(gpkg_path), recurse = TRUE)
  if (file.exists(gpkg_path)) file.remove(gpkg_path)
  sf::st_write(sf_object, dsn = gpkg_path, layer = layer_name, quiet = TRUE)
  invisible(gpkg_path)
}

enforce_target_crs <- function(sf_object, epsg_code) {
  current_crs <- sf::st_crs(sf_object)
  if (is.na(current_crs)) {
    sf::st_crs(sf_object) <- sf::st_crs(epsg_code)
    return(sf_object)
  }
  if (!is.na(current_crs$epsg) && current_crs$epsg != epsg_code) {
    sf_object <- sf::st_transform(sf_object, epsg_code)
  }
  sf_object
}

add_global_polygon_id <- function(superpatch_sf, year_value) {
  
  superpatch_sf <- superpatch_sf %>%
    mutate(year = as.integer(year_value)) %>%
    arrange(superpatch_id)  # deterministic ordering
  
  polygon_sequence  <- seq_len(nrow(superpatch_sf))
  polygon_id_string <- sprintf("%06d", polygon_sequence)
  
  superpatch_sf %>%
    mutate(poly_id = paste0("poly_", year_value, "_", polygon_id_string))
}

create_allowed_year_pairs <- function(sorted_year_values, exception_from, exception_to) {
  consecutive_pairs <- tibble::tibble(
    year_from = sorted_year_values[-length(sorted_year_values)],
    year_to   = sorted_year_values[-1]
  ) %>%
    filter(year_to - year_from == 1)
  
  exception_pair <- tibble::tibble(
    year_from = as.integer(exception_from),
    year_to   = as.integer(exception_to)
  )
  
  # Include exception only if both years exist in the dataset
  if (exception_from %in% sorted_year_values && exception_to %in% sorted_year_values) {
    bind_rows(consecutive_pairs, exception_pair) %>%
      distinct(year_from, year_to) %>%
      arrange(year_from, year_to)
  } else {
    consecutive_pairs
  }
}

compute_iou_jaccard <- function(geometry_a, geometry_b) {
  intersection_geometry <- sf::st_intersection(geometry_a, geometry_b)
  if (length(intersection_geometry) == 0) return(0)
  
  intersection_area_m2 <- as.numeric(sf::st_area(intersection_geometry))
  if (is.na(intersection_area_m2) || intersection_area_m2 <= 0) return(0)
  
  area_a_m2 <- as.numeric(sf::st_area(geometry_a))
  area_b_m2 <- as.numeric(sf::st_area(geometry_b))
  union_area_m2 <- area_a_m2 + area_b_m2 - intersection_area_m2
  
  if (is.na(union_area_m2) || union_area_m2 <= 0) return(0)
  
  intersection_area_m2 / union_area_m2
}

build_candidate_pairs <- function(
    polygons_from_year_sf,
    polygons_to_year_sf,
    buffer_distance_m,
    distance_threshold_m
) {
  # Candidate set 1: buffered overlap 
  buffered_from_geometry <- sf::st_buffer(sf::st_geometry(polygons_from_year_sf), dist = buffer_distance_m)
  buffered_to_geometry   <- sf::st_buffer(sf::st_geometry(polygons_to_year_sf),   dist = buffer_distance_m)
  
  buffered_overlap_list <- sf::st_intersects(buffered_from_geometry, buffered_to_geometry)
  
  buffered_candidate_table <- tibble::tibble(
    from_index = rep(seq_along(buffered_overlap_list), lengths(buffered_overlap_list)),
    to_index   = unlist(buffered_overlap_list)
  )
  
  # Candidate set 2: distance-only proximity (captures near-miss cases)
  within_distance_list <- sf::st_is_within_distance(
    sf::st_geometry(polygons_from_year_sf),
    sf::st_geometry(polygons_to_year_sf),
    dist = distance_threshold_m
  )
  
  distance_candidate_table <- tibble::tibble(
    from_index = rep(seq_along(within_distance_list), lengths(within_distance_list)),
    to_index   = unlist(within_distance_list)
  )
  
  candidate_pairs <- bind_rows(buffered_candidate_table, distance_candidate_table) %>%
    distinct(from_index, to_index)
  
  candidate_pairs
}

score_and_filter_links <- function(
    polygons_from_year_sf,
    polygons_to_year_sf,
    candidate_pairs_table,
    iou_strong_threshold,
    buffer_distance_m,
    distance_weak_threshold_m
) {
  if (nrow(candidate_pairs_table) == 0) {
    return(tibble::tibble())
  }
  
  from_geometry <- sf::st_geometry(polygons_from_year_sf)
  to_geometry   <- sf::st_geometry(polygons_to_year_sf)
  
  buffered_from_geometry <- sf::st_buffer(from_geometry, dist = buffer_distance_m)
  buffered_to_geometry   <- sf::st_buffer(to_geometry,   dist = buffer_distance_m)
  
  edge_rows <- vector("list", length = nrow(candidate_pairs_table))
  
  for (row_index in seq_len(nrow(candidate_pairs_table))) {
    
    from_index_value <- candidate_pairs_table$from_index[row_index]
    to_index_value   <- candidate_pairs_table$to_index[row_index]
    
    geometry_from <- from_geometry[from_index_value]
    geometry_to   <- to_geometry[to_index_value]
    
    iou_value <- compute_iou_jaccard(geometry_from, geometry_to)
    
    buffered_overlap_value <- length(sf::st_intersects(
      buffered_from_geometry[from_index_value],
      buffered_to_geometry[to_index_value]
    )[[1]]) > 0
    
    edge_distance_m <- as.numeric(sf::st_distance(geometry_from, geometry_to))
    if (length(edge_distance_m) > 1) edge_distance_m <- edge_distance_m[1]
    
    # Acceptance rules (your specification):
    # Strong: IoU >= threshold
    # Weak: ONLY when IoU == 0, and only if buffered overlap exists OR distance <= threshold
    is_strong_link <- (!is.na(iou_value)) && (iou_value >= iou_strong_threshold)
    
    is_weak_link <- (iou_value == 0) && (isTRUE(buffered_overlap_value) || (!is.na(edge_distance_m) && edge_distance_m <= distance_weak_threshold_m))
    
    is_accepted <- is_strong_link || is_weak_link
    
    if (!is_accepted) {
      edge_rows[[row_index]] <- NULL
      next
    }
    
    link_strength_class <- if (is_strong_link) "strong" else "weak"
    
    link_reason <- if (is_strong_link) {
      "iou"
    } else {
      if (isTRUE(buffered_overlap_value) && !is.na(edge_distance_m) && edge_distance_m <= distance_weak_threshold_m) {
        "buffer_or_distance"
      } else if (isTRUE(buffered_overlap_value)) {
        "buffer"
      } else {
        "distance"
      }
    }
    
    edge_rows[[row_index]] <- tibble::tibble(
      poly_id_from = polygons_from_year_sf$poly_id[from_index_value],
      poly_id_to   = polygons_to_year_sf$poly_id[to_index_value],
      year_from    = polygons_from_year_sf$year[from_index_value],
      year_to      = polygons_to_year_sf$year[to_index_value],
      iou_jaccard  = iou_value,
      buffered_overlap = buffered_overlap_value,
      edge_distance_m  = edge_distance_m,
      link_strength    = link_strength_class,
      link_reason      = link_reason
    )
  }
  
  bind_rows(edge_rows)
}

compute_lineage_components <- function(edge_table, node_table) {
  if (nrow(edge_table) == 0) {
    node_table$lineage_id <- NA_character_
    node_table$component_id <- NA_integer_
    return(node_table)
  }
  
  # Connected components should be computed on undirected connectivity.
  undirected_edges <- edge_table %>%
    select(poly_id_from, poly_id_to) %>%
    distinct()
  
  graph_object <- igraph::graph_from_data_frame(
    d = undirected_edges,
    directed = FALSE,
    vertices = node_table %>% select(poly_id) %>% distinct()
  )
  
  component_membership <- igraph::components(graph_object)$membership
  
  component_table <- tibble::tibble(
    poly_id = names(component_membership),
    component_id = as.integer(component_membership)
  )
  
  # Deterministic lineage_id assignment: order components by earliest year then size then id
  component_summary <- node_table %>%
    inner_join(component_table, by = "poly_id") %>%
    group_by(component_id) %>%
    summarise(
      component_size = n(),
      component_first_year = min(year, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(component_first_year, desc(component_size), component_id) %>%
    mutate(lineage_id = paste0("lin_", sprintf("%06d", row_number())))
  
  node_table_with_lineage <- node_table %>%
    left_join(component_table, by = "poly_id") %>%
    left_join(component_summary %>% select(component_id, lineage_id), by = "component_id")
  
  node_table_with_lineage
}

compute_predecessor_successor_counts <- function(edge_table, node_table) {
  predecessor_counts <- edge_table %>%
    group_by(poly_id_to) %>%
    summarise(n_predecessors = n(), .groups = "drop") %>%
    rename(poly_id = poly_id_to)
  
  successor_counts <- edge_table %>%
    group_by(poly_id_from) %>%
    summarise(n_successors = n(), .groups = "drop") %>%
    rename(poly_id = poly_id_from)
  
  node_table %>%
    left_join(predecessor_counts, by = "poly_id") %>%
    left_join(successor_counts, by = "poly_id") %>%
    mutate(
      n_predecessors = ifelse(is.na(n_predecessors), 0L, as.integer(n_predecessors)),
      n_successors   = ifelse(is.na(n_successors), 0L, as.integer(n_successors))
    )
}

# ------------------------------------------------
# 6. Main process
# ------------------------------------------------
log_section("PROCESS")

sf::sf_use_s2(FALSE)

qc_table <- tibble::tibble()

qc_table <- bind_rows(
  qc_table,
  make_qc_row("input_superpatch_files_discovered", length(superpatch_gpkg_files)),
  make_qc_row("iou_strong_threshold_default", iou_strong_threshold),
  make_qc_row("buffer_distance_m_default", buffer_distance_m),
  make_qc_row("distance_weak_threshold_m_default", distance_weak_threshold_m),
  make_qc_row("iou_sensitivity_values", paste(iou_sensitivity_values, collapse = ",")),
  make_qc_row("buffer_sensitivity_values", paste(buffer_sensitivity_values, collapse = ",")),
  make_qc_row("distance_sensitivity_values", paste(distance_sensitivity_values, collapse = ","))
)

# --- Load all yearly superpatch layers into memory (nodes) --------------------
year_to_superpatch_sf <- list()

for (superpatch_file_path in superpatch_gpkg_files) {
  
  year_value <- extract_year_from_filename(superpatch_file_path)
  if (is.na(year_value)) {
    log_warn(glue("Skipping file (no 4-digit year in name): {basename(superpatch_file_path)}"))
    qc_table <- bind_rows(qc_table, make_qc_row("skip_no_year_file", basename(superpatch_file_path)))
    next
  }
  
  log_section(glue("LOAD YEAR {year_value}"))
  
  year_superpatch_sf <- with_timing(glue("Read superpatch gpkg ({year_value})"), quote({
    sf::st_read(superpatch_file_path, quiet = TRUE)
  }))
  
  year_superpatch_sf <- enforce_target_crs(year_superpatch_sf, target_epsg)
  year_superpatch_sf <- sf::st_make_valid(year_superpatch_sf)
  
  year_superpatch_sf <- add_global_polygon_id(year_superpatch_sf, year_value)
  
  # --- QC: superpatch_id uniqueness per year -------------------------------
  n_rows_year <- nrow(year_superpatch_sf)
  n_distinct_ids <- dplyr::n_distinct(year_superpatch_sf$superpatch_id)
  
  qc_table <- bind_rows(
    qc_table,
    make_qc_row(
      metric_name  = glue("year_{year_value}_superpatch_id_distinct"),
      metric_value = n_distinct_ids
    ),
    make_qc_row(
      metric_name  = glue("year_{year_value}_superpatch_id_rows"),
      metric_value = n_rows_year
    ),
    make_qc_row(
      metric_name  = glue("year_{year_value}_superpatch_id_unique"),
      metric_value = n_distinct_ids == n_rows_year
    )
  )
  
  # Hard stop if violated
  if (n_distinct_ids != n_rows_year) {
    dupes <- year_superpatch_sf$superpatch_id[duplicated(year_superpatch_sf$superpatch_id)]
    log_error(glue(
      "Duplicate superpatch_id detected in year {year_value}. Example: {dupes[1]}"
    ))
    stop("QC failure: superpatch_id is not unique within year ", year_value)
  }
  
  qc_table <- bind_rows(
    qc_table,
    make_qc_row(glue("year_{year_value}_rows_loaded"), nrow(year_superpatch_sf))
  )
  
  year_to_superpatch_sf[[as.character(year_value)]] <- year_superpatch_sf
}

available_year_values <- sort(as.integer(names(year_to_superpatch_sf)))

if (length(available_year_values) == 0) {
  log_error("No readable yearly superpatch layers were loaded.")
  stop("No yearly layers loaded. Check input directory and filenames.")
}

qc_table <- bind_rows(qc_table, make_qc_row("years_loaded_count", length(available_year_values)))
qc_table <- bind_rows(qc_table, make_qc_row("years_loaded_list", paste(available_year_values, collapse = ",")))

allowed_year_pairs <- create_allowed_year_pairs(
  sorted_year_values = available_year_values,
  exception_from = gap_exception_from_year,
  exception_to   = gap_exception_to_year
)

qc_table <- bind_rows(qc_table, make_qc_row("allowed_year_pairs_count", nrow(allowed_year_pairs)))
log_info(glue("Allowed year pairs: {nrow(allowed_year_pairs)}"))

# --- Build edge list ----------------------------------------------------------
edge_table_all <- tibble::tibble()

for (pair_index in seq_len(nrow(allowed_year_pairs))) {
  
  year_from_value <- allowed_year_pairs$year_from[pair_index]
  year_to_value   <- allowed_year_pairs$year_to[pair_index]
  
  log_section(glue("LINK {year_from_value} -> {year_to_value}"))
  
  polygons_from_year_sf <- year_to_superpatch_sf[[as.character(year_from_value)]]
  polygons_to_year_sf   <- year_to_superpatch_sf[[as.character(year_to_value)]]
  
  if (is.null(polygons_from_year_sf) || is.null(polygons_to_year_sf)) {
    log_warn(glue("Missing year layer in memory: {year_from_value} or {year_to_value}"))
    next
  }
  
  candidate_pairs_table <- with_timing(glue("Candidate search (buffer + distance) {year_from_value}->{year_to_value}"), quote({
    build_candidate_pairs(
      polygons_from_year_sf = polygons_from_year_sf,
      polygons_to_year_sf   = polygons_to_year_sf,
      buffer_distance_m     = buffer_distance_m,
      distance_threshold_m  = distance_weak_threshold_m
    )
  }))
  
  qc_table <- bind_rows(
    qc_table,
    make_qc_row(glue("pair_{year_from_value}_{year_to_value}_candidate_pairs"), nrow(candidate_pairs_table))
  )
  
  edge_table_pair <- with_timing(glue("Score + filter links {year_from_value}->{year_to_value}"), quote({
    score_and_filter_links(
      polygons_from_year_sf     = polygons_from_year_sf,
      polygons_to_year_sf       = polygons_to_year_sf,
      candidate_pairs_table     = candidate_pairs_table,
      iou_strong_threshold      = iou_strong_threshold,
      buffer_distance_m         = buffer_distance_m,
      distance_weak_threshold_m = distance_weak_threshold_m
    )
  }))
  
  qc_table <- bind_rows(
    qc_table,
    make_qc_row(glue("pair_{year_from_value}_{year_to_value}_edges_accepted"), nrow(edge_table_pair)),
    make_qc_row(glue("pair_{year_from_value}_{year_to_value}_strong_edges"), sum(edge_table_pair$link_strength == "strong", na.rm = TRUE)),
    make_qc_row(glue("pair_{year_from_value}_{year_to_value}_weak_edges"), sum(edge_table_pair$link_strength == "weak", na.rm = TRUE))
  )
  
  edge_table_all <- bind_rows(edge_table_all, edge_table_pair)
}

qc_table <- bind_rows(qc_table, make_qc_row("edges_total_accepted", nrow(edge_table_all)))

# --- Build node table (attributes + geometry retained) ------------------------
node_sf_all <- bind_rows(year_to_superpatch_sf)

if (dplyr::n_distinct(node_sf_all$poly_id) != nrow(node_sf_all)) {
  dupes <- node_sf_all$poly_id[duplicated(node_sf_all$poly_id)]
  log_error(glue("poly_id is not unique. Example duplicate: {dupes[1]}"))
  stop("poly_id uniqueness failure")
}

dupe_by_year <- node_sf_all |>
  st_drop_geometry() |>
  count(year, poly_id) |>
  filter(n > 1)

if (nrow(dupe_by_year) > 0) stop("poly_id duplicates within year detected")

# Compact node CSV table (no geometry)
node_table_all <- node_sf_all %>%
  st_drop_geometry() %>%
  select(poly_id, year, everything())

# --- Nodes are written WITHOUT lineage fields ---------------------------------
node_table_clean <- node_table_all

qc_table <- bind_rows(
  qc_table,
  make_qc_row("nodes_total", nrow(node_table_clean))
)

# --- QC: CSV vs GPKG parity (attributes only) --------------------------------
nodes_csv_read <- readr::read_csv(nodes_csv_path, show_col_types = FALSE)
nodes_gpkg_read <- sf::st_read(nodes_gpkg_path, layer = nodes_layer_name, quiet = TRUE) |>
  sf::st_drop_geometry()

# 1) row counts
qc_table <- bind_rows(qc_table,
                      make_qc_row("qc_nodes_csv_n", nrow(nodes_csv_read)),
                      make_qc_row("qc_nodes_gpkg_n", nrow(nodes_gpkg_read))
)

# 2) poly_id uniqueness
qc_table <- bind_rows(qc_table,
                      make_qc_row("qc_nodes_csv_poly_id_unique", dplyr::n_distinct(nodes_csv_read$poly_id) == nrow(nodes_csv_read)),
                      make_qc_row("qc_nodes_gpkg_poly_id_unique", dplyr::n_distinct(nodes_gpkg_read$poly_id) == nrow(nodes_gpkg_read))
)

# 3) poly_id set equality
csv_ids  <- sort(nodes_csv_read$poly_id)
gpkg_ids <- sort(nodes_gpkg_read$poly_id)

qc_table <- bind_rows(qc_table,
                      make_qc_row("qc_poly_id_sets_equal", identical(csv_ids, gpkg_ids),
                                  notes = if (!identical(csv_ids, gpkg_ids)) {
                                    paste0("csv_only=", length(setdiff(csv_ids, gpkg_ids)),
                                           "; gpkg_only=", length(setdiff(gpkg_ids, csv_ids)))
                                  } else NA_character_)
)

# 4) attribute equality (excluding geometry; tolerant for doubles)
# choose “load-bearing” columns that should never differ
check_cols <- intersect(
  names(nodes_csv_read),
  names(nodes_gpkg_read)
)

# align rows by poly_id
csv_aligned  <- nodes_csv_read  |> dplyr::arrange(poly_id) |> dplyr::select(all_of(check_cols))
gpkg_aligned <- nodes_gpkg_read |> dplyr::arrange(poly_id) |> dplyr::select(all_of(check_cols))

# numeric tolerance for floating columns
num_cols <- check_cols[sapply(csv_aligned, is.numeric)]

max_abs_diff <- 0
if (length(num_cols) > 0) {
  diffs <- mapply(function(a, b) max(abs(a - b), na.rm = TRUE),
                  csv_aligned[num_cols], gpkg_aligned[num_cols])
  max_abs_diff <- max(diffs, na.rm = TRUE)
}

# non-numeric exact match
non_num_cols <- setdiff(check_cols, num_cols)
non_num_equal <- TRUE
if (length(non_num_cols) > 0) {
  non_num_equal <- all(mapply(function(a, b) identical(a, b),
                              csv_aligned[non_num_cols], gpkg_aligned[non_num_cols]))
}

qc_table <- bind_rows(qc_table,
                      make_qc_row("qc_nodes_non_numeric_equal", non_num_equal),
                      make_qc_row("qc_nodes_numeric_max_abs_diff", max_abs_diff)
)

# Set a sensible threshold, e.g. 1e-6 or 1e-8 depending on your pipeline
qc_table <- bind_rows(qc_table,
                      make_qc_row("qc_nodes_numeric_within_tol", max_abs_diff <= 1e-6)
)

# ------------------------------------------------
# 7. Write outputs
# ------------------------------------------------
log_section("OUTPUTS")

edges_csv_path <- file.path(lineage_output_directory, "superpatch_edges.csv")
nodes_csv_path <- file.path(lineage_output_directory, "superpatch_nodes.csv")


with_timing("Write edges CSV", quote({
  readr::write_csv(edge_table_all, edges_csv_path)
}))
log_info(glue("Wrote: {edges_csv_path}"))

with_timing("Write nodes CSV", quote({
  readr::write_csv(node_table_clean, nodes_csv_path)
}))
log_info(glue("Wrote: {nodes_csv_path}"))

with_timing("Write nodes GPKG", quote({
  # Keep geometry + all attributes (including poly_id)
  node_sf_out <- node_sf_all %>%
    select(poly_id, year, everything())
  
  safe_write_gpkg_layer(node_sf_out, nodes_gpkg_path, nodes_layer_name)
}))
log_info(glue("Wrote: {nodes_gpkg_path} (layer: {nodes_layer_name})"))



# ------------------------------------------------
# 8. QC summary (always write)
# ------------------------------------------------
log_section("QC SUMMARY")

if (nrow(qc_table) == 0) {
  qc_table <- bind_rows(qc_table, make_qc_row("qc_note", "No QC rows recorded."))
}

with_timing("Write QC tables", quote({
  write_qc_tables(qc_table, latest_file = qc_latest_file, run_file = qc_run_file)
}))

log_info(glue("QC latest: {qc_latest_file}"))
log_info(glue("QC run:    {qc_run_file}"))

# ------------------------------------------------
# 9. Done
# ------------------------------------------------
log_section("DONE")
log_info("Script completed successfully.")

