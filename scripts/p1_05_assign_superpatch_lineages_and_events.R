# ==============================================================================
# Script Title: p1_05_assign_superpatch_lineages_and_events
# Purpose: Assign lineage IDs and event types from an existing spatiotemporal
#          edge list and node table (NO edge rebuilding here).
# Author: Jason Kenosky
# Last Updated: 2026-01-05
# ==============================================================================

rm(list = ls())
gc()

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

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

SCRIPT_ID <- "p1_05_assign_superpatch_lineages_and_events"

project_root_directory <- here::here()
data_processed_directory <- file.path(project_root_directory, "data_processed")
logs_directory <- file.path(project_root_directory, "logs")
qc_directory   <- file.path(project_root_directory, "qc")
lineage_directory <- file.path(data_processed_directory, "vims_superpatch_lineages")

nodes_csv_out <- file.path(lineage_directory, "superpatch_nodes_with_lineage.csv")
nodes_gpkg_out <- file.path(lineage_directory, "superpatch_nodes_with_lineage.gpkg")
nodes_layer <- "superpatch_nodes_with_lineage"

lineage_summary_out <- file.path(lineage_directory, "lineage_summary.csv")
event_summary_out   <- file.path(lineage_directory, "event_summary.csv")

fs::dir_create(logs_directory, recurse = TRUE)
fs::dir_create(qc_directory, recurse = TRUE)
fs::dir_create(lineage_directory, recurse = TRUE)

qc_latest_file <- file.path(qc_directory, glue("{SCRIPT_ID}_qc_latest.csv"))

# Spatial CRS (meters)
target_epsg <- 26918
sf::sf_use_s2(FALSE)

# Reappearance detection (conservative)
compute_reappearance <- TRUE
reappearance_distance_m <- 20  # meters
suppress_reappearance_year <- 1989L

# ------------------------------------------------
# Logging setup (your helpers)
# ------------------------------------------------
logger_helpers_file <- here::here("R", "logger_helpers.R")
if (!file.exists(logger_helpers_file)) stop("Missing logger helpers: ", logger_helpers_file)
source(logger_helpers_file)

log_meta <- start_log(SCRIPT_ID, logs_directory)
on.exit(stop_log(log_meta$log_con), add = TRUE)

log_section(glue("{SCRIPT_ID} - START"))
log_info(glue("Project root: {project_root_directory}"))
log_info(glue("Working dir:  {getwd()}"))
log_info(glue("Log file:     {log_meta$log_file}"))

with_timing <- function(label, expr) {
  start_time <- Sys.time()
  result <- tryCatch(
    eval(expr),
    error = function(e) {
      log_error(glue("{label} FAILED: {conditionMessage(e)}"))
      stop(e)  # ← THIS IS THE CRITICAL LINE
    }
  )
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  log_info(glue("{label} | {round(elapsed, 3)} sec"))
  result
}

qc_timestamp <- gsub(glue("{SCRIPT_ID}_|\\.log$"), "", basename(log_meta$log_file))
qc_run_file  <- file.path(qc_directory, glue("{SCRIPT_ID}_qc_{qc_timestamp}.csv"))

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

qc_row <- function(metric, value, notes = NA_character_) {
  tibble(
    timestamp = Sys.time(),
    script_id = SCRIPT_ID,
    metric    = metric,
    value     = as.character(value),
    notes     = notes
  )
}

safe_write_csv_local <- function(x, path) {
  fs::dir_create(dirname(path), recurse = TRUE)
  readr::write_csv(x, path)
  invisible(path)
}

safe_write_gpkg_layer <- function(sf_object, gpkg_path, layer_name) {
  fs::dir_create(dirname(gpkg_path), recurse = TRUE)
  if (file.exists(gpkg_path)) file.remove(gpkg_path)
  sf::st_write(sf_object, dsn = gpkg_path, layer = layer_name, quiet = TRUE)
  invisible(gpkg_path)
}

safe_write_csv_any <- function(x, path) {
  if (exists("safe_write_csv")) {
    safe_write_csv(x, path)
  } else {
    safe_write_csv_local(x, path)
  }
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

# ------------------------------------------------
# Inputs
# ------------------------------------------------
log_section("INPUTS")

nodes_csv_in  <- file.path(lineage_directory, "superpatch_nodes.csv")
edges_csv_in  <- file.path(lineage_directory, "superpatch_edges.csv")

# Note: geometry comes from the gpkg produced by Script 04
nodes_gpkg_in <- file.path(lineage_directory, "superpatch_nodes.gpkg")

if (!file.exists(nodes_csv_in))  stop("Missing nodes CSV: ", nodes_csv_in)
if (!file.exists(edges_csv_in))  stop("Missing edges CSV: ", edges_csv_in)
if (!file.exists(nodes_gpkg_in)) stop("Missing nodes GPKG: ", nodes_gpkg_in)

log_info(glue("Nodes CSV:  {nodes_csv_in}"))
log_info(glue("Edges CSV:  {edges_csv_in}"))
log_info(glue("Nodes GPKG: {nodes_gpkg_in}"))

# ------------------------------------------------
# Helpers: graph + events
# ------------------------------------------------
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
    left_join(successor_counts,   by = "poly_id") %>%
    mutate(
      n_predecessors = ifelse(is.na(n_predecessors), 0L, as.integer(n_predecessors)),
      n_successors   = ifelse(is.na(n_successors),   0L, as.integer(n_successors))
    )
}

assign_components_and_lineage_ids <- function(edge_table, node_table) {
  if (nrow(edge_table) == 0) {
    node_table$component_id <- NA_integer_
    node_table$lineage_id <- NA_character_
    return(node_table)
  }
  
  undirected_edges <- edge_table %>%
    select(poly_id_from, poly_id_to) %>%
    distinct()
  
  graph_object <- igraph::graph_from_data_frame(
    d = undirected_edges,
    directed = FALSE,
    vertices = node_table %>% select(poly_id) %>% distinct()
  )
  
  membership <- igraph::components(graph_object)$membership
  
  component_table <- tibble(
    poly_id = names(membership),
    component_id = as.integer(membership)
  )
  
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
  
  node_table %>%
    left_join(component_table, by = "poly_id") %>%
    left_join(component_summary %>% select(component_id, lineage_id), by = "component_id")
}

assign_event_types <- function(node_table) {
  max_year <- max(node_table$year, na.rm = TRUE)
  
  node_table %>%
    mutate(
      event_type = case_when(
        year == max_year & n_predecessors >= 1L ~ "right_censored_continuation",
        n_predecessors == 0L                    ~ "first_appearance",
        n_predecessors == 1L & n_successors == 1L ~ "continuation",
        n_predecessors == 1L & n_successors  > 1L ~ "fragmentation",
        n_predecessors  > 1L & n_successors == 1L ~ "merge",
        n_successors == 0L & year != max_year      ~ "disappearance",
        TRUE ~ "complex_transition"
      )
    )
}

mark_reappearance <- function(node_sf, reappearance_distance_m, suppress_year) {
  years_all <- sort(unique(node_sf$year))
  min_year <- min(years_all, na.rm = TRUE)
  
  node_sf$has_prior_proximate <- FALSE
  
  # centroids for speed
  cent <- sf::st_centroid(sf::st_geometry(node_sf))
  node_sf$cent_geom <- cent
  
  for (yr in years_all) {
    if (is.na(yr)) next
    if (yr <= (min_year + 1)) next
    if (yr == suppress_year) next
    
    idx_now <- which(node_sf$year == yr & node_sf$n_predecessors == 0L)
    if (length(idx_now) == 0) next
    
    idx_prior <- which(node_sf$year <= (yr - 2))
    if (length(idx_prior) == 0) next
    
    now_pts   <- sf::st_as_sf(node_sf[idx_now, ],  sf_column_name = "cent_geom")
    prior_pts <- sf::st_as_sf(node_sf[idx_prior,], sf_column_name = "cent_geom")
    
    near_list <- sf::st_is_within_distance(
      sf::st_geometry(now_pts),
      sf::st_geometry(prior_pts),
      dist = reappearance_distance_m
    )
    
    node_sf$has_prior_proximate[idx_now] <- lengths(near_list) > 0
  }
  
  node_sf$cent_geom <- NULL
  node_sf
}

rename_list_fields_to_values <- function(df) {
  nm <- names(df)
  names(df) <- gsub("_list$", "_values", nm)
  df
}

# ------------------------------------------------
# Main
# ------------------------------------------------
log_section("PROCESS")
qc <- tibble()

nodes_tbl <- with_timing("Read nodes CSV", quote({
  readr::read_csv(nodes_csv_in, show_col_types = FALSE)
}))

edges_tbl <- with_timing("Read edges CSV", quote({
  readr::read_csv(edges_csv_in, show_col_types = FALSE)
}))

required_node_cols <- c("poly_id", "year")
required_edge_cols <- c("poly_id_from", "poly_id_to", "year_from", "year_to")

missing_nodes <- setdiff(required_node_cols, names(nodes_tbl))
missing_edges <- setdiff(required_edge_cols, names(edges_tbl))

if (length(missing_nodes) > 0) stop("Nodes table missing required columns: ", paste(missing_nodes, collapse = ", "))
if (length(missing_edges) > 0) stop("Edges table missing required columns: ", paste(missing_edges, collapse = ", "))

qc <- bind_rows(
  qc,
  qc_row("nodes_in", nrow(nodes_tbl)),
  qc_row("edges_in", nrow(edges_tbl)),
  qc_row("years_min", min(nodes_tbl$year, na.rm = TRUE)),
  qc_row("years_max", max(nodes_tbl$year, na.rm = TRUE))
)

nodes_tbl2 <- with_timing("Compute predecessor/successor counts", quote({
  compute_predecessor_successor_counts(edges_tbl, nodes_tbl)
}))

nodes_tbl3 <- with_timing("Assign components + lineage_id", quote({
  assign_components_and_lineage_ids(edges_tbl, nodes_tbl2)
}))

nodes_tbl4 <- with_timing("Assign event types (base)", quote({
  assign_event_types(nodes_tbl3)
}))

qc <- bind_rows(
  qc,
  qc_row("distinct_components", dplyr::n_distinct(nodes_tbl4$component_id, na.rm = TRUE)),
  qc_row("distinct_lineages",   dplyr::n_distinct(nodes_tbl4$lineage_id,   na.rm = TRUE))
)

# Read authoritative geometry nodes from Script 04 output
node_sf <- with_timing("Read nodes GPKG (authoritative geometry)", quote({
  sf_obj <- sf::st_read(nodes_gpkg_in, quiet = TRUE)
  sf_obj <- enforce_target_crs(sf_obj, target_epsg)
  sf_obj <- sf::st_make_valid(sf_obj)
  sf_obj
}))

# Validate join keys exist
if (!all(c("poly_id", "year") %in% names(node_sf))) {
  stop("nodes_gpkg_in is missing poly_id/year. Columns are: ", paste(names(node_sf), collapse = ", "))
}

# Join computed lineage + events onto geometry (by poly_id + year)
node_sf2 <- with_timing("Join lineage + events onto geometry", quote({
  node_sf %>%
    select(-any_of(c("component_id", "lineage_id", "n_predecessors", "n_successors", "event_type", "has_prior_proximate"))) %>%
    left_join(
      nodes_tbl4 %>% select(poly_id, year, component_id, lineage_id, n_predecessors, n_successors, event_type),
      by = c("poly_id", "year")
    )
}))

if (sum(is.na(node_sf2$lineage_id)) > 0) {
  stop("Join produced missing lineage_id values. Check poly_id/year alignment.")
}

n_join_miss <- sum(is.na(node_sf2$lineage_id))
qc <- bind_rows(qc, qc_row("nodes_missing_lineage_after_join", n_join_miss))

# After node_sf2 is created and validated:
node_sf3 <- node_sf2

log_info(glue("checkpoint: after join | node_sf2 exists={exists('node_sf2')} | n={nrow(node_sf2)}"))

mark_reappearance_fast <- function(node_sf, reappearance_distance_m, suppress_year, chunk_size = 2000) {
  stopifnot(inherits(node_sf, "sf"))
  stopifnot(all(c("year", "n_predecessors") %in% names(node_sf)))
  
  years_all <- sort(unique(node_sf$year))
  min_year  <- min(years_all, na.rm = TRUE)
  
  pts <- sf::st_point_on_surface(sf::st_geometry(node_sf))
  idx_by_year <- split(seq_len(nrow(node_sf)), node_sf$year)
  
  has_prior <- rep(FALSE, nrow(node_sf))
  
  for (yr in years_all) {
    if (is.na(yr)) next
    if (yr <= (min_year + 1)) next
    if (yr == suppress_year) next
    
    idx_now <- idx_by_year[[as.character(yr)]]
    idx_now <- idx_now[node_sf$n_predecessors[idx_now] == 0L]
    if (length(idx_now) == 0) next
    
    prior_years <- years_all[years_all <= (yr - 2)]
    if (length(prior_years) == 0) next
    
    idx_prior <- unlist(idx_by_year[as.character(prior_years)], use.names = FALSE)
    if (length(idx_prior) == 0) next
    
    # progress log lives HERE (vars exist here)
    if (exists("log_info")) {
      log_info(glue("reappearance: year={yr} | now={length(idx_now)} | prior={length(idx_prior)}"))
    }
    
    prior_geom <- pts[idx_prior]
    
    chunks <- split(idx_now, ceiling(seq_along(idx_now) / chunk_size))
    for (i in seq_along(chunks)) {
      ch <- chunks[[i]]
      
      if (exists("log_info") && (i %% 10 == 1 || i == length(chunks))) {
        log_info(glue("reappearance: year={yr} | chunk {i}/{length(chunks)} | ch_n={length(ch)}"))
      }
      
      now_buf <- sf::st_buffer(pts[ch], dist = reappearance_distance_m)
      hits <- sf::st_intersects(now_buf, prior_geom, sparse = TRUE)
      has_prior[ch] <- lengths(hits) > 0
    }
  }
  
  node_sf$has_prior_proximate <- has_prior
  node_sf
}


node_sf3 <- with_timing("Compute reappearance flags (buffered intersects)", quote({
  mark_reappearance_fast(
    node_sf3,
    reappearance_distance_m = reappearance_distance_m,
    suppress_year = suppress_reappearance_year
  )
}))

stopifnot(
  inherits(node_sf3, "sf"),
  all(c("poly_id","year","lineage_id","event_type","n_predecessors","n_successors") %in% names(node_sf3))
)

node_sf4 <- rename_list_fields_to_values(node_sf3)
log_info("About to build node_sf4 and nodes_out_tbl...")
stopifnot(exists("node_sf3"))
stopifnot(inherits(node_sf3, "sf"))
stopifnot(all(c("poly_id","year","lineage_id","event_type") %in% names(node_sf3)))

nodes_out_tbl <- node_sf4 %>%
  st_drop_geometry() %>%
  arrange(year, poly_id)

event_summary <- nodes_out_tbl %>%
  count(event_type, name = "n") %>%
  arrange(desc(n))

lineage_summary <- nodes_out_tbl %>%
  filter(!is.na(lineage_id)) %>%
  group_by(lineage_id) %>%
  summarise(
    n_nodes = n(),
    first_year = min(year, na.rm = TRUE),
    last_year  = max(year, na.rm = TRUE),
    n_years_observed = n_distinct(year),
    n_first_appear  = sum(event_type == "first_appearance", na.rm = TRUE),
    n_reappear      = sum(event_type == "reappearance", na.rm = TRUE),
    n_continuation  = sum(event_type == "continuation", na.rm = TRUE),
    n_fragmentation = sum(event_type == "fragmentation", na.rm = TRUE),
    n_merge         = sum(event_type == "merge", na.rm = TRUE),
    n_disappear     = sum(event_type == "disappearance", na.rm = TRUE),
    n_censored      = sum(event_type == "right_censored_continuation", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(first_year, desc(n_nodes), lineage_id)

qc <- bind_rows(
  qc,
  qc_row("event_types_distinct", dplyr::n_distinct(nodes_out_tbl$event_type)),
  qc_row("reappearance_enabled", compute_reappearance),
  qc_row("reappearance_distance_m", reappearance_distance_m)
)

# ---- Guardrail: do not write outputs if process objects don't exist ----------
if (!exists("nodes_out_tbl")) {
  stop("PROCESS failed before creating nodes_out_tbl; aborting before OUTPUTS. See earlier log for the first failure.")
}
if (!exists("node_sf4")) {
  stop("PROCESS failed before creating node_sf4; aborting before OUTPUTS.")
}
if (!exists("lineage_summary") || !exists("event_summary")) {
  stop("PROCESS failed before summaries were created; aborting before OUTPUTS.")
}

# ------------------------------------------------
# Outputs
# ------------------------------------------------
log_section("OUTPUTS")

nodes_csv_out <- file.path(lineage_directory, "superpatch_nodes_with_lineage.csv")
nodes_gpkg_out <- file.path(lineage_directory, "superpatch_nodes_with_lineage.gpkg")
nodes_layer <- "superpatch_nodes_with_lineage"

lineage_summary_out <- file.path(lineage_directory, "lineage_summary.csv")
event_summary_out   <- file.path(lineage_directory, "event_summary.csv")

with_timing("Write nodes CSV", quote({
  safe_write_csv_any(nodes_out_tbl, nodes_csv_out)
}))
log_info(glue("Wrote OK: {nodes_csv_out}"))

with_timing("Write nodes GPKG", quote({
  safe_write_gpkg_layer(node_sf4, nodes_gpkg_out, nodes_layer)
}))
log_info(paste("Wrote:", nodes_gpkg_out, "(layer:", nodes_layer, ")"))

with_timing("Write lineage summary CSV", quote({
  safe_write_csv_any(lineage_summary, lineage_summary_out)
}))
log_info(paste("Wrote:", lineage_summary_out))

with_timing("Write event summary CSV", quote({
  safe_write_csv_any(event_summary, event_summary_out)
}))
log_info(paste("Wrote:", event_summary_out))
# ------------------------------------------------
# QC
# ------------------------------------------------
log_section("QC SUMMARY")

with_timing("Write QC tables", quote({
  safe_write_csv_any(qc, qc_latest_file)
  safe_write_csv_any(qc, qc_run_file)
}))

log_info(glue("QC latest: {qc_latest_file}"))
log_info(glue("QC run:    {qc_run_file}"))

log_section("DONE")
log_info("Script completed successfully.")

