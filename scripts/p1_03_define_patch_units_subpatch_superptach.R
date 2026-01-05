# ==============================================================================
# Script Title: p1_03_define_superpatch_units
# Purpose: Build yearly superpatches by dissolving touching SAV polygons within
#          year and density class, then recalculate patch metrics on superpatches.
# Author: Jason Kenosky
# Last Updated: 2026-01-02
#
# Description:
#   - Reads cleaned yearly SAV polygon layers from p0_02.
#   - Defines superpatch units as polygons that touch AND share the same density.
#   - Recalculates metrics on dissolved superpatch geometry.
#   - Writes one .gpkg per year + QC tables.
#
# Inputs:
#   data_processed/vims_sav_cleaned/sav_cleaned_YYYY.gpkg
#
# Outputs:
#   data_processed/vims_superpatches/superpatches_YYYY.gpkg
#   qc/<SCRIPT_ID>_qc_latest.csv  (+ timestamped)
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
  library(lwgeom)   # for st_perimeter in projected CRS + safe geometry ops
})

# ------------------------------------------------
# 2. Script configuration (directories, parameters)
# ------------------------------------------------
SCRIPT_ID <- "p1_03_define_superpatch_units"

project_root_directory <- here::here()

data_processed_directory <- file.path(project_root_directory, "data_processed")
outputs_directory        <- file.path(project_root_directory, "outputs")

logs_directory <- file.path(project_root_directory, "logs")
qc_directory   <- file.path(project_root_directory, "qc")

cleaned_sav_directory <- file.path(data_processed_directory, "vims_sav_cleaned")
superpatch_directory  <- file.path(data_processed_directory, "vims_superpatches")

fs::dir_create(superpatch_directory, recurse = TRUE)
fs::dir_create(logs_directory, recurse = TRUE)
fs::dir_create(qc_directory, recurse = TRUE)

qc_latest_file <- file.path(qc_directory, glue("{SCRIPT_ID}_qc_latest.csv"))

# CRS (should already be 26918 from script 02, but we enforce)
target_epsg <- 26918

# ------------------------------------------------
# 3. Logging setup (helpers only)
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

if (!dir_exists(cleaned_sav_directory)) {
  log_error(glue("Missing cleaned SAV directory: {cleaned_sav_directory}"))
  stop("Missing directory: ", cleaned_sav_directory)
}

cleaned_gpkg_files <- fs::dir_ls(cleaned_sav_directory, type = "file", regexp = "\\.gpkg$")
if (length(cleaned_gpkg_files) == 0) {
  log_error("No cleaned gpkg files found.")
  stop("No gpkg files found in: ", cleaned_sav_directory)
}

log_info(glue("Cleaned gpkg files discovered: {length(cleaned_gpkg_files)}"))

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

build_connected_groups <- function(adjacency_list) {
  polygon_count <- length(adjacency_list)
  group_id <- rep(NA_integer_, polygon_count)
  current_group <- 0L
  
  for (polygon_index in seq_len(polygon_count)) {
    if (!is.na(group_id[polygon_index])) next
    
    current_group <- current_group + 1L
    queue <- polygon_index
    group_id[polygon_index] <- current_group
    
    while (length(queue) > 0) {
      current_node <- queue[[1]]
      queue <- queue[-1]
      
      neighbor_nodes <- adjacency_list[[current_node]]
      if (length(neighbor_nodes) == 0) next
      
      unassigned_neighbors <- neighbor_nodes[is.na(group_id[neighbor_nodes])]
      if (length(unassigned_neighbors) == 0) next
      
      group_id[unassigned_neighbors] <- current_group
      queue <- c(queue, unassigned_neighbors)
    }
  }
  
  group_id
}

make_superpatches_from_subpatches <- function(subpatch_sf, year_value) {
  
  # ---- Geometry hygiene ------------------------------------------------------
  # Compute subpatch area for weighting
  subpatch_sf$subpatch_area_m2 <- as.numeric(sf::st_area(sf::st_geometry(subpatch_sf)))
  
  subpatch_sf <- sf::st_make_valid(subpatch_sf)
  
  # Some files can contain geometry collections after make_valid
  subpatch_sf <- sf::st_collection_extract(subpatch_sf, "POLYGON", warn = FALSE)
  
  # Compute subpatch area for weighting (robust to geom/geometry column name)
  subpatch_sf$subpatch_area_m2 <- as.numeric(sf::st_area(sf::st_geometry(subpatch_sf)))
  
  # ---- Adjacency (touch OR overlap) -----------------------------------------
  # If you only want boundary-touch, swap st_intersects -> st_touches
  adjacency_list <- sf::st_touches(subpatch_sf)
  
  superpatch_group_id <- build_connected_groups(adjacency_list)
  
  subpatch_sf <- subpatch_sf |>
    dplyr::mutate(superpatch_group_id = superpatch_group_id)
  
  # ---- Dissolve + density recompute -----------------------------------------
  superpatch_sf <- subpatch_sf |>
    dplyr::group_by(superpatch_group_id) |>
    dplyr::summarise(
      year = year_value,
      subpatch_count = dplyr::n(),
      
      # optional provenance fields
      bedid_list   = paste(stats::na.omit(unique(bedid)), collapse = ";"),
      quadid_list  = paste(stats::na.omit(unique(quadid)), collapse = ";"),
      cbpseg_list  = paste(stats::na.omit(unique(cbpseg)), collapse = ";"),
      zone_list    = paste(stats::na.omit(unique(zone)), collapse = ";"),
      state_list   = paste(stats::na.omit(unique(state)), collapse = ";"),
      
      # density summaries
      density_area_weighted_mean =
        stats::weighted.mean(density, w = subpatch_area_m2, na.rm = TRUE),
      
      density_simple_mean = mean(density, na.rm = TRUE),
      
      # majority/“mode” class (ties break by first)
      density_majority_class = {
        density_nonmissing <- density[!is.na(density)]
        if (length(density_nonmissing) == 0) NA_real_ else {
          density_table <- sort(table(density_nonmissing), decreasing = TRUE)
          as.numeric(names(density_table)[1])
        }
      },
      
      .groups = "drop"
    )
  
  # Choose one density to carry forward as the "superpatch density"
  # (This keeps downstream code simple.)
  superpatch_sf <- superpatch_sf |>
    dplyr::mutate(
      density = density_area_weighted_mean
    )
  
  # Final IDs + superpatch metrics
  superpatch_sf <- superpatch_sf |>
    dplyr::mutate(
      superpatch_id = glue::glue("sp_{year_value}_{superpatch_group_id}"),
      superpatch_area_m2 = as.numeric(sf::st_area(sf::st_geometry(superpatch_sf)))
    )
  
  superpatch_sf
}


# Build superpatch ids deterministically within a year:
# order by density, then descending area, then centroid x/y
assign_superpatch_ids <- function(superpatch_sf, year_value) {
  centroids <- sf::st_centroid(sf::st_geometry(superpatch_sf))
  centroid_coordinates <- sf::st_coordinates(centroids)
  
  superpatch_sf <- superpatch_sf %>%
    mutate(
      centroid_x = centroid_coordinates[, 1],
      centroid_y = centroid_coordinates[, 2]
    ) %>%
    arrange(density, desc(area_m2), centroid_x, centroid_y) %>%
    mutate(
      superpatch_id = glue("sp_{year_value}_{sprintf('%05d', dplyr::row_number())}")
    ) %>%
    select(-centroid_x, -centroid_y)
  
  superpatch_sf
}

# Metrics (projected CRS in meters)
compute_patch_metrics <- function(patch_sf) {
  patch_area_m2 <- as.numeric(sf::st_area(patch_sf))
  patch_perimeter_m <- as.numeric(lwgeom::st_perimeter(sf::st_geometry(patch_sf)))
  
  # shape index (common landscape ecology form):
  # SI = perimeter / (2*sqrt(pi*area))
  shape_index <- patch_perimeter_m / (2 * sqrt(pi * patch_area_m2))
  
  # fractal dimension (common approximation):
  # FD = 2 * log(0.25*P) / log(A)
  # guard against zeros
  safe_area <- ifelse(patch_area_m2 <= 0, NA_real_, patch_area_m2)
  safe_perimeter <- ifelse(patch_perimeter_m <= 0, NA_real_, patch_perimeter_m)
  
  fractal_dimension <- 2 * log(0.25 * safe_perimeter) / log(safe_area)
  
  # edge density (perimeter per area) scaled to per-hectare if you want:
  # here: meters per m^2 (unit-consistent; you can rescale later)
  edge_density <- safe_perimeter / safe_area
  
  patch_sf %>%
    mutate(
      area_m2           = patch_area_m2,
      perimeter_m       = patch_perimeter_m,
      shape_index       = shape_index,
      fractal_dimension = fractal_dimension,
      edge_density      = edge_density
    )
}



# ------------------------------------------------
# 6. Main process
# ------------------------------------------------
log_section("PROCESS")

sf::sf_use_s2(FALSE)  # important for planar adjacency in EPSG:26918

qc_table <- tibble::tibble()

qc_table <- bind_rows(
  qc_table,
  make_qc_row("cleaned_files_discovered", length(cleaned_gpkg_files))
)

for (cleaned_gpkg_path in cleaned_gpkg_files) {
  
  year_value <- extract_year_from_filename(cleaned_gpkg_path)
  if (is.na(year_value)) {
    log_warn(glue("Skipping (no 4-digit year in filename): {basename(cleaned_gpkg_path)}"))
    qc_table <- bind_rows(qc_table, make_qc_row("skip_no_year", basename(cleaned_gpkg_path)))
    next
  }
  
  log_section(glue("YEAR {year_value}"))
  
  cleaned_year_sf <- with_timing(glue("Read cleaned gpkg ({year_value})"), quote({
    sf::st_read(cleaned_gpkg_path, quiet = TRUE)
  }))
  
  qc_table <- bind_rows(
    qc_table,
    make_qc_row(glue("year_{year_value}_rows_cleaned"), nrow(cleaned_year_sf))
  )
  
  superpatch_year_sf <- with_timing(glue("Build superpatches ({year_value})"), quote({
    make_superpatches_from_subpatches(cleaned_year_sf, year_value)
  }))
  
  superpatch_year_sf <- with_timing(glue("Compute superpatch metrics ({year_value})"), quote({
    compute_patch_metrics(superpatch_year_sf)
  }))
  
  superpatch_year_sf <- with_timing(glue("Assign superpatch IDs ({year_value})"), quote({
    assign_superpatch_ids(superpatch_year_sf, year_value)
  }))
  
  qc_table <- bind_rows(
    qc_table,
    make_qc_row(glue("year_{year_value}_rows_superpatch"), nrow(superpatch_year_sf)),
    make_qc_row(glue("year_{year_value}_area_total_m2"), round(sum(superpatch_year_sf$area_m2, na.rm = TRUE), 3))
  )
  
  output_gpkg_path  <- file.path(superpatch_directory, glue("superpatches_{year_value}.gpkg"))
  output_layer_name <- glue("superpatches_{year_value}")
  
  with_timing(glue("Write superpatch gpkg ({year_value})"), quote({
    safe_write_gpkg_layer(superpatch_year_sf, output_gpkg_path, output_layer_name)
  }))
  
  log_info(glue("Wrote: {output_gpkg_path} (layer: {output_layer_name})"))
}

# ------------------------------------------------
# 7. QC summary (always write)
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
# 8. Done
# ------------------------------------------------
log_section("DONE")
log_info("Script completed successfully.")

