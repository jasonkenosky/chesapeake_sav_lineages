# ==============================================================================
# Script Title: Inventory raw VIMS SAV yearly datasets
# Purpose: Record what raw yearly SAV datasets exist and their contents.
# Author: Jason Kenosky
# Last Updated: 2026-01-02
#
# Description:
#   This script inventories raw VIMS SAV datasets (read-only). It records:
#   - file paths, file sizes, and hashes
#   - inferred survey year from filename
#   - layer names (for geopackages)
#   - feature counts and geometry types
#   - CRS (EPSG when available)
#   - attribute schema (field names and field types)
#   Outputs are QC tables only.
#
# Inputs:
#   config/project_config.yml
#   config/thresholds.yml
#   data_raw/vims_yearly/
#
# Outputs:
#   qc/01_ingest_inventory_raw_years__raw_inventory_latest.csv
#   qc/01_ingest_inventory_raw_years__raw_inventory__<run_id>.csv
#   qc/01_ingest_inventory_raw_years__raw_schema_latest.csv
#   qc/01_ingest_inventory_raw_years__raw_schema__<run_id>.csv
#   qc/01_ingest_inventory_raw_years__year_coverage_latest.csv
#   qc/01_ingest_inventory_raw_years__year_coverage__<run_id>.csv
#
# Notes:
#   - Raw files are never modified.
#   - Package versions are logged by logger_helpers.
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

SCRIPT_ID <- "p0_01_ingest_inventory_raw_years"
script_start_time <- Sys.time()

# ------------------------------------------------
# 1. Load packages
# ------------------------------------------------
suppressPackageStartupMessages({
  library(here)
  library(yaml)
  library(fs)
  library(glue)
  library(lubridate)
  
  library(sf)
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(digest)
})

sf::sf_use_s2(FALSE)

# ------------------------------------------------
# 2. Script configuration (directories, parameters)
# ------------------------------------------------
DIR_ROOT   <- here::here()
DIR_CONFIG <- file.path(DIR_ROOT, "config")
DIR_R      <- file.path(DIR_ROOT, "R")
DIR_LOGS   <- file.path(DIR_ROOT, "logs")
DIR_QC     <- file.path(DIR_ROOT, "qc")

dir.create(DIR_LOGS, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_QC,   showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------
# 3. Logging setup (helpers only)
# ------------------------------------------------
FILE_LOG_HELPERS <- here::here("R", "logger_helpers.R")
if (!file.exists(FILE_LOG_HELPERS)) {
  stop("Missing logger helpers: ", FILE_LOG_HELPERS)
}
source(FILE_LOG_HELPERS)

log_meta <- start_log(SCRIPT_ID, DIR_LOGS)
on.exit(stop_log(log_meta$log_con), add = TRUE)

log_section(glue("{SCRIPT_ID} - START"))
log_info(glue("Project root: {DIR_ROOT}"))
log_info(glue("Working dir:  {getwd()}"))
log_info(glue("Log file:     {log_meta$log_file}"))

# ------------------------------------------------
# 4. Inputs (validate early)
# ------------------------------------------------
log_section("INPUTS")

FILE_PROJECT_CONFIG <- file.path(DIR_CONFIG, "project_config.yml")
FILE_THRESHOLDS     <- file.path(DIR_CONFIG, "thresholds.yml")

if (!file.exists(FILE_PROJECT_CONFIG)) stop("Missing: ", FILE_PROJECT_CONFIG)
if (!file.exists(FILE_THRESHOLDS))     stop("Missing: ", FILE_THRESHOLDS)

project_config  <- yaml::read_yaml(FILE_PROJECT_CONFIG)
thresholds_config <- yaml::read_yaml(FILE_THRESHOLDS)

# ---- Spatial extent sanity checks -----------------------------------

if (is.null(project_config$extent_sanity$expected_easting_min) ||
    is.null(project_config$extent_sanity$expected_easting_max)) {
  stop(
    "Missing extent_sanity expected_easting_min / expected_easting_max ",
    "in project_config.yml"
  )
}

expected_easting_min <- as.numeric(project_config$extent_sanity$expected_easting_min)
expected_easting_max <- as.numeric(project_config$extent_sanity$expected_easting_max)

if (is.na(expected_easting_min) || is.na(expected_easting_max)) {
  stop("extent_sanity values must be numeric")
}

if (expected_easting_min >= expected_easting_max) {
  stop("extent_sanity expected_easting_min must be < expected_easting_max")
}

log_info(
  glue(
    "Extent sanity easting range: {expected_easting_min}–{expected_easting_max}"
  )
)

DIR_RAW <- here::here(project_config$directories$data_raw_vims_yearly)

if (!dir.exists(DIR_RAW)) stop("Missing raw data directory: ", DIR_RAW)

log_info(glue("Raw directory: {DIR_RAW}"))

first_year <- project_config$survey_years$first_year
last_year  <- project_config$survey_years$last_year

excluded_years <- project_config$survey_years$excluded_years
missing_survey_years <- project_config$survey_years$missing_survey_years

log_info(glue("Configured year range: {first_year}–{last_year}"))
log_info(glue("Excluded years: {paste(excluded_years, collapse = ', ')}"))
log_info(glue("Missing survey years: {paste(missing_survey_years, collapse = ', ')}"))

# ------------------------------------------------
# 5. Helpers (script-local only)
# ------------------------------------------------
safe_write_csv <- function(data_table, file_path) {
  fs::dir_create(dirname(file_path), recurse = TRUE)
  readr::write_csv(data_table, file_path)
  invisible(file_path)
}

qc_row <- function(metric, value, notes = NA_character_) {
  tibble::tibble(
    timestamp = as.character(Sys.time()),
    script_id = SCRIPT_ID,
    metric    = metric,
    value     = as.character(value),
    notes     = notes
  )
}

write_qc_table <- function(data_table, file_latest, file_run) {
  safe_write_csv(data_table, file_latest)
  safe_write_csv(data_table, file_run)
  invisible(TRUE)
}

extract_year_from_filename <- function(file_name, first_year, last_year) {
  year_4 <- stringr::str_extract(file_name, "(19\\d{2}|20\\d{2})")
  if (!is.na(year_4)) {
    year_value <- as.integer(year_4)
    if (year_value >= first_year && year_value <= last_year) return(year_value)
  }
  
  year_2 <- stringr::str_match(file_name, "(?i)(beds|sav|veg)[^0-9]*([0-9]{2})")[, 3]
  if (!is.na(year_2)) {
    yy <- as.integer(year_2)
    year_value <- if (yy >= 84) 1900 + yy else 2000 + yy
    if (year_value >= first_year && year_value <= last_year) return(year_value)
  }
  
  NA_integer_
}

summarize_field_types <- function(sf_object) {
  attributes <- sf::st_drop_geometry(sf_object)
  tibble::tibble(
    field_name = names(attributes),
    field_type = vapply(attributes, function(x) paste(class(x), collapse = "|"), character(1))
  )
}

# ------------------------------------------------
# 6. Main process
# ------------------------------------------------
log_section("PROCESS")

raw_files <- fs::dir_ls(
  path = DIR_RAW,
  recurse = TRUE,
  type = "file",
  regexp = "\\.(shp|gpkg)$"
)

raw_files <- raw_files[!stringr::str_detect(raw_files, "/\\.")]
raw_files <- sort(raw_files)

log_info(glue("Discovered {length(raw_files)} candidate files."))

if (length(raw_files) == 0) {
  stop("No .shp or .gpkg files found under: ", DIR_RAW)
}

inventory_table <- tibble::tibble()
schema_table <- tibble::tibble()

for (index in seq_along(raw_files)) {
  file_path <- raw_files[[index]]
  file_name <- fs::path_file(file_path)
  file_extension <- tolower(fs::path_ext(file_path))
  
  inferred_year <- extract_year_from_filename(file_name, first_year, last_year)
  
  file_size_bytes <- as.numeric(fs::file_info(file_path)$size)
  file_md5 <- digest::digest(file = file_path, algo = "md5")
  
  log_info(glue("[{index}/{length(raw_files)}] Reading: {file_name}"))
  
  # Determine layers
  layer_names <- NA_character_
  if (file_extension == "gpkg") {
    layer_info <- tryCatch(sf::st_layers(file_path), error = function(e) NULL)
    if (!is.null(layer_info) && nrow(layer_info) > 0) {
      layer_names <- layer_info$name
    }
  }
  
  for (layer_name in layer_names) {
    sf_object <- tryCatch(
      {
        if (is.na(layer_name)) {
          sf::st_read(file_path, quiet = TRUE, stringsAsFactors = FALSE)
        } else {
          sf::st_read(file_path, layer = layer_name, quiet = TRUE, stringsAsFactors = FALSE)
        }
      },
      error = function(e) {
        log_warn(glue("Failed to read {file_name}: {conditionMessage(e)}"))
        NULL
      }
    )
    
    if (is.null(sf_object)) next
    
    feature_count <- nrow(sf_object)
    geometry_types <- unique(as.character(sf::st_geometry_type(sf_object)))
    geometry_type <- paste(sort(geometry_types), collapse = "|")
    
    crs_object <- sf::st_crs(sf_object)
    crs_epsg <- suppressWarnings(as.integer(crs_object$epsg))
    
    bbox <- sf::st_bbox(sf_object)
    
    bbox_xmin <- as.numeric(bbox["xmin"])
    bbox_ymin <- as.numeric(bbox["ymin"])
    bbox_xmax <- as.numeric(bbox["xmax"])
    bbox_ymax <- as.numeric(bbox["ymax"])
    
    bbox_width_m  <- bbox_xmax - bbox_xmin
    bbox_height_m <- bbox_ymax - bbox_ymin
    
    crs_is_missing <- is.na(crs_epsg)
    
    bbox_has_negative_easting <- bbox_xmin < 0
    
    bbox_easting_out_of_range <- (bbox_xmin < expected_easting_min) |
      (bbox_xmax > expected_easting_max)
    
    bbox_text <- glue("{bbox_xmin},{bbox_ymin},{bbox_xmax},{bbox_ymax}")
    
    one_inventory_row <- tibble::tibble(
      source_file = file_name,
      source_path = as.character(file_path),
      layer = ifelse(is.na(layer_name), NA_character_, layer_name),
      inferred_year = inferred_year,
      file_extension = file_extension,
      file_size_bytes = file_size_bytes,
      file_md5 = file_md5,
      feature_count = as.integer(feature_count),
      geometry_type = geometry_type,
      crs_epsg = crs_epsg,
      crs_is_missing = crs_is_missing,
      bbox = as.character(bbox_text),
      bbox_xmin = bbox_xmin,
      bbox_ymin = bbox_ymin,
      bbox_xmax = bbox_xmax,
      bbox_ymax = bbox_ymax,
      bbox_width_m = bbox_width_m,
      bbox_height_m = bbox_height_m,
      bbox_has_negative_easting = bbox_has_negative_easting,
      bbox_easting_out_of_range = bbox_easting_out_of_range
    )
    
    inventory_table <- dplyr::bind_rows(inventory_table, one_inventory_row)
    
    field_summary <- summarize_field_types(sf_object) %>%
      dplyr::mutate(
        source_file = file_name,
        source_path = as.character(file_path),
        layer = ifelse(is.na(layer_name), NA_character_, layer_name),
        inferred_year = inferred_year
      )
    
    schema_table <- dplyr::bind_rows(schema_table, field_summary)
  }
}

inventory_table <- inventory_table %>%
  dplyr::arrange(inferred_year, source_file, layer)

anomalies_table <- inventory_table %>%
  dplyr::filter(crs_is_missing | bbox_easting_out_of_range) %>%
  dplyr::arrange(inferred_year, source_file, layer)

log_info(glue("Anomalies flagged: {nrow(anomalies_table)} rows"))

schema_table <- schema_table %>%
  dplyr::arrange(inferred_year, source_file, layer, field_name)

year_coverage_table <- tibble::tibble(year = seq(first_year, last_year)) %>%
  dplyr::mutate(
    is_excluded_year = year %in% excluded_years,
    is_missing_survey_year = year %in% missing_survey_years,
    is_expected_year = !is_excluded_year & !is_missing_survey_year,
    has_dataset = year %in% inventory_table$inferred_year
  )

unknown_year_count <- inventory_table %>% dplyr::filter(is.na(inferred_year)) %>% nrow()
if (unknown_year_count > 0) {
  log_warn(glue("Files with no inferred year: {unknown_year_count}. Rename or add a mapping rule."))
}

# ------------------------------------------------
# 7. Write outputs (QC only)
# ------------------------------------------------
log_section("OUTPUTS")

run_id <- log_meta$run_id

FILE_ANOMALIES_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}__anomalies_latest.csv"))
FILE_ANOMALIES_RUN    <- file.path(DIR_QC, glue("{SCRIPT_ID}__anomalies__{run_id}.csv"))

write_qc_table(anomalies_table, FILE_ANOMALIES_LATEST, FILE_ANOMALIES_RUN)
log_info(glue("Wrote: {FILE_ANOMALIES_LATEST}"))

FILE_INVENTORY_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}__raw_inventory_latest.csv"))
FILE_INVENTORY_RUN    <- file.path(DIR_QC, glue("{SCRIPT_ID}__raw_inventory__{run_id}.csv"))

FILE_SCHEMA_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}__raw_schema_latest.csv"))
FILE_SCHEMA_RUN    <- file.path(DIR_QC, glue("{SCRIPT_ID}__raw_schema__{run_id}.csv"))

FILE_COVERAGE_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}__year_coverage_latest.csv"))
FILE_COVERAGE_RUN    <- file.path(DIR_QC, glue("{SCRIPT_ID}__year_coverage__{run_id}.csv"))

write_qc_table(inventory_table, FILE_INVENTORY_LATEST, FILE_INVENTORY_RUN)
write_qc_table(schema_table,    FILE_SCHEMA_LATEST,    FILE_SCHEMA_RUN)
write_qc_table(year_coverage_table, FILE_COVERAGE_LATEST, FILE_COVERAGE_RUN)
write_qc_table(anomalies_table, FILE_ANOMALIES_LATEST, FILE_ANOMALIES_RUN)

log_info(glue("Wrote: {FILE_ANOMALIES_LATEST}"))
log_info(glue("Wrote: {FILE_INVENTORY_LATEST}"))
log_info(glue("Wrote: {FILE_SCHEMA_LATEST}"))
log_info(glue("Wrote: {FILE_COVERAGE_LATEST}"))

# ------------------------------------------------
# 8. QC summary (always write)
# ------------------------------------------------
log_section("QC SUMMARY")

qc_summary <- dplyr::bind_rows(
  qc_row("raw_files_discovered", length(raw_files)),
  qc_row("inventory_rows", nrow(inventory_table)),
  qc_row("schema_rows", nrow(schema_table)),
  qc_row("unknown_year_rows", unknown_year_count),
  qc_row("anomaly_rows", nrow(anomalies_table))
)

FILE_QC_SUMMARY_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}__qc_summary_latest.csv"))
FILE_QC_SUMMARY_RUN    <- file.path(DIR_QC, glue("{SCRIPT_ID}__qc_summary__{run_id}.csv"))

write_qc_table(qc_summary, FILE_QC_SUMMARY_LATEST, FILE_QC_SUMMARY_RUN)

log_info("Script completed successfully.")

