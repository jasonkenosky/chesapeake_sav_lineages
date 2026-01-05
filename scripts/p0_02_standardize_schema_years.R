# ==============================================================================
# Script Title: p0_02_standardize_schema_years
# Purpose: Standardize SAV polygon schema across years, remove cartography artifacts,
#          and clip all layers to a consistent Chesapeake Bay + Atlantic coast bbox.
# Author: Jason Kenosky
# Last Updated: 2026-01-02
#
# Description:
#   - Produces a consistent, minimal schema for yearly SAV polygons (VIMS-style).
#   - Removes rows representing quad/CBPSEG outline artifacts (BEDID NA + DENSITY 0).
#   - Clips to a shared bbox extent (NOT Maryland-only).
#
# Inputs:
#   data_raw/vims_sav/<yearly layers>  (shp/gpkg/etc)
#
# Outputs:
#   data_processed/vims_sav_cleaned/sav_cleaned_YYYY.gpkg
#   outputs/qc/<SCRIPT_ID>_qc_latest.csv  (+ timestamped)
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
  library(janitor)
  library(tibble)
  library(sf)
  library(readr)
})

# ------------------------------------------------
# 2. Script configuration (directories, parameters)
# ------------------------------------------------
SCRIPT_ID <- "p0_02_standardize_schema_years"

DIR_ROOT <- here::here()

DIR_DATA_RAW       <- file.path(DIR_ROOT, "data_raw")
DIR_DATA_PROCESSED <- file.path(DIR_ROOT, "data_processed")
DIR_OUTPUTS        <- file.path(DIR_ROOT, "outputs")
DIR_LOGS           <- file.path(DIR_ROOT, "logs")
DIR_QC             <- file.path(DIR_ROOT, "qc")

# --- Script-specific IO -------------------------------------------------------
# Raw yearly SAV layers live here (adjust if needed)
DIR_IN_SAV <- file.path(DIR_DATA_RAW, "vims_yearly")

# Cleaned yearly outputs here (gpkg)
DIR_OUT_SAV <- file.path(DIR_DATA_PROCESSED, "vims_sav_cleaned")
fs::dir_create(DIR_OUT_SAV, recurse = TRUE)

# Ensure core directories exist
fs::dir_create(DIR_DATA_PROCESSED, recurse = TRUE)
fs::dir_create(DIR_OUTPUTS, recurse = TRUE)
fs::dir_create(DIR_LOGS, recurse = TRUE)
fs::dir_create(DIR_QC, recurse = TRUE)

# QC file (stable "latest")
FILE_QC_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}_qc_latest.csv"))

# CRS target (Maryland State Plane meters; what you’ve been using)
CRS_TARGET <- 26918

# Clip bbox (xmin, ymin, xmax, ymax) — in CRS_TARGET units
CLIP_BBOX <- c(
  xmin = 278135.558732842,
  ymin = 4059348.24996215,
  xmax = 500032.8531475,
  ymax = 4400768.50628012
)

# ------------------------------------------------
# 3. Logging setup (helpers only)
# ------------------------------------------------
FILE_LOG_HELPERS <- here::here("R", "logger_helpers.R")
if (!file.exists(FILE_LOG_HELPERS)) stop("Missing logger helpers: ", FILE_LOG_HELPERS)
source(FILE_LOG_HELPERS)

# ---- Timing helper fallback --------------------------------------------------
if (!exists("with_timing")) {
  with_timing <- function(label, expr) {
    start_time <- Sys.time()
    result <- eval(expr)
    end_time <- Sys.time()
    elapsed_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    if (exists("log_info")) {
      log_info(glue("{label} | {round(elapsed_seconds, 3)} sec"))
    } else {
      message(glue("{label} | {round(elapsed_seconds, 3)} sec"))
    }
    
    result
  }
}

log_meta <- start_log(SCRIPT_ID, DIR_LOGS)
on.exit(stop_log(log_meta$log_con), add = TRUE)

log_section(glue("{SCRIPT_ID} - START"))
log_info(glue("Project root: {DIR_ROOT}"))
log_info(glue("Working dir:  {getwd()}"))
log_info(glue("Log file:     {log_meta$log_file}"))

# Timestamp-aligned QC run file
qc_timestamp <- gsub(glue("{SCRIPT_ID}_|\\.log$"), "", basename(log_meta$log_file))
FILE_QC_RUN <- file.path(DIR_QC, glue("{SCRIPT_ID}_qc_{qc_timestamp}.csv"))

# ------------------------------------------------
# 4. Inputs (validate early)
# ------------------------------------------------
log_section("INPUTS")

if (!dir_exists(DIR_IN_SAV)) {
  log_error(glue("Missing input directory: {DIR_IN_SAV}"))
  stop("Missing input directory: ", DIR_IN_SAV)
}
log_info(glue("Input folder: {DIR_IN_SAV}"))

# You can tighten this pattern if needed (e.g., only .shp or only .gpkg)
in_files <- fs::dir_ls(DIR_IN_SAV, recurse = TRUE, type = "file")
in_files <- in_files[grepl("\\.(shp|gpkg|geojson)$", in_files, ignore.case = TRUE)]

if (length(in_files) == 0) {
  log_error("No spatial files found in input folder (shp/gpkg/geojson).")
  stop("No spatial files found in: ", DIR_IN_SAV)
}
log_info(glue("Spatial files discovered: {length(in_files)}"))

# ------------------------------------------------
# 5. Helpers (script-local only)
# ------------------------------------------------
qc_row <- function(metric, value, notes = NA_character_) {
  tibble::tibble(
    timestamp = Sys.time(),
    script_id = SCRIPT_ID,
    metric    = metric,
    value     = as.character(value),
    notes     = notes
  )
}

write_qc <- function(qc_tbl, file_latest, file_run) {
  fs::dir_create(dirname(file_latest), recurse = TRUE)
  readr::write_csv(qc_tbl, file_latest)
  readr::write_csv(qc_tbl, file_run)
  invisible(TRUE)
}

# Extract a 2- or 4-digit year from filename (beds06_polygon, beds1984, sav_2012, etc.)
extract_year <- function(file_path) {
  
  filename <- basename(file_path)
  
  four_digit_year_match <- regmatches(filename, regexpr("(19|20)\\d{2}", filename))
  if (length(four_digit_year_match) == 1 && nchar(four_digit_year_match) == 4) {
    return(as.integer(four_digit_year_match))
  }
  
  two_digit_year_match <- regmatches(filename, regexpr("(?<!\\d)\\d{2}(?!\\d)", filename, perl = TRUE))
  if (length(two_digit_year_match) == 1 && nchar(two_digit_year_match) == 2) {
    
    two_digit_year_value <- as.integer(two_digit_year_match)
    
    inferred_year <- ifelse(two_digit_year_value >= 84,
                            1900 + two_digit_year_value,
                            2000 + two_digit_year_value)
    
    return(as.integer(inferred_year))
  }
  
  NA_integer_
}
# Rename if present
rename_if_present <- function(sf_obj, from, to) {
  nms <- names(sf_obj)
  if (from %in% nms && !(to %in% nms)) {
    sf_obj <- dplyr::rename(sf_obj, !!to := !!rlang::sym(from))
  }
  sf_obj
}

# Standardize key field names to canonical lowercase set
standardize_schema <- function(x) {
  names(x) <- tolower(names(x))
  
  # common variants -> canonical
  x <- rename_if_present(x, "st", "state")
  x <- rename_if_present(x, "state_", "state")
  x <- rename_if_present(x, "zone_", "zone")
  x <- rename_if_present(x, "quad_id", "quadid")
  x <- rename_if_present(x, "quad", "quadid")
  x <- rename_if_present(x, "dens", "density")
  x <- rename_if_present(x, "densit", "density")
  
  x <- x %>%
    mutate(
      density = suppressWarnings(as.numeric(density)),
      bedid   = na_if(as.character(bedid), "")
    )
  
  # Keep only the requested columns (plus geometry)
  keep <- c("bedid", "quadid", "density", "state", "cbpseg", "zone")
  x <- dplyr::select(x, dplyr::any_of(keep), geometry)
  
  x
}

# Clip polygon (bbox) in CRS_TARGET
make_clip_poly <- function() {
  bb <- sf::st_bbox(CLIP_BBOX, crs = sf::st_crs(CRS_TARGET))
  sf::st_as_sfc(bb)
}

# Safe gpkg write (one layer per file)
safe_write_gpkg <- function(x, path, layer) {
  fs::dir_create(dirname(path), recurse = TRUE)
  if (file.exists(path)) file.remove(path)
  sf::st_write(x, dsn = path, layer = layer, quiet = TRUE)
  invisible(path)
}

# ------------------------------------------------
# 6. Main process
# ------------------------------------------------
log_section("PROCESS")

qc_table <- tibble::tibble()

clip_bbox_object <- sf::st_bbox(CLIP_BBOX, crs = sf::st_crs(CRS_TARGET))

# Process each input file
for (input_file_path in in_files) {
  
  file_basename <- basename(input_file_path)
  parsed_year <- extract_year(input_file_path)
  
  if (is.na(parsed_year)) {
    log_warn(glue("Skipping (no year parsed): {file_basename}"))
    qc_table <- bind_rows(qc_table, qc_row("skip_no_year", file_basename))
    next
  }
  
  log_section(glue("YEAR {parsed_year}"))
  log_info(glue("Input file: {input_file_path}"))
  
  per_file_success <- tryCatch({
    
    # ---- Read ---------------------------------------------------------------
    sav_layer_sf <- with_timing(glue("Read {file_basename}"), quote({
      sf::st_read(input_file_path, quiet = TRUE)
    }))
    
    qc_table <- bind_rows(
      qc_table,
      qc_row(glue("year_{parsed_year}_rows_raw"), nrow(sav_layer_sf), file_basename),
      qc_row(glue("year_{parsed_year}_cols_raw"), ncol(sav_layer_sf), file_basename)
    )
    
    # ---- CRS handling --------------------------------------------------------
    input_crs <- sf::st_crs(sav_layer_sf)
    
    if (is.na(input_crs)) {
      log_warn(glue("CRS missing for {file_basename}. Assigning EPSG:{CRS_TARGET}."))
      sf::st_crs(sav_layer_sf) <- sf::st_crs(CRS_TARGET)
    } else if (!isTRUE(input_crs$epsg == CRS_TARGET)) {
      log_info(glue("Transforming CRS EPSG:{input_crs$epsg} -> EPSG:{CRS_TARGET}"))
      sav_layer_sf <- sf::st_transform(sav_layer_sf, CRS_TARGET)
    }
    
    # ---- Geometry validity (before clip) ------------------------------------
    sav_layer_sf <- with_timing("Make valid geometry", quote({
      sf::st_make_valid(sav_layer_sf)
    }))
    
    # ---- Clip (bbox crop; robust) -------------------------------------------
    sav_layer_sf <- with_timing("Clip to bbox (crop)", quote({
      sf::st_crop(sav_layer_sf, clip_bbox_object)
    }))
    
    qc_table <- bind_rows(
      qc_table,
      qc_row(glue("year_{parsed_year}_rows_after_clip"), nrow(sav_layer_sf))
    )
    
    # ---- Standardize schema --------------------------------------------------
    sav_layer_sf <- with_timing("Standardize schema", quote({
      standardize_schema(sav_layer_sf)
    }))
    
    # Fail early if density/bedid missing (your core requirement)
    required_fields <- c("bedid", "density")
    missing_required_fields <- required_fields[!required_fields %in% names(sav_layer_sf)]
    
    if (length(missing_required_fields) > 0) {
      stop(glue(
        "Missing required fields after schema standardization: {paste(missing_required_fields, collapse = ', ')}"
      ))
    }
    
    # ---- Remove cartography artifacts ---------------------------------------
    rows_before_artifact_filter <- nrow(sav_layer_sf)
    
    sav_layer_sf <- with_timing("Remove artifacts (BEDID NA & DENSITY 0)", quote({
      sav_layer_sf %>%
        dplyr::filter(!(is.na(bedid) & density == 0))
    }))
    
    qc_table <- bind_rows(
      qc_table,
      qc_row(glue("year_{parsed_year}_rows_after_artifact_filter"), nrow(sav_layer_sf)),
      qc_row(glue("year_{parsed_year}_rows_removed_artifact_filter"),
             rows_before_artifact_filter - nrow(sav_layer_sf))
    )
    
    # ---- Add year field ------------------------------------------------------
    sav_layer_sf <- sav_layer_sf %>%
      dplyr::mutate(year = parsed_year)
    
    # ---- Write output --------------------------------------------------------
    output_gpkg_path <- file.path(DIR_OUT_SAV, glue("sav_cleaned_{parsed_year}.gpkg"))
    output_layer_name <- glue("sav_cleaned_{parsed_year}")
    
    with_timing("Write gpkg", quote({
      safe_write_gpkg(sav_layer_sf, output_gpkg_path, output_layer_name)
    }))
    
    log_info(glue("Wrote: {output_gpkg_path} (layer: {output_layer_name})"))
    qc_table <- bind_rows(qc_table, qc_row(glue("year_{parsed_year}_out_file"), output_gpkg_path))
    
    TRUE
    
  }, error = function(error_object) {
    
    error_message <- conditionMessage(error_object)
    log_error(glue("YEAR {parsed_year} FAILED: {error_message}"))
    
    qc_table <<- bind_rows(
      qc_table,
      qc_row(glue("year_{parsed_year}_error"), error_message, file_basename)
    )
    
    FALSE
  })
  
  if (!per_file_success) next
}

if (nrow(qc_table) == 0) {
  qc_table <- bind_rows(qc_table, qc_row("qc_note", "No QC rows were recorded (all years may have failed early)."))
}

# ------------------------------------------------
# 7. QC summary (always write)
# ------------------------------------------------
log_section("QC SUMMARY")

with_timing("Write QC files", quote({
  write_qc(qc_table, file_latest = FILE_QC_LATEST, file_run = FILE_QC_RUN)
}))

log_info(glue("QC latest: {FILE_QC_LATEST}"))
log_info(glue("QC run:    {FILE_QC_RUN}"))

# ------------------------------------------------
# 8. Done
# ------------------------------------------------
log_section("DONE")
log_info("Script completed successfully.")

