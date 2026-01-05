# ==============================================================================
# Script Title: Initialize project and validate configuration
# Purpose: Confirm the project environment and configuration are internally consistent.
# Author: Jason Kenosky
# Last Updated: 2026-01-02
#
# Description:
#   This script initializes the project directory structure, starts a run log,
#   validates configuration files, and writes configuration QC inventories.
#   It does not read or modify any SAV data.
#
# Inputs:
#   config/project_config.yml
#   config/thresholds.yml
#   R/logger_helpers.R
#
# Outputs (QC only):
#   qc/00_init_project__project_config_latest.csv
#   qc/00_init_project__project_config__<run_id>.csv
#   qc/00_init_project__thresholds_latest.csv
#   qc/00_init_project__thresholds__<run_id>.csv
#   qc/00_init_project__run_environment_latest.csv
#   qc/00_init_project__run_environment__<run_id>.csv
#   qc/00_init_project__qc_summary_latest.csv
#   qc/00_init_project__qc_summary__<run_id>.csv
#
# Notes:
#   - This script is safe to run at any time.
#   - Fail fast when configuration is invalid.
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

SCRIPT_ID <- "00_init_project"
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
  
  library(readr)
  library(dplyr)
  library(tibble)
})

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

project_config <- yaml::read_yaml(FILE_PROJECT_CONFIG)
thresholds_config <- yaml::read_yaml(FILE_THRESHOLDS)

# ------------------------------------------------
# 5. Helpers (script-local only)
# ------------------------------------------------
safe_write_csv <- function(data_table, file_path) {
  fs::dir_create(dirname(file_path), recurse = TRUE)
  readr::write_csv(data_table, file_path)
  invisible(file_path)
}

write_qc_table <- function(data_table, file_latest, file_run) {
  safe_write_csv(data_table, file_latest)
  safe_write_csv(data_table, file_run)
  invisible(TRUE)
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

flatten_list <- function(x, parent_key = "") {
  # Deterministic flattening for QC inventories.
  # Produces keys like: "crs.canonical_epsg"
  if (is.null(x)) return(tibble::tibble(key = character(0), value = character(0)))
  
  if (!is.list(x)) {
    key <- ifelse(parent_key == "", "value", parent_key)
    return(tibble::tibble(key = key, value = as.character(x)))
  }
  
  output <- tibble::tibble(key = character(0), value = character(0))
  
  names_x <- names(x)
  if (is.null(names_x)) names_x <- rep("", length(x))
  
  for (i in seq_along(x)) {
    name_i <- names_x[[i]]
    if (name_i == "") name_i <- as.character(i)
    
    child_key <- if (parent_key == "") name_i else paste(parent_key, name_i, sep = ".")
    output <- dplyr::bind_rows(output, flatten_list(x[[i]], child_key))
  }
  
  output
}

# ------------------------------------------------
# 6. Main process
# ------------------------------------------------
log_section("PROCESS")

# Validate directory keys exist in config (schema-level validation)
required_directory_keys <- c(
  "data_raw_vims_yearly",
  "data_intermediate",
  "outputs",
  "qc",
  "logs"
)

for (key in required_directory_keys) {
  if (is.null(project_config$directories[[key]])) {
    stop("Missing directories.", key, " in project_config.yml")
  }
}

# Create directory structure (do not delete anything)
required_directories <- c(
  project_config$directories$data_raw_vims_yearly,
  project_config$directories$data_intermediate,
  project_config$directories$outputs,
  project_config$directories$qc,
  project_config$directories$logs
)

for (directory in required_directories) {
  fs::dir_create(here::here(directory), recurse = TRUE)
}

log_info("Directory structure verified.")

# Validate CRS config
if (is.null(project_config$crs$canonical_epsg)) stop("Missing crs.canonical_epsg")
canonical_epsg <- project_config$crs$canonical_epsg
if (!is.numeric(canonical_epsg)) stop("crs.canonical_epsg must be numeric")

# Validate survey years
if (is.null(project_config$survey_years$first_year)) stop("Missing survey_years.first_year")
if (is.null(project_config$survey_years$last_year))  stop("Missing survey_years.last_year")

first_year <- as.integer(project_config$survey_years$first_year)
last_year  <- as.integer(project_config$survey_years$last_year)
if (!(first_year < last_year)) stop("survey_years.first_year must be < survey_years.last_year")

# Validate temporal adjacency exception
if (is.null(project_config$temporal_adjacency$allowed_gap_links)) stop("Missing temporal_adjacency.allowed_gap_links")

allowed_gap_links <- project_config$temporal_adjacency$allowed_gap_links
if (length(allowed_gap_links) != 1) stop("Exactly one allowed gap link is required (1987 -> 1989)")

gap_from <- allowed_gap_links[[1]]$from_year
gap_to   <- allowed_gap_links[[1]]$to_year
if (!(gap_from == 1987 && gap_to == 1989)) stop("Allowed gap link must be from_year=1987 and to_year=1989")

# Validate scoring weights sum to 1.0
if (is.null(thresholds_config$scoring$weights)) stop("Missing scoring.weights in thresholds.yml")
weights <- thresholds_config$scoring$weights
weight_sum <- sum(unlist(weights))
if (abs(weight_sum - 1.0) > 1e-6) stop("scoring.weights must sum to 1.0")

log_info("Configuration validation passed.")

# ------------------------------------------------
# 7. Write outputs (QC only)
# ------------------------------------------------
log_section("OUTPUTS")

run_id <- log_meta$run_id

project_config_inventory <- flatten_list(project_config) %>%
  dplyr::arrange(key)

thresholds_inventory <- flatten_list(thresholds_config) %>%
  dplyr::arrange(key)

run_environment <- tibble::tibble(
  key = c("script_id", "start_time", "r_version", "platform", "working_directory"),
  value = c(
    SCRIPT_ID,
    as.character(script_start_time),
    R.version.string,
    R.version$platform,
    getwd()
  )
)

FILE_PROJECT_CONFIG_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}__project_config_latest.csv"))
FILE_PROJECT_CONFIG_RUN    <- file.path(DIR_QC, glue("{SCRIPT_ID}__project_config__{run_id}.csv"))

FILE_THRESHOLDS_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}__thresholds_latest.csv"))
FILE_THRESHOLDS_RUN    <- file.path(DIR_QC, glue("{SCRIPT_ID}__thresholds__{run_id}.csv"))

FILE_ENV_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}__run_environment_latest.csv"))
FILE_ENV_RUN    <- file.path(DIR_QC, glue("{SCRIPT_ID}__run_environment__{run_id}.csv"))

write_qc_table(project_config_inventory, FILE_PROJECT_CONFIG_LATEST, FILE_PROJECT_CONFIG_RUN)
write_qc_table(thresholds_inventory,     FILE_THRESHOLDS_LATEST,     FILE_THRESHOLDS_RUN)
write_qc_table(run_environment,          FILE_ENV_LATEST,            FILE_ENV_RUN)

log_info(glue("Wrote: {FILE_PROJECT_CONFIG_LATEST}"))
log_info(glue("Wrote: {FILE_THRESHOLDS_LATEST}"))
log_info(glue("Wrote: {FILE_ENV_LATEST}"))

# ------------------------------------------------
# 8. QC summary (always write)
# ------------------------------------------------
log_section("QC SUMMARY")

qc_summary <- dplyr::bind_rows(
  qc_row("canonical_epsg", canonical_epsg),
  qc_row("first_year", first_year),
  qc_row("last_year", last_year),
  qc_row("allowed_gap_link", glue("{gap_from}->{gap_to}")),
  qc_row("scoring_weight_sum", weight_sum)
)

FILE_QC_SUMMARY_LATEST <- file.path(DIR_QC, glue("{SCRIPT_ID}__qc_summary_latest.csv"))
FILE_QC_SUMMARY_RUN    <- file.path(DIR_QC, glue("{SCRIPT_ID}__qc_summary__{run_id}.csv"))

write_qc_table(qc_summary, FILE_QC_SUMMARY_LATEST, FILE_QC_SUMMARY_RUN)

log_section("DONE")
log_info("Script completed successfully.")

