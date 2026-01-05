# ============================================================
# File: p3_06_qc_lineage_tables.R
# Purpose: QC + compare lineage/event summary tables produced by p1_05
# Author: Jason Kenosky
# Philosophy: clarity | nouns for objects | verbs for functions
# ============================================================

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
  library(readr)
  library(sf)
})

SCRIPT_ID <- "p3_06_qc_lineage_tables"

# ------------------------------------------------
# Directories
# ------------------------------------------------
project_root_directory    <- here::here()
data_processed_directory  <- file.path(project_root_directory, "data_processed")
logs_directory            <- file.path(project_root_directory, "logs")
qc_directory              <- file.path(project_root_directory, "qc")
lineage_directory         <- file.path(data_processed_directory, "vims_superpatch_lineages")

fs::dir_create(logs_directory, recurse = TRUE)
fs::dir_create(qc_directory, recurse = TRUE)
fs::dir_create(lineage_directory, recurse = TRUE)

qc_latest_file    <- file.path(qc_directory, glue("{SCRIPT_ID}_qc_latest.csv"))
qc_artifacts_dir  <- file.path(qc_directory, glue("{SCRIPT_ID}_artifacts"))
fs::dir_create(qc_artifacts_dir, recurse = TRUE)

# ------------------------------------------------
# Logging setup (your helpers)
# ------------------------------------------------
logger_helpers_file <- here::here("R", "logger_helpers.R")
if (!file.exists(logger_helpers_file)) stop("Missing logger helpers: ", logger_helpers_file)
source(logger_helpers_file)

log_meta <- start_log(SCRIPT_ID, logs_directory)
on.exit(stop_log(log_meta), add = TRUE)   # <<<< IMPORTANT: pass log_meta, not log_con

log_section(glue("{SCRIPT_ID} - START"))
log_info(glue("Project root: {project_root_directory}"))
log_info(glue("Working dir:  {getwd()}"))
log_info(glue("Log file:     {log_meta$log_file}"))

qc_timestamp <- gsub(glue("{SCRIPT_ID}_|\\.log$"), "", basename(log_meta$log_file))
qc_run_file  <- file.path(qc_directory, glue("{SCRIPT_ID}_qc_{qc_timestamp}.csv"))

# ------------------------------------------------
# Local helpers
# ------------------------------------------------
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

safe_write_csv_any <- function(x, path) {
  if (exists("safe_write_csv")) {
    safe_write_csv(x, path)
  } else {
    safe_write_csv_local(x, path)
  }
}

with_timing <- function(label, expr) {
  start_time <- Sys.time()
  result <- tryCatch(
    eval(expr),
    error = function(e) {
      log_error(glue("{label} FAILED: {conditionMessage(e)}"))
      stop(e)
    }
  )
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  log_info(glue("{label} | {round(elapsed, 3)} sec"))
  result
}

stop_if <- function(cond, msg) {
  if (isTRUE(cond)) {
    log_error(msg)
    stop(msg, call. = FALSE)
  }
}

# ------------------------------------------------
# Inputs from p1_05
# ------------------------------------------------
log_section("INPUTS")

nodes_csv_in   <- file.path(lineage_directory, "superpatch_nodes_with_lineage.csv")
nodes_gpkg_in  <- file.path(lineage_directory, "superpatch_nodes_with_lineage.gpkg")
lineage_sum_in <- file.path(lineage_directory, "lineage_summary.csv")
event_sum_in   <- file.path(lineage_directory, "event_summary.csv")

inputs <- c(nodes_csv_in, nodes_gpkg_in, lineage_sum_in, event_sum_in)
for (p in inputs) log_info(glue("Input: {p} | exists={file.exists(p)}"))

stop_if(!file.exists(nodes_csv_in),   paste0("Missing required input: ", nodes_csv_in))
stop_if(!file.exists(nodes_gpkg_in),  paste0("Missing required input: ", nodes_gpkg_in))
stop_if(!file.exists(lineage_sum_in), paste0("Missing required input: ", lineage_sum_in))
stop_if(!file.exists(event_sum_in),   paste0("Missing required input: ", event_sum_in))

# ------------------------------------------------
# QC start
# ------------------------------------------------
log_section("PROCESS")
qc <- tibble()

# ---- Read tables ----
nodes <- with_timing("Read nodes_with_lineage CSV", quote({
  readr::read_csv(nodes_csv_in, show_col_types = FALSE)
}))

lin_sum <- with_timing("Read lineage_summary CSV", quote({
  readr::read_csv(lineage_sum_in, show_col_types = FALSE)
}))

evt_sum <- with_timing("Read event_summary CSV", quote({
  readr::read_csv(event_sum_in, show_col_types = FALSE)
}))

node_sf <- with_timing("Read nodes_with_lineage GPKG", quote({
  sf::st_read(nodes_gpkg_in, quiet = TRUE)
}))

# ---- Basic structure checks ----
required_nodes_cols <- c(
  "poly_id", "year",
  "component_id", "lineage_id",
  "n_predecessors", "n_successors",
  "event_type"
)
missing_nodes_cols <- setdiff(required_nodes_cols, names(nodes))
stop_if(length(missing_nodes_cols) > 0,
        paste0("nodes CSV missing required columns: ", paste(missing_nodes_cols, collapse = ", ")))

stop_if(!inherits(node_sf, "sf"), "GPKG read did not return an sf object.")

# ---- Row count match between CSV and GPKG ----
qc <- bind_rows(
  qc,
  qc_row("nodes_csv_n", nrow(nodes)),
  qc_row("nodes_gpkg_n", nrow(node_sf))
)
stop_if(nrow(nodes) != nrow(node_sf),
        glue("Row mismatch: CSV n={nrow(nodes)} vs GPKG n={nrow(node_sf)}"))

# ---- Unique key: poly_id + year ----
key_n <- nodes %>%
  summarise(
    n = n(),
    n_key = n_distinct(paste(poly_id, year)),
    any_dupes = n != n_key
  )

qc <- bind_rows(
  qc,
  qc_row("nodes_any_dupes_polyid_year", key_n$any_dupes),
  qc_row("nodes_n_distinct_polyid_year", key_n$n_key)
)
stop_if(isTRUE(key_n$any_dupes), "Duplicate (poly_id, year) keys detected in nodes table.")

# ---- Missingness ----
miss <- nodes %>%
  summarise(
    miss_poly    = sum(is.na(poly_id)),
    miss_year    = sum(is.na(year)),
    miss_lineage = sum(is.na(lineage_id)),
    miss_event   = sum(is.na(event_type))
  )

qc <- bind_rows(
  qc,
  qc_row("nodes_miss_poly_id", miss$miss_poly),
  qc_row("nodes_miss_year",    miss$miss_year),
  qc_row("nodes_miss_lineage", miss$miss_lineage),
  qc_row("nodes_miss_event",   miss$miss_event)
)

stop_if(miss$miss_poly > 0, "nodes has missing poly_id values.")
stop_if(miss$miss_year > 0, "nodes has missing year values.")
stop_if(miss$miss_lineage > 0, "nodes has missing lineage_id values.")
stop_if(miss$miss_event > 0, "nodes has missing event_type values.")

# ---- Event-type invariants ----
max_year <- max(nodes$year, na.rm = TRUE)

inv <- nodes %>%
  summarise(
    bad_first_app = sum(event_type == "first_appearance" & n_predecessors != 0),
    bad_disappear = sum(event_type == "disappearance" & (n_successors != 0 | year == max_year)),
    bad_censored  = sum(event_type == "right_censored_continuation" & year != max_year)
  )

qc <- bind_rows(
  qc,
  qc_row("bad_first_appearance_pred_nonzero", inv$bad_first_app),
  qc_row("bad_disappearance_succ_nonzero_or_lastyear", inv$bad_disappear),
  qc_row("bad_right_censored_not_in_lastyear", inv$bad_censored),
  qc_row("nodes_max_year", max_year)
)

stop_if(inv$bad_first_app > 0,
        "Invariant failed: first_appearance should have n_predecessors == 0.")
stop_if(inv$bad_disappear > 0,
        "Invariant failed: disappearance should have n_successors == 0 and not occur in last year.")
stop_if(inv$bad_censored > 0,
        "Invariant failed: right_censored_continuation should occur only in last year.")

# ---- CRS check ----
crs_epsg <- sf::st_crs(node_sf)$epsg
has_geom <- !all(is.na(sf::st_geometry(node_sf)))

qc <- bind_rows(
  qc,
  qc_row("gpkg_has_geometry", has_geom),
  qc_row("gpkg_crs_epsg", crs_epsg)
)

stop_if(!isTRUE(has_geom), "GPKG has missing/empty geometry.")
stop_if(is.na(crs_epsg), "GPKG CRS has NA epsg (missing CRS).")

# ---- Recompute event_summary and compare ----
evt_recalc <- nodes %>%
  count(event_type, name = "n") %>%
  arrange(event_type)

evt_in_norm <- evt_sum %>%
  mutate(event_type = as.character(event_type)) %>%
  arrange(event_type)

evt_cmp <- evt_recalc %>%
  full_join(evt_in_norm, by = "event_type", suffix = c("_recalc", "_file")) %>%
  mutate(
    n_recalc = coalesce(n_recalc, 0L),
    n_file   = coalesce(n_file,   0L),
    diff     = n_recalc - n_file
  )

evt_cmp_path <- file.path(qc_artifacts_dir, glue("event_summary_compare_{qc_timestamp}.csv"))
safe_write_csv_any(evt_cmp, evt_cmp_path)
log_info(glue("Wrote event_summary compare: {evt_cmp_path}"))

qc <- bind_rows(
  qc,
  qc_row("event_summary_any_diff", any(evt_cmp$diff != 0)),
  qc_row("event_summary_max_abs_diff", max(abs(evt_cmp$diff)))
)

stop_if(any(evt_cmp$diff != 0),
        "event_summary.csv does not match event counts recomputed from nodes.")

# ---- Recompute lineage_summary and compare ----
lin_recalc <- nodes %>%
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
  )

lin_in_norm <- lin_sum %>%
  mutate(lineage_id = as.character(lineage_id))

cmp_cols <- intersect(names(lin_recalc), names(lin_in_norm))
cmp_cols <- setdiff(cmp_cols, character(0))

lin_cmp <- lin_recalc %>%
  select(all_of(cmp_cols)) %>%
  full_join(
    lin_in_norm %>% select(all_of(cmp_cols)),
    by = "lineage_id",
    suffix = c("_recalc", "_file")
  )

num_fields <- setdiff(cmp_cols, "lineage_id")

# For each numeric field, compute diff = recalc - file
for (f in num_fields) {
  rec_col <- paste0(f, "_recalc")
  fil_col <- paste0(f, "_file")
  dif_col <- paste0("diff_", f)
  
  lin_cmp[[rec_col]] <- coalesce(lin_cmp[[rec_col]], 0)
  lin_cmp[[fil_col]] <- coalesce(lin_cmp[[fil_col]], 0)
  
  lin_cmp[[dif_col]] <- lin_cmp[[rec_col]] - lin_cmp[[fil_col]]
}

diff_cols <- paste0("diff_", num_fields)
lin_any_diff <- any(as.matrix(select(lin_cmp, all_of(diff_cols))) != 0)
lin_max_abs  <- max(abs(as.matrix(select(lin_cmp, all_of(diff_cols)))))

lin_cmp_path <- file.path(qc_artifacts_dir, glue("lineage_summary_compare_{qc_timestamp}.csv"))
safe_write_csv_any(lin_cmp, lin_cmp_path)
log_info(glue("Wrote lineage_summary compare: {lin_cmp_path}"))

qc <- bind_rows(
  qc,
  qc_row("lineage_summary_any_diff", lin_any_diff),
  qc_row("lineage_summary_max_abs_diff", lin_max_abs),
  qc_row("lineage_n_in_nodes", n_distinct(nodes$lineage_id)),
  qc_row("lineage_n_in_file",  n_distinct(lin_in_norm$lineage_id))
)

stop_if(isTRUE(lin_any_diff),
        "lineage_summary.csv does not match lineage summary recomputed from nodes.")

# ------------------------------------------------
# Outputs
# ------------------------------------------------
log_section("OUTPUTS")

with_timing("Write QC tables", quote({
  safe_write_csv_any(qc, qc_run_file)
  safe_write_csv_any(qc, qc_latest_file)
}))

log_info(glue("QC latest: {qc_latest_file}"))
log_info(glue("QC run:    {qc_run_file}"))

log_section("DONE")
log_info("Script completed successfully.")

