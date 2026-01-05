# ------------------------------------------------
# Script:      p2_06a_qc_quick_checks
# Purpose:     Lightweight QC sanity checks for the lineage outputs from p1_05
# Inputs:      data_processed/vims_superpatch_lineages/superpatch_nodes_with_lineage.(csv|gpkg),
#              lineage_summary.csv, event_summary.csv
# Outputs:     qc/p1_06a_qc_quick_checks_qc_<timestamp>.csv (+ _latest),
#              qc/p1_06a_qc_quick_checks_artifacts/* (tables + plot)
# Notes:       This is intentionally "quick checks" (human-readable) in addition to formal QC.
# ------------------------------------------------

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
  library(ggplot2)
})

SCRIPT_ID <- "p2_06a_qc_quick_checks"

# ------------------------------------------------
# Directories
# ------------------------------------------------
project_root_directory   <- here::here()
data_processed_directory <- file.path(project_root_directory, "data_processed")
logs_directory           <- file.path(project_root_directory, "logs")
qc_directory             <- file.path(project_root_directory, "qc")
lineage_directory        <- file.path(data_processed_directory, "vims_superpatch_lineages")

fs::dir_create(logs_directory, recurse = TRUE)
fs::dir_create(qc_directory,   recurse = TRUE)
fs::dir_create(lineage_directory, recurse = TRUE)

qc_artifacts_dir <- file.path(qc_directory, glue("{SCRIPT_ID}_artifacts"))
fs::dir_create(qc_artifacts_dir, recurse = TRUE)

qc_latest_file <- file.path(qc_directory, glue("{SCRIPT_ID}_qc_latest.csv"))

# ------------------------------------------------
# Logging setup
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

safe_copy_latest <- function(from, to) {
  fs::file_copy(from, to, overwrite = TRUE)
  invisible(to)
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
# Inputs
# ------------------------------------------------
log_section("INPUTS")

nodes_csv_in  <- file.path(lineage_directory, "superpatch_nodes_with_lineage.csv")
nodes_gpkg_in <- file.path(lineage_directory, "superpatch_nodes_with_lineage.gpkg")
lin_sum_in    <- file.path(lineage_directory, "lineage_summary.csv")
evt_sum_in    <- file.path(lineage_directory, "event_summary.csv")

inputs <- c(nodes_csv_in, nodes_gpkg_in, lin_sum_in, evt_sum_in)
for (p in inputs) log_info(glue("Input: {p} | exists={file.exists(p)}"))

stop_if(!file.exists(nodes_csv_in),  paste0("Missing required input: ", nodes_csv_in))
stop_if(!file.exists(nodes_gpkg_in), paste0("Missing required input: ", nodes_gpkg_in))
stop_if(!file.exists(lin_sum_in),    paste0("Missing required input: ", lin_sum_in))
stop_if(!file.exists(evt_sum_in),    paste0("Missing required input: ", evt_sum_in))

# ------------------------------------------------
# PROCESS
# ------------------------------------------------
log_section("PROCESS")
qc <- tibble()

# ---- File existence + size ----
paths <- c(nodes_csv_in, nodes_gpkg_in, lin_sum_in, evt_sum_in)
file_tbl <- tibble(
  file = basename(paths),
  path = paths,
  exists = file.exists(paths),
  size_mb = round(file.info(paths)$size / 1024^2, 2)
)

file_tbl_path <- file.path(qc_artifacts_dir, glue("files_{qc_timestamp}.csv"))
safe_write_csv_local(file_tbl, file_tbl_path)
log_info(glue("Wrote file table: {file_tbl_path}"))

qc <- bind_rows(
  qc,
  qc_row("files_all_exist", all(file_tbl$exists)),
  qc_row("nodes_csv_size_mb", file_tbl$size_mb[file_tbl$file == "superpatch_nodes_with_lineage.csv"]),
  qc_row("nodes_gpkg_size_mb", file_tbl$size_mb[file_tbl$file == "superpatch_nodes_with_lineage.gpkg"])
)

# ---- Read core tables ----
nodes <- with_timing("Read nodes_with_lineage CSV", quote({
  readr::read_csv(nodes_csv_in, show_col_types = FALSE)
}))

evt_sum <- with_timing("Read event_summary CSV", quote({
  readr::read_csv(evt_sum_in, show_col_types = FALSE)
}))

lin_sum <- with_timing("Read lineage_summary CSV", quote({
  readr::read_csv(lin_sum_in, show_col_types = FALSE)
}))

node_sf <- with_timing("Read nodes_with_lineage GPKG", quote({
  sf::st_read(nodes_gpkg_in, quiet = TRUE)
}))

# ---- Basic counts ----
qc <- bind_rows(
  qc,
  qc_row("nodes_csv_n", nrow(nodes)),
  qc_row("nodes_gpkg_n", nrow(node_sf))
)
stop_if(nrow(nodes) != nrow(node_sf),
        glue("Row mismatch: nodes CSV n={nrow(nodes)} vs GPKG n={nrow(node_sf)}"))

# ---- Unique key (poly_id, year) ----
key_chk <- nodes %>%
  summarise(
    n = n(),
    n_key = n_distinct(paste(poly_id, year)),
    any_dupes = n != n_key
  )

qc <- bind_rows(
  qc,
  qc_row("nodes_any_dupes_polyid_year", key_chk$any_dupes),
  qc_row("nodes_n_distinct_polyid_year", key_chk$n_key)
)
stop_if(isTRUE(key_chk$any_dupes), "Duplicate (poly_id, year) keys detected in nodes.")

# ---- Missingness ----
miss <- nodes %>%
  summarise(
    miss_lineage = sum(is.na(lineage_id)),
    miss_event   = sum(is.na(event_type)),
    miss_poly    = sum(is.na(poly_id)),
    miss_year    = sum(is.na(year))
  )

qc <- bind_rows(
  qc,
  qc_row("nodes_miss_lineage", miss$miss_lineage),
  qc_row("nodes_miss_event",   miss$miss_event),
  qc_row("nodes_miss_poly_id", miss$miss_poly),
  qc_row("nodes_miss_year",    miss$miss_year)
)

stop_if(miss$miss_lineage > 0, "nodes has missing lineage_id values.")
stop_if(miss$miss_event   > 0, "nodes has missing event_type values.")
stop_if(miss$miss_poly    > 0, "nodes has missing poly_id values.")
stop_if(miss$miss_year    > 0, "nodes has missing year values.")

# ---- CRS + geometry sanity ----
qc <- bind_rows(
  qc,
  qc_row("gpkg_has_geometry", !all(is.na(sf::st_geometry(node_sf)))),
  qc_row("gpkg_crs_epsg", sf::st_crs(node_sf)$epsg)
)

# ---- Event summary view (save) ----
evt_sorted <- evt_sum %>% arrange(desc(n))
evt_sorted_path <- file.path(qc_artifacts_dir, glue("event_summary_sorted_{qc_timestamp}.csv"))
safe_write_csv_local(evt_sorted, evt_sorted_path)
log_info(glue("Wrote sorted event summary: {evt_sorted_path}"))

# ---- Sample a few lineages (save) ----
set.seed(1)
some_lin <- nodes %>%
  filter(!is.na(lineage_id)) %>%
  distinct(lineage_id) %>%
  slice_sample(n = 5)

sample_lin_tbl <- nodes %>%
  semi_join(some_lin, by = "lineage_id") %>%
  arrange(lineage_id, year) %>%
  select(lineage_id, year, poly_id, n_predecessors, n_successors, event_type)

sample_lin_path <- file.path(qc_artifacts_dir, glue("sample_lineages_{qc_timestamp}.csv"))
safe_write_csv_local(sample_lin_tbl, sample_lin_path)
log_info(glue("Wrote sample lineages: {sample_lin_path}"))

# ---- Top lineage preview (save) ----
top10_lin <- lin_sum %>%
  arrange(desc(n_nodes)) %>%
  slice_head(n = 10)

top10_lin_path <- file.path(qc_artifacts_dir, glue("top10_lineages_{qc_timestamp}.csv"))
safe_write_csv_local(top10_lin, top10_lin_path)
log_info(glue("Wrote top10 lineage summary: {top10_lin_path}"))

target_lin <- top10_lin %>% slice(1) %>% pull(lineage_id)

target_lin_tbl <- nodes %>%
  filter(lineage_id == target_lin) %>%
  arrange(year, poly_id) %>%
  select(lineage_id, year, poly_id, n_predecessors, n_successors, event_type)

target_lin_path <- file.path(qc_artifacts_dir, glue("largest_lineage_{qc_timestamp}.csv"))
safe_write_csv_local(target_lin_tbl, target_lin_path)
log_info(glue("Wrote largest lineage table: {target_lin_path}"))

# ---- Invariant checks ----
max_year <- max(nodes$year, na.rm = TRUE)

inv <- nodes %>%
  summarise(
    bad_first_app = sum(event_type == "first_appearance" & n_predecessors != 0),
    bad_disappear = sum(event_type == "disappearance" & (n_successors != 0 | year == max_year)),
    bad_censored  = sum(event_type == "right_censored_continuation" & year != max_year)
  )

qc <- bind_rows(
  qc,
  qc_row("nodes_max_year", max_year),
  qc_row("bad_first_appearance_pred_nonzero", inv$bad_first_app),
  qc_row("bad_disappearance_succ_nonzero_or_lastyear", inv$bad_disappear),
  qc_row("bad_right_censored_not_in_lastyear", inv$bad_censored)
)

stop_if(inv$bad_first_app > 0, "Invariant failed: first_appearance should have n_predecessors == 0.")
stop_if(inv$bad_disappear > 0, "Invariant failed: disappearance should have n_successors == 0 and not occur in last year.")
stop_if(inv$bad_censored  > 0, "Invariant failed: right_censored_continuation should occur only in last year.")

# ---- Years present (save) ----
years_tbl <- tibble(year = sort(unique(nodes$year)))
years_path <- file.path(qc_artifacts_dir, glue("years_present_{qc_timestamp}.csv"))
safe_write_csv_local(years_tbl, years_path)
log_info(glue("Wrote years present: {years_path}"))

qc <- bind_rows(
  qc,
  qc_row("years_n", nrow(years_tbl)),
  qc_row("years_min", min(years_tbl$year)),
  qc_row("years_max", max(years_tbl$year))
)

# ---- Extra quick stats ----
quick_stats <- nodes %>%
  summarise(
    n = n(),
    pct_singletons = mean(n_predecessors == 0 & n_successors == 0),
    pct_first_only = mean(event_type == "first_appearance"),
    pct_disappear  = mean(event_type == "disappearance")
  )

quick_stats_path <- file.path(qc_artifacts_dir, glue("quick_stats_{qc_timestamp}.csv"))
safe_write_csv_local(quick_stats, quick_stats_path)
log_info(glue("Wrote quick stats: {quick_stats_path}"))

qc <- bind_rows(
  qc,
  qc_row("pct_singletons", round(quick_stats$pct_singletons, 6)),
  qc_row("pct_first_appearance", round(quick_stats$pct_first_only, 6)),
  qc_row("pct_disappearance", round(quick_stats$pct_disappear, 6))
)

# ---- Plot year x event counts (save PNG) ----
year_events <- nodes %>%
  count(year, event_type)

p <- ggplot(year_events, aes(year, n, group = event_type)) +
  geom_line() +
  facet_wrap(~event_type, scales = "free_y") +
  theme_bw()

plot_path <- file.path(qc_artifacts_dir, glue("year_event_counts_{qc_timestamp}.png"))
ggsave(plot_path, p, width = 10, height = 7, dpi = 150)
log_info(glue("Wrote plot: {plot_path}"))

# ------------------------------------------------
# OUTPUTS
# ------------------------------------------------
log_section("OUTPUTS")

with_timing("Write QC tables", quote({
  safe_write_csv_local(qc, qc_run_file)
  safe_copy_latest(qc_run_file, qc_latest_file)
}))

log_info(glue("QC latest: {qc_latest_file}"))
log_info(glue("QC run:    {qc_run_file}"))

log_section("DONE")
log_info("Script completed successfully.")