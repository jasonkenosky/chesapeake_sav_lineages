# ============================================================
# File: logger_helpers.R
# Purpose: Reusable logging utilities for reproducible R scripts
# Author: Jason Kenosky
# Philosophy: clarity | nouns for objects | verbs for functions
# ============================================================

suppressPackageStartupMessages({
  library(glue)
  library(tibble)
  library(readr)
  library(dplyr)
})

# ---- Internal helpers -------------------------------------------------------

reset_sinks_safe <- function(max_steps = 20) {
  
  # First close connections; this prevents "dangling sink to dead con" stalls
  try(closeAllConnections(), silent = TRUE)
  
  # Drain output sinks (bounded)
  for (i in seq_len(max_steps)) {
    if (sink.number(type = "output") <= 0) break
    try(sink(NULL, type = "output"), silent = TRUE)
  }
  
  # Drain message sinks (bounded)
  for (i in seq_len(max_steps)) {
    if (sink.number(type = "message") <= 0) break
    try(sink(NULL, type = "message"), silent = TRUE)
  }
  
  invisible(TRUE)
}

start_log <- function(script_id, logs_dir) {
  
  reset_sinks_safe()
  
  dir.create(logs_dir, showWarnings = FALSE, recursive = TRUE)
  
  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  log_file  <- file.path(logs_dir, paste0(script_id, "_", timestamp, ".log"))
  log_con   <- file(log_file, open = "wt")
  
  sink(log_con, type = "output", split = TRUE)
  sink(log_con, type = "message", append = TRUE)
  
  cat("--------------------------------------------------\n")
  cat("Script:      ", script_id, "\n", sep = "")
  cat("Started:     ", as.character(Sys.time()), "\n", sep = "")
  cat("Log file:    ", log_file, "\n", sep = "")
  cat("Working dir: ", getwd(), "\n", sep = "")
  cat("--------------------------------------------------\n\n")
  
  list(log_file = log_file, log_con = log_con, enabled = TRUE)
}

stop_log <- function(log_meta) {
  
  if (is.null(log_meta)) return(invisible(NULL))
  
  reset_sinks_safe()
  
  if (!is.null(log_meta$log_con)) {
    try(close(log_meta$log_con), silent = TRUE)
  }
  
  invisible(NULL)
}

log_section <- function(title) {
  cat("\n==================================================\n")
  cat(title, "\n", sep = "")
  cat("==================================================\n")
}

log_info <- function(message) {
  cat(sprintf("[INFO  %s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), message))
}

log_warn <- function(message) {
  cat(sprintf("[WARN  %s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), message))
}

log_error <- function(message) {
  cat(sprintf("[ERROR %s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), message))
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

log_packages <- function() {
  log_section("PACKAGES")
  pkgs <- sort(unique(.packages()))
  for (pkg in pkgs) {
    ver <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
    log_info(paste0(pkg, "==", ver))
  }
  invisible(TRUE)
}

log_inputs <- function(paths) {
  log_section("INPUT INVENTORY")
  for (path in paths) {
    exists <- file.exists(path) || dir.exists(path)
    log_info(paste0("Input: ", path, " | exists=", exists))
  }
  invisible(TRUE)
}

log_outputs <- function(paths) {
  log_section("OUTPUT INVENTORY")
  for (path in paths) {
    exists <- file.exists(path)
    log_info(paste0("Output: ", path, " | exists=", exists))
  }
  invisible(TRUE)
}