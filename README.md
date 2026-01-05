# Chesapeake Bay SAV Lineages

## Overview

This repository provides a reproducible, stepwise pipeline for reconstructing
Submerged Aquatic Vegetation (SAV) patch lineages in the Chesapeake Bay directly
from raw VIMS SAV polygon datasets.

The workflow constructs node and edge tables, identifies graph-connected
components, assigns stable lineage identifiers, and classifies patch-level
events (e.g., appearance, continuation, fragmentation, merging, disappearance,
and right-censoring).

The purpose of this project is **infrastructure, not interpretation**.
It defines a canonical and defensible method for building SAV lineages that can
be reused consistently across downstream analyses (e.g., time-series modeling,
sequence-based methods, resilience studies, and manuscripts).

## Design Principles

- Reproducibility over convenience  
- Deterministic lineage construction  
- Explicit quality control at every stage  
- Fail fast on silent errors  
- Separate lineage construction from analysis and interpretation  
This repository provides a reproducible, stepwise pipeline for reconstructing Submerged Aquatic Vegetation (SAV) patch lineages in the Chesapeake Bay directly from raw VIMS SAV polygon datasets.

The workflow builds node and edge tables, identifies graph-connected components, assigns stable lineage identifiers, and classifies patch-level events (e.g., appearance, continuation, fragmentation, merging, disappearance, and right-censoring). The goal of this project is not ecological interpretation, but to provide a defensible and canonical lineage construction framework that can be reused consistently across downstream analyses, including time-series modeling, sequence-based methods, and resilience studies.

## Repository Structure

```text
chesapeake_sav_lineages/
├── R/                  # Core processing scripts (p1_01 → p1_06)
├── data_processed/     # Derived lineage products (CSV, GPKG)
├── qc/                 # Quality control tables and artifacts
├── logs/               # Timestamped execution logs
├── README.md
└── .gitignore
```

## Pipeline Summary


## Pipeline Summary

The canonical lineage construction pipeline proceeds in six steps:

1. Project initialization and directory setup  
2. VIMS SAV polygon cleaning and normalization  
3. Node table construction  
4. Edge table construction  
5. Lineage assignment and event classification  
6. Lineage-level and event-level quality control  

Scripts are prefixed numerically (`p1_01_…`, `p1_02_…`) to enforce execution order.


## Outputs


Primary outputs include:

- `superpatch_nodes_with_lineage.csv`
- `superpatch_nodes_with_lineage.gpkg`
- `lineage_summary.csv`
- `event_summary.csv`

These products are intended to be treated as **authoritative lineage inputs**
for downstream projects.

## Scope and non-Goals

This repository does **not**:

- Interpret ecological drivers
- Fit statistical or mechanistic models
- Modify or reclassify original VIMS SAV delineations
- Replace official VIMS or state reporting products

## Licesne

meh

## Author

Jason Kenosky


