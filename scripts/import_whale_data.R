# import_whale_data.R
# standalone script to import Rice's whale density data into DuckDB
# run this script when the database is not locked by another R session

library(DBI)
library(duckdb)
library(glue)
library(sf)
library(dplyr)
library(tidyr)
library(terra)
library(here)

# source the functions
source(here("scripts/risk_functions.R"))

# set paths ----
is_server   <- Sys.info()[["sysname"]] == "Linux"
dir_data    <- ifelse(is_server, "/share/data/ricei", "~/My Drive/projects/ricei/data")
dir_raw     <- glue("{dir_data}/raw")
goa_db      <- glue("{dir_data}/raw/ships/gulf_of_mexico_2023/goa.duckdb")
whales_shp  <- glue("{dir_raw}/whales/Rices_Whale_Monthly_Density.shp")

cat("DuckDB path:", goa_db, "\n")
cat("Whale shapefile:", whales_shp, "\n\n")

# connect to database (read-write mode) ----
cat("Connecting to database (read-write mode)...\n")
con <- dbConnect(duckdb(), dbdir = goa_db, read_only = FALSE)
dbExecute(con, "INSTALL spatial; LOAD spatial;")

# check existing tables
cat("Existing tables:", paste(dbListTables(con), collapse = ", "), "\n\n")

# create canonical raster template from goa.geojson ----
goa_sf     <- st_read(here("data/goa.geojson"), quiet = TRUE)
r_template <- create_cell_template(goa_sf)
cat("Raster template:", nrow(r_template), "x", ncol(r_template),
    "=", ncell(r_template), "cells\n\n")

# migration check: compare existing cell_ids with expected raster indices ----
if ("cell" %in% dbListTables(con)) {
  sample_cell <- dbGetQuery(con, "
    SELECT cell_id, cell_ll_lon, cell_ll_lat FROM cell LIMIT 1
  ")

  if (nrow(sample_cell) > 0) {
    expected_id <- cellFromXY(
      r_template,
      cbind(
        sample_cell$cell_ll_lon + 0.05,
        sample_cell$cell_ll_lat + 0.05) )

    if (!is.na(expected_id) && sample_cell$cell_id != expected_id) {
      cat("MIGRATION NEEDED: cell_id", sample_cell$cell_id,
          "should be", expected_id, "\n")
      cat("Running reassign_cell_ids()...\n\n")
      r_template <- reassign_cell_ids(con, goa_sf, verbose = TRUE)
      cat("\n")
    } else {
      cat("Cell IDs already match raster indices (no migration needed).\n\n")
    }
  }
}

# check if whale tables exist ----
whale_exists      <- "whale" %in% dbListTables(con)
whale_cell_exists <- "whale_cell" %in% dbListTables(con)

if (whale_exists && whale_cell_exists) {
  cat("Tables 'whale' and 'whale_cell' already exist.\n")
  cat("Use overwrite = TRUE in import_whale_to_duckdb() to replace.\n\n")

  response <- readline("Overwrite existing tables? (y/N): ")
  overwrite <- tolower(response) == "y"
} else {
  overwrite <- TRUE
}

# process whale density using direct rasterization ----
cat("\nProcessing whale density shapefile (rasterization method)...\n")
whale_data <- process_whale_density(
  shp_path   = whales_shp,
  r_template = r_template,
  verbose    = TRUE
)

cat("\n--- Whale table preview ---\n")
print(whale_data$whale_tbl)

cat("\n--- Whale cell table preview (first 10 rows with value > 0) ---\n")
print(head(whale_data$whale_cell_tbl |> filter(value > 0), 10))

# import to database ----
cat("\nImporting to database...\n")
import_whale_to_duckdb(
  con            = con,
  whale_tbl      = whale_data$whale_tbl,
  whale_cell_tbl = whale_data$whale_cell_tbl,
  r_template     = r_template,
  overwrite      = overwrite,
  verbose        = TRUE
)

# verify ----
cat("\n--- Verification ---\n")
cat("Final tables:", paste(dbListTables(con), collapse = ", "), "\n")

cat("\nCell table row count:", dbGetQuery(con, "SELECT COUNT(*) FROM cell")[[1]], "\n")
cat("Whale table row count:", dbGetQuery(con, "SELECT COUNT(*) FROM whale")[[1]], "\n")
cat("Whale cell table row count:", dbGetQuery(con, "SELECT COUNT(*) FROM whale_cell")[[1]], "\n")

# verify cell_ids are raster indices (should be large integers)
cat("\nSample cell_ids (should be large raster indices, not sequential 1..N):\n")
print(dbGetQuery(con, "SELECT cell_id, cell_ll_lon, cell_ll_lat FROM cell ORDER BY cell_id LIMIT 5"))

# check for positive values
cat("\nCells with positive values by month/metric:\n")
print(dbGetQuery(con, "
  SELECT month_int, month_abb, metric, COUNT(*) as n_cells,
    SUM(CASE WHEN value > 0 THEN 1 ELSE 0 END) as n_positive
  FROM whale_cell
  GROUP BY month_int, month_abb, metric
  ORDER BY month_int, metric
"))

# check whale cells vs ship cells overlap
cat("\nCell overlap between whale and ship data:\n")
n_whale_cells <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT cell_id) FROM whale_cell")[[1]]
n_ship_cells <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT cell_id) FROM ship_cell")[[1]]
n_overlap <- dbGetQuery(con, "
  SELECT COUNT(DISTINCT wc.cell_id)
  FROM whale_cell wc
  JOIN (SELECT DISTINCT cell_id FROM ship_cell) sc
    ON wc.cell_id = sc.cell_id")[[1]]
cat("  Whale-only cells:", n_whale_cells - n_overlap, "\n")
cat("  Ship-only cells:", n_ship_cells - n_overlap, "\n")
cat("  Overlap cells:", n_overlap, "\n")

# spot-check: verify cellFromXY matches cell_id in DB
cat("\nSpot-check: verify cellFromXY(r_template, coords) == cell_id in DB:\n")
check_cells <- dbGetQuery(con, "SELECT cell_id, cell_ll_lon, cell_ll_lat FROM cell LIMIT 3")
for (i in seq_len(nrow(check_cells))) {
  expected <- cellFromXY(
    r_template,
    cbind(check_cells$cell_ll_lon[i] + 0.05, check_cells$cell_ll_lat[i] + 0.05) )
  match_ok <- check_cells$cell_id[i] == expected
  cat("  cell_id=", check_cells$cell_id[i], " expected=", expected,
      if (match_ok) " OK" else " MISMATCH", "\n")
}

# disconnect ----
dbDisconnect(con, shutdown = TRUE)
cat("\nDone! Database connection closed.\n")
