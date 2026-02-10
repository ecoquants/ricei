# risk_functions.R - Ship-strike risk calculation functions for Rice's whale analysis
# NRDC Contract: Further analysis of Rice's whale ship-strike risk

# Conn/Silber 2013 speed-based risk factors ----
RISK_FACTORS <- list(
  # speed bin definitions and associated risk factors
  bins = c(0, 10, 12, 15, Inf),
  factors = c(1.0, 1.6, 5.4, 10.8),
  labels = c("<=10 kt", ">10-12 kt", ">12-15 kt", ">15 kt")
)

#' Assign speed bin based on average speed
#' @param speed Numeric vector of speeds in knots
#' @return Factor of speed bins
assign_speed_bin <- function(speed) {
  cut(
    speed,
    breaks = RISK_FACTORS$bins,
    labels = RISK_FACTORS$labels,
    include.lowest = TRUE,
    right = TRUE
  )
}

#' Get risk factor for a speed bin
#' @param speed_bin Factor or character of speed bin labels
#' @return Numeric risk factor
get_risk_factor <- function(speed_bin) {
  idx <- match(as.character(speed_bin), RISK_FACTORS$labels)
  RISK_FACTORS$factors[idx]
}

#' Calculate base risk (whale density x traffic, no speed weighting)
#' @param whale_density Numeric whale density value
#' @param traffic_metric Numeric traffic metric (distance, hours, or count)
#' @return Numeric base risk value
calc_base_risk <- function(whale_density, traffic_metric) {
  whale_density * traffic_metric
}

#' Calculate speed-weighted risk using Conn/Silber 2013 factors
#' @param whale_density Numeric whale density value
#' @param traffic_metric Numeric traffic metric
#' @param speed_bin Speed bin (factor or character)
#' @return Numeric speed-weighted risk value
calc_speed_weighted_risk <- function(whale_density, traffic_metric, speed_bin) {
  risk_factor <- get_risk_factor(speed_bin)
  whale_density * traffic_metric * risk_factor
}

#' Calculate risk reduction from slowdown scenario
#' @param baseline_risk Numeric baseline risk value
#' @param slowdown_risk Numeric risk after slowdown (all speeds at 10kt)
#' @return List with absolute reduction and percentage reduction
calc_risk_reduction <- function(baseline_risk, slowdown_risk) {
  absolute <- baseline_risk - slowdown_risk
  pct <- ifelse(baseline_risk > 0, (absolute / baseline_risk) * 100, 0)
  list(
    absolute   = absolute,
    percentage = pct
  )
}

#' Summarize risk by geographic area
#' @param risk_data Data frame with columns: area_id, whale_density, traffic, speed_bin
#' @param area_col Name of area column
#' @param verbose Show debug messages
#' @return Data frame with risk summaries by area
summarize_risk_by_area <- function(risk_data, area_col = "area_id", verbose = interactive()) {

  if (verbose) message("[summarize_risk_by_area] Calculating risk for column: ", area_col)

  # add risk columns
  risk_data <- risk_data |>
    dplyr::mutate(
      base_risk = calc_base_risk(whale_density, traffic),
      speed_weighted_risk = calc_speed_weighted_risk(whale_density, traffic, speed_bin),
      # slowdown scenario: all at 10kt (factor = 1.0)
      slowdown_risk = calc_base_risk(whale_density, traffic)
    )

  # summarize by area
  risk_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(area_col))) |>
    dplyr::summarize(
      total_base_risk           = sum(base_risk, na.rm = TRUE),
      total_speed_weighted_risk = sum(speed_weighted_risk, na.rm = TRUE),
      total_slowdown_risk       = sum(slowdown_risk, na.rm = TRUE),
      risk_reduction_abs        = total_speed_weighted_risk - total_slowdown_risk,
      risk_reduction_pct        = ifelse(
        total_speed_weighted_risk > 0,
        (risk_reduction_abs / total_speed_weighted_risk) * 100,
        0
      ),
      .groups = "drop"
    )
}

#' Summarize risk by company
#' @param risk_data Data frame with columns: company, whale_density, traffic, speed_bin
#' @param verbose Show debug messages
#' @return Data frame with risk summaries by company
summarize_risk_by_company <- function(risk_data, verbose = interactive()) {

  if (verbose) message("[summarize_risk_by_company] Calculating risk by company")

  summarize_risk_by_area(risk_data, area_col = "company", verbose = verbose)
}

#' Calculate contribution of each area to total risk
#' @param area_risk Data frame from summarize_risk_by_area
#' @param risk_col Which risk column to use for percentages
#' @return Data frame with contribution percentages added
calc_area_contribution <- function(area_risk, risk_col = "total_speed_weighted_risk") {

  total <- sum(area_risk[[risk_col]], na.rm = TRUE)

  area_risk |>
    dplyr::mutate(
      contribution_pct = ifelse(
        total > 0,
        (.data[[risk_col]] / total) * 100,
        0
      )
    )
}

#' Get speed bin SQL expression for DuckDB
#' @return SQL CASE expression string
get_speed_bin_sql <- function() {
  "CASE
    WHEN avg_speed_knots <= 10 THEN '<=10 kt'
    WHEN avg_speed_knots <= 12 THEN '>10-12 kt'
    WHEN avg_speed_knots <= 15 THEN '>12-15 kt'
    ELSE '>15 kt'
  END"
}

#' Get risk factor SQL expression for DuckDB
#' @return SQL CASE expression string
get_risk_factor_sql <- function() {
  "CASE
    WHEN avg_speed_knots <= 10 THEN 1.0
    WHEN avg_speed_knots <= 12 THEN 1.6
    WHEN avg_speed_knots <= 15 THEN 5.4
    ELSE 10.8
  END"
}

# Garrison et al. (2025) lethality-curve functions ----

GARRISON_GLM_URL <- "https://github.com/SEFSC/VesselStrikeRiskModel/raw/main/Data/LethalityCurveGLM_26Apr24.RDS"

#' Download and cache the Garrison et al. (2025) lethality GLM
#' @param data_dir Directory to store RDS file
#' @param verbose Show debug messages
#' @return GLM model object
get_garrison_glm <- function(data_dir = here::here("data"), verbose = interactive()) {
  rds_path <- file.path(data_dir, "LethalityCurveGLM_26Apr24.RDS")
  if (!file.exists(rds_path)) {
    if (verbose) message("[get_garrison_glm] Downloading GLM from SEFSC GitHub...")
    download.file(GARRISON_GLM_URL, rds_path, mode = "wb")
  }
  readRDS(rds_path)
}

#' Classify vessel length into Garrison size categories
#' @param length_m Numeric vector of vessel lengths in meters
#' @param default Default size class for NA lengths
#' @return Character vector of size classes (S/M/L/XL)
assign_vessel_size <- function(length_m, default = "L") {
  dplyr::case_when(
    is.na(length_m)    ~ default,
    length_m >= 106.68 ~ "XL",
    length_m >= 19.812 ~ "L",
    length_m >= 12.192 ~ "M",
    TRUE               ~ "S"
  )
}

#' Get vessel size SQL CASE expression for DuckDB
#' @return SQL CASE expression string
get_vessel_size_sql <- function() {
  "CASE
    WHEN s.length_m IS NULL    THEN 'L'
    WHEN s.length_m >= 106.68  THEN 'XL'
    WHEN s.length_m >= 19.812  THEN 'L'
    WHEN s.length_m >= 12.192  THEN 'M'
    ELSE 'S'
  END"
}

#' Calculate P(lethality) using the Garrison GLM
#' @param speed_knots Numeric vector of vessel speeds in knots
#' @param vessel_size Character vector of size classes (S/M/L/XL)
#' @param glm_model GLM object from get_garrison_glm()
#' @param species Species category (default "Not Humpback")
#' @return Numeric vector of P(lethality) values [0,1]
calc_p_lethal_garrison <- function(speed_knots, vessel_size, glm_model,
                                    species = "Not Humpback") {
  newdata <- data.frame(
    Vess.Speed = speed_knots,
    vess.cat_f = vessel_size,
    spe.HB     = species)
  predict(glm_model, newdata = newdata, se.fit = FALSE, type = "response")
}

#' Summarize Garrison risk by geographic area
#' @param risk_data Data frame with garrison_risk and garrison_slowdown_risk
#' @param area_col Name of area column
#' @param verbose Show debug messages
#' @return Data frame with risk summaries by area
summarize_garrison_risk <- function(risk_data, area_col = "area_label",
                                     verbose = interactive()) {
  if (verbose) message("[summarize_garrison_risk] Calculating risk for column: ", area_col)

  risk_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(area_col))) |>
    dplyr::summarize(
      total_base_risk     = sum(whale_density * traffic, na.rm = TRUE),
      total_garrison_risk = sum(garrison_risk, na.rm = TRUE),
      total_slowdown_risk = sum(garrison_slowdown_risk, na.rm = TRUE),
      risk_reduction_abs  = total_garrison_risk - total_slowdown_risk,
      risk_reduction_pct  = ifelse(
        total_garrison_risk > 0,
        (risk_reduction_abs / total_garrison_risk) * 100, 0),
      .groups = "drop")
}

# whale data processing functions ----

#' Create canonical raster template from goa.geojson bounding box
#' @param goa_sf sf object of study area (reads data/goa.geojson if NULL)
#' @param res Resolution in decimal degrees (default 0.1)
#' @return terra SpatRaster template
create_cell_template <- function(goa_sf = NULL, res = 0.1) {
  if (is.null(goa_sf))
    goa_sf <- sf::st_read(here::here("data/goa.geojson"), quiet = TRUE)

  bb <- sf::st_bbox(goa_sf)

  # snap to resolution grid
  xmin <- floor(bb["xmin"] / res) * res
  xmax <- ceiling(bb["xmax"] / res) * res
  ymin <- floor(bb["ymin"] / res) * res
  ymax <- ceiling(bb["ymax"] / res) * res

  terra::rast(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    res  = res,
    crs  = "EPSG:4326"
  )
}

#' Process whale density shapefile using direct rasterization
#' @param shp_path Path to Rices_Whale_Monthly_Density.shp
#' @param r_template terra SpatRaster template from create_cell_template()
#' @param na_value Value indicating missing data (default -9999)
#' @param verbose Show progress messages
#' @return List with whale_tbl (data frame) and whale_cell_tbl (data frame)
process_whale_density <- function(shp_path, r_template, na_value = -9999, verbose = interactive()) {

  # read whale shapefile
  if (verbose) message("[process_whale_density] Reading shapefile...")
  whales <- sf::st_read(shp_path, quiet = TRUE) |>
    sf::st_transform(4326)
  if (verbose) message("  Read ", nrow(whales), " hexagons")

  if (verbose) message("  Template: ", terra::nrow(r_template), " x ", terra::ncol(r_template),
                       " = ", terra::ncell(r_template), " cells")

  # define months and metrics to process
  months  <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  metrics <- c("n", "cv", "se")

  # rasterize each month/metric combination ----
  if (verbose) message("[process_whale_density] Rasterizing monthly data...")

  all_results <- list()

  for (m in seq_along(months)) { # m = seq_along(months)[1]
    month_name <- months[m]
    month_abb  <- toupper(substr(month_name, 1, 3))

    for (metric in metrics) { # metric = metrics[1]
      col_name <- paste0(month_name, "_", metric)

      if (!col_name %in% names(whales)) {
        if (verbose) message("  Skipping ", col_name, " (not found)")
        next
      }

      # filter valid data
      whales_sub <- whales[whales[[col_name]] != na_value, ]

      # rasterize (cell center must be inside hexagon; no touches since
      # fun="mean" is incompatible with touches=TRUE in terra >= 1.8)
      r <- terra::rasterize(
        terra::vect(whales_sub),
        r_template,
        field = col_name,
        fun   = "mean")

      # extract values with raster cell indices (cell index = cell_id)
      vals <- terra::as.data.frame(r, cells = TRUE, na.rm = TRUE)
      names(vals) <- c("cell_id", "value")

      vals_out <- vals |>
        dplyr::mutate(
          month_int = m,
          month_abb = month_abb,
          metric    = metric
        ) |>
        dplyr::select(cell_id, month_int, month_abb, metric, value)

      all_results[[paste0(month_name, "_", metric)]] <- vals_out
    }
  }

  # combine all monthly results
  whale_cell_monthly <- dplyr::bind_rows(all_results)
  if (verbose) message("  Monthly records: ", nrow(whale_cell_monthly))

  # calculate annual averages (month_int = 0, month_abb = "ALL") ----
  if (verbose) message("[process_whale_density] Calculating annual averages...")

  whale_cell_annual <- whale_cell_monthly |>
    dplyr::group_by(cell_id, metric) |>
    dplyr::summarize(
      value = dplyr::case_when(
        dplyr::first(metric) == "n"  ~ mean(value, na.rm = TRUE),
        dplyr::first(metric) == "cv" ~ mean(value, na.rm = TRUE),
        dplyr::first(metric) == "se" ~ sqrt(mean(value^2, na.rm = TRUE)),
        TRUE ~ mean(value, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      month_int = 0L,
      month_abb = "ALL"
    ) |>
    dplyr::select(cell_id, month_int, month_abb, metric, value)

  # combine monthly and annual
  whale_cell_combined <- dplyr::bind_rows(whale_cell_monthly, whale_cell_annual)
  if (verbose) message("  Total records: ", nrow(whale_cell_combined))

  # whale_cell_combined already has cell_id from raster cell index ----
  whale_cell_tbl <- whale_cell_combined
  if (verbose) message("  Whale_cell rows: ", nrow(whale_cell_tbl))

  # create whale table (metadata for each month/metric combo) ----
  whale_tbl <- whale_cell_tbl |>
    dplyr::group_by(month_int, month_abb, metric) |>
    dplyr::summarize(
      n_cells    = dplyr::n(),
      min_value  = min(value, na.rm = TRUE),
      max_value  = max(value, na.rm = TRUE),
      mean_value = mean(value, na.rm = TRUE),
      sum_value  = sum(value, na.rm = TRUE),
      .groups    = "drop"
    )

  if (verbose) message("  Whale metadata records: ", nrow(whale_tbl))

  list(
    whale_tbl      = whale_tbl,
    whale_cell_tbl = whale_cell_tbl,
    r_template     = r_template
  )
}

#' Import whale data into DuckDB database
#' @param con DuckDB connection
#' @param whale_tbl Data frame from process_whale_density()$whale_tbl
#' @param whale_cell_tbl Data frame from process_whale_density()$whale_cell_tbl
#' @param r_template terra SpatRaster template from create_cell_template()
#' @param overwrite Overwrite existing tables if they exist
#' @param verbose Show progress messages
#' @return TRUE if successful
import_whale_to_duckdb <- function(con, whale_tbl, whale_cell_tbl, r_template,
                                   overwrite = FALSE, verbose = interactive()) {

  # check if tables exist
  existing_tables <- DBI::dbListTables(con)
  whale_exists      <- "whale" %in% existing_tables
  whale_cell_exists <- "whale_cell" %in% existing_tables

  if (whale_exists && !overwrite) {
    if (verbose) message("[import_whale_to_duckdb] Table 'whale' already exists, skipping")
  } else {
    if (whale_exists && overwrite) {
      if (verbose) message("[import_whale_to_duckdb] Dropping existing 'whale' table")
      DBI::dbExecute(con, "DROP TABLE IF EXISTS whale")
    }
    if (verbose) message("[import_whale_to_duckdb] Creating 'whale' table...")
    DBI::dbWriteTable(con, "whale", whale_tbl, overwrite = TRUE)
  }

  if (whale_cell_exists && !overwrite) {
    if (verbose) message("[import_whale_to_duckdb] Table 'whale_cell' already exists, skipping")
  } else {
    if (whale_cell_exists && overwrite) {
      if (verbose) message("[import_whale_to_duckdb] Dropping existing 'whale_cell' table")
      DBI::dbExecute(con, "DROP TABLE IF EXISTS whale_cell")
    }
    if (verbose) message("[import_whale_to_duckdb] Creating 'whale_cell' table...")
    DBI::dbWriteTable(con, "whale_cell", whale_cell_tbl, overwrite = TRUE)
  }

  # insert whale-only cells into cell table ----
  if (verbose) message("[import_whale_to_duckdb] Checking for whale-only cells...")
  DBI::dbExecute(con, "LOAD spatial")

  whale_cell_ids <- unique(whale_cell_tbl$cell_id)
  existing_cell_ids <- DBI::dbGetQuery(con, "SELECT cell_id FROM cell")$cell_id
  new_cell_ids <- setdiff(whale_cell_ids, existing_cell_ids)

  if (length(new_cell_ids) > 0) {
    if (verbose) message("  Adding ", length(new_cell_ids), " whale-only cells to cell table...")

    # get coordinates from raster template for new cell_ids
    xy <- terra::xyFromCell(r_template, new_cell_ids)
    res <- terra::res(r_template)[1]
    new_cells <- data.frame(
      cell_id     = new_cell_ids,
      cell_ll_lon = round(xy[, 1] - res / 2, 1),
      cell_ll_lat = round(xy[, 2] - res / 2, 1)
    )

    DBI::dbWriteTable(con, "cell_new_tmp", new_cells, overwrite = TRUE)
    DBI::dbExecute(con, glue::glue("
      INSERT INTO cell (cell_id, cell_ll_lon, cell_ll_lat, geom)
      SELECT
        cell_id, cell_ll_lon, cell_ll_lat,
        ST_MakeEnvelope(cell_ll_lon, cell_ll_lat, cell_ll_lon + {res}, cell_ll_lat + {res})
      FROM cell_new_tmp
    "))
    DBI::dbExecute(con, "DROP TABLE cell_new_tmp")

    if (verbose) message("  Cell table now has ",
                         DBI::dbGetQuery(con, "SELECT COUNT(*) FROM cell")[[1]], " cells")
  } else {
    if (verbose) message("  All whale cells already exist in cell table")
  }

  if (verbose) {
    message("  whale table: ", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM whale")[[1]], " rows")
    message("  whale_cell table: ", DBI::dbGetQuery(con, "SELECT COUNT(*) FROM whale_cell")[[1]], " rows")
  }

  TRUE
}

#' One-time migration: reassign cell_ids to match terra raster cell indices
#' @param con DuckDB connection (read-write)
#' @param goa_sf sf object of study area (reads data/goa.geojson if NULL)
#' @param verbose Show progress messages
#' @return r_template (the canonical raster template used for new cell_ids)
reassign_cell_ids <- function(con, goa_sf = NULL, verbose = interactive()) {

  if (verbose) message("[reassign_cell_ids] Creating canonical raster template...")
  r_template <- create_cell_template(goa_sf)
  res <- terra::res(r_template)[1]

  if (verbose) message("  Template: ", terra::nrow(r_template), " x ",
                       terra::ncol(r_template), " = ", terra::ncell(r_template), " cells")

  # compute new cell_id for each existing cell ----
  if (verbose) message("[reassign_cell_ids] Computing new cell_ids...")
  cells_db <- DBI::dbGetQuery(con, "SELECT cell_id, cell_ll_lon, cell_ll_lat FROM cell")

  center_lon <- cells_db$cell_ll_lon + res / 2
  center_lat <- cells_db$cell_ll_lat + res / 2
  new_ids <- terra::cellFromXY(r_template, cbind(center_lon, center_lat))

  mapping <- data.frame(
    old_cell_id = cells_db$cell_id,
    new_cell_id = new_ids
  )

  # check for unmapped cells (outside new template extent)
  n_na <- sum(is.na(mapping$new_cell_id))
  if (n_na > 0) {
    if (verbose) message("  WARNING: ", n_na, " cells outside template extent, dropping them")
    mapping <- mapping[!is.na(mapping$new_cell_id), ]
  }

  # check for duplicate new_cell_ids
  n_dup <- sum(duplicated(mapping$new_cell_id))
  if (n_dup > 0)
    stop("  ERROR: ", n_dup, " duplicate new_cell_ids detected. Check resolution/extent.")

  if (verbose) message("  Mapping ", nrow(mapping), " cells (old -> new)")

  # write mapping to temp table ----
  DBI::dbExecute(con, "LOAD spatial")
  DBI::dbWriteTable(con, "cell_id_map", mapping, overwrite = TRUE)

  # recreate ship_cell with new cell_ids ----
  if (verbose) message("[reassign_cell_ids] Migrating ship_cell...")

  # get ship_cell columns (exclude cell_id)
  sc_cols <- DBI::dbListFields(con, "ship_cell")
  sc_cols_other <- setdiff(sc_cols, "cell_id")

  DBI::dbExecute(con, "ALTER TABLE ship_cell RENAME TO ship_cell_old")
  DBI::dbExecute(con, glue::glue("
    CREATE TABLE ship_cell AS
    SELECT m.new_cell_id AS cell_id, {paste(paste0('sc.', sc_cols_other), collapse = ', ')}
    FROM ship_cell_old sc
    JOIN cell_id_map m ON sc.cell_id = m.old_cell_id
  "))
  n_sc <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM ship_cell")[[1]]
  DBI::dbExecute(con, "DROP TABLE ship_cell_old")
  if (verbose) message("  ship_cell: ", n_sc, " rows migrated")

  # recreate cell table with new cell_ids ----
  if (verbose) message("[reassign_cell_ids] Migrating cell table...")
  DBI::dbExecute(con, "ALTER TABLE cell RENAME TO cell_old")
  DBI::dbExecute(con, glue::glue("
    CREATE TABLE cell AS
    SELECT
      m.new_cell_id AS cell_id,
      c.cell_ll_lon, c.cell_ll_lat,
      ST_MakeEnvelope(
        c.cell_ll_lon, c.cell_ll_lat,
        c.cell_ll_lon + {res}, c.cell_ll_lat + {res}) AS geom
    FROM cell_old c
    JOIN cell_id_map m ON c.cell_id = m.old_cell_id
  "))
  n_cell <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM cell")[[1]]
  DBI::dbExecute(con, "DROP TABLE cell_old")
  if (verbose) message("  cell: ", n_cell, " rows migrated")

  # drop whale tables (will be reimported with correct cell_ids) ----
  if (verbose) message("[reassign_cell_ids] Dropping whale/whale_cell tables (will be reimported)...")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS whale")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS whale_cell")

  # clean up
  DBI::dbExecute(con, "DROP TABLE cell_id_map")

  if (verbose) message("[reassign_cell_ids] Migration complete!")

  r_template
}

#' Get vessel traffic raster from DuckDB
#' @param con DuckDB connection
#' @param metric Traffic metric: "km" (distance), "hours", or "n_ships"
#' @param r_template terra SpatRaster template from create_cell_template()
#' @return terra SpatRaster object in EPSG:4326
get_traffic_raster <- function(con, metric = "km", r_template = NULL) {

  if (is.null(r_template))
    r_template <- create_cell_template()

  # build aggregation expression based on metric
  agg_expr <- switch(metric,
    km      = "SUM(dist_travelled_km)",
    hours   = "SUM(hours)",
    n_ships = "COUNT(DISTINCT mmsi)",
    stop("Unknown metric: ", metric)
  )

  sql <- glue::glue("
    SELECT cell_id, {agg_expr} AS value
    FROM ship_cell
    WHERE avg_speed_knots IS NOT NULL
    GROUP BY cell_id
  ")

  traffic_vals <- DBI::dbGetQuery(con, sql)

  r <- terra::init(r_template, NA)
  r[traffic_vals$cell_id] <- traffic_vals$value
  names(r) <- glue::glue("traffic_{metric}")

  r
}

#' Get whale density raster from DuckDB
#' @param con DuckDB connection
#' @param month_int Month integer (1-12 for monthly, 0 for annual average)
#' @param metric Metric to retrieve ("n", "cv", "se")
#' @param r_template terra SpatRaster template from create_cell_template()
#' @return terra SpatRaster object in EPSG:4326
get_whale_raster <- function(con, month_int = 0, metric = "n", r_template = NULL) {

  # create template if not provided
  if (is.null(r_template))
    r_template <- create_cell_template()

  # query whale_cell values (cell_id IS the raster cell index)
  sql <- glue::glue("
    SELECT cell_id, value
    FROM whale_cell
    WHERE month_int = {month_int} AND metric = '{metric}'
  ")

  whale_vals <- DBI::dbGetQuery(con, sql)

  # create output raster and populate values directly
  r <- terra::init(r_template, NA)
  r[whale_vals$cell_id] <- whale_vals$value

  # set layer name
  month_lbl <- if (month_int == 0) "ALL" else toupper(month.abb[month_int])
  names(r) <- glue::glue("whale_{month_lbl}_{metric}")

  r
}

#' Create leaflet map of raster data
#' @param r terra SpatRaster object
#' @param legend_title HTML legend title
#' @param colors Color palette name (default "Spectral")
#' @param group Layer group name
#' @param overlay_sf Optional sf object to overlay as polygons
#' @param overlay_color Color for overlay polygon borders
#' @param overlay_label Label for overlay
#' @return leaflet map object
map_rast_simple <- function(r, legend_title, colors = "Spectral", group = "raster",
                            overlay_sf = NULL, overlay_color = "blue", overlay_label = NULL) {

  # project raster to EPSG:4326 for leaflet
  r_4326 <- terra::project(r, "EPSG:4326")

  # create color palette
  vals <- terra::values(r_4326, na.rm = TRUE)
  pal <- leaflet::colorNumeric(
    colors,
    domain   = vals,
    reverse  = TRUE,
    na.color = "transparent"
  )

  # build map
 m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = leaflet::providerTileOptions(
        variant = "Ocean/World_Ocean_Base"
      )
    ) |>
    leaflet::addProviderTiles(
      "Esri.OceanBasemap",
      options = leaflet::providerTileOptions(
        variant = "Ocean/World_Ocean_Reference"
      )
    ) |>
    leaflet::addRasterImage(
      r_4326,
      colors  = pal,
      opacity = 0.8,
      group   = group
    ) |>
    leaflet::addLegend(
      pal      = pal,
      values   = vals,
      opacity  = 0.7,
      title    = legend_title,
      position = "topright"
    ) |>
    leaflet::addScaleBar(position = "bottomleft")

  # add overlay if provided
  if (!is.null(overlay_sf)) {
    m <- m |>
      leaflet::addPolygons(
        data        = overlay_sf,
        fillColor   = "transparent",
        color       = overlay_color,
        weight      = 2,
        opacity     = 1,
        label       = overlay_label
      )
  }

  # add fullscreen control if available
  if (requireNamespace("leaflet.extras", quietly = TRUE)) {
    m <- m |> leaflet.extras::addFullscreenControl()
  }

  m
}
