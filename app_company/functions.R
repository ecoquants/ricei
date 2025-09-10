# functions.R - Helper functions for the Gulf of America shipping app

# database functions ----

#' Connect to the GOA DuckDB database
#' @param db_path Path to the DuckDB database file
#' @param verbose Show debug messages
#' @return DuckDB connection object
connect_to_db <- function(db_path, verbose = interactive()) {
  if (verbose) message("[connect_to_db] Connecting to database: ", db_path)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  if (verbose) message("[connect_to_db] Loading spatial extension...")
  # duckdbfs::load_spatial(con)
  DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")
  if (verbose) message("[connect_to_db] Connection established")
  return(con)
}

#' Get list of companies from database
#' @param con Database connection
#' @param verbose Show debug messages
#' @return Character vector of company names including special options
get_company_choices <- function(con, verbose = interactive()) {
  query <- "SELECT DISTINCT company FROM company ORDER BY company"
  if (verbose) message("[get_company_choices] Running query: ", query)
  companies <- DBI::dbGetQuery(con, query) |>
    dplyr::pull(company)
  if (verbose) message("[get_company_choices] Found ", length(companies), " companies")
  
  c("All" = "All",
    "All Companies" = "All Companies", 
    "Other Companies" = "Other Companies",
    setNames(companies, companies))
}

#' Get month range from ship_cell data
#' @param con Database connection
#' @param verbose Show debug messages
#' @return List with min and max months as numeric values (1-12)
get_month_range <- function(con, verbose = interactive()) {
  query <- "SELECT MIN(date) as min_date, MAX(date) as max_date FROM ship_cell"
  if (verbose) message("[get_month_range] Running query: ", query)
  dates <- DBI::dbGetQuery(con, query)
  if (verbose) message("[get_month_range] Date range: ", dates$min_date, " to ", dates$max_date)
  
  list(
    min = lubridate::month(dates$min_date),
    max = lubridate::month(dates$max_date)
  )
}

#' Get speed range from ship_cell data
#' @param con Database connection
#' @param verbose Show debug messages
#' @return Named numeric vector with min and max speeds
get_speed_range <- function(con, verbose = interactive()) {
  query <- "SELECT MIN(avg_speed_knots) as min_speed, MAX(avg_speed_knots) as max_speed FROM ship_cell WHERE avg_speed_knots IS NOT NULL"
  if (verbose) message("[get_speed_range] Running query: ", query)
  speeds <- DBI::dbGetQuery(con, query)
  if (verbose) message("[get_speed_range] Speed range: ", speeds$min_speed, " to ", speeds$max_speed, " knots")
  
  c(min = speeds$min_speed, max = speeds$max_speed)
}

# data processing functions ----

#' Filter ship data based on user inputs
#' @param con Database connection
#' @param company_filter Selected company filter
#' @param month_range Selected month range (numeric 1-12)
#' @param speed_range Selected speed range
#' @param selected_ships Optional vector of selected ship MMSIs
#' @param verbose Show debug messages
#' @return Filtered data
filter_ship_data <- function(
    con, company_filter, month_range, speed_range, 
    selected_ships = NULL, verbose = interactive()) {
  
  # build company filter SQL
  company_sql <- if (company_filter == "All") {
    "1=1"  # no filter
  } else if (company_filter == "All Companies") {
    "s.operator IN (SELECT operator_lookup FROM company)"
  } else if (company_filter == "Other Companies") {
    "s.operator NOT IN (SELECT operator_lookup FROM company)"
  } else {
    glue::glue_sql("s.operator IN (
      SELECT operator_lookup FROM company WHERE company = {company_filter}
    )", .con = con)
  }
  
  # build ship filter SQL if ships are selected
  ship_sql <- if (!is.null(selected_ships) && length(selected_ships) > 0) {
    mmsi_list <- paste(selected_ships, collapse = ",")
    glue::glue("AND sc.mmsi IN ({mmsi_list})")
  } else {
    ""
  }
  
  # main query
  query <- glue::glue("
    SELECT 
      c.cell_id,
      c.cell_ll_lon,
      c.cell_ll_lat,
      sc.date,
      sc.mmsi,
      sc.hours,
      sc.dist_travelled_km,
      sc.avg_speed_knots,
      s.name_of_ship,
      s.operator,
      ST_AsText(c.geom) as geom_wkt
    FROM cell c
    JOIN ship_cell sc ON c.cell_id = sc.cell_id
    JOIN ship s ON sc.mmsi = s.mmsi
    WHERE {company_sql}
      AND EXTRACT(MONTH FROM sc.date) BETWEEN {month_range[1]} AND {month_range[2]}
      AND sc.avg_speed_knots BETWEEN {speed_range[1]} AND {speed_range[2]}
      {ship_sql}
  ")
  
  if (verbose) {
    message("[filter_ship_data] Running main query:")
    message("  Company filter: ", company_filter)
    message("  Month range: ", month_range[1], "-", month_range[2])
    message("  Speed range: ", speed_range[1], "-", speed_range[2], " knots")
    message("  Selected ships: ", ifelse(is.null(selected_ships), "None", length(selected_ships)))
    message("  Query preview: ", substr(query, 1, 200), "...")
  }
  
  start_time <- Sys.time()
  result <- DBI::dbGetQuery(con, query)
  query_time <- Sys.time() - start_time
  
  if (verbose) {
    message("[filter_ship_data] Query completed in ", round(query_time, 2), " ", units(query_time))
    message("[filter_ship_data] Returned ", nrow(result), " rows")
  }
  
  result
}

#' Calculate metrics by cell for choropleth map
#' @param data Filtered ship data
#' @param metric Selected metric to calculate
#' @param weight_by Weight speeds by "hours" or "kilometers"
#' @param verbose Show debug messages
#' @return sf object with cells and calculated metric
calculate_cell_metrics <- function(data, metric, weight_by = "hours", verbose = interactive()) {
  
  if (verbose) message("[calculate_cell_metrics] Processing ", nrow(data), " rows for metric: ", metric)
  
  if (nrow(data) == 0) {
    if (verbose) message("[calculate_cell_metrics] No data to process")
    return(NULL)
  }
  
  # group by cell and calculate metrics
  cell_data <- data |>
    dplyr::group_by(cell_id, cell_ll_lon, cell_ll_lat, geom_wkt) |>
    dplyr::summarise(
      avg_speed = if (weight_by == "hours") {
        weighted.mean(avg_speed_knots, hours, na.rm = TRUE)
      } else {
        weighted.mean(avg_speed_knots, dist_travelled_km, na.rm = TRUE)
      },
      min_speed = min(avg_speed_knots, na.rm = TRUE),
      max_speed = max(avg_speed_knots, na.rm = TRUE),
      km_traveled = sum(dist_travelled_km, na.rm = TRUE),
      hrs_traveled = sum(hours, na.rm = TRUE),
      n_ships = n_distinct(mmsi),
      n_records = n(),
      .groups = "drop"
    )
  
  # select the requested metric
  metric_col <- switch(metric,
    "avg speed" = "avg_speed",
    "min speed" = "min_speed", 
    "max speed" = "max_speed",
    "km traveled" = "km_traveled",
    "hrs traveled" = "hrs_traveled",
    "# unique ships" = "n_ships",
    "# ship-cell records" = "n_records"
  )
  
  cell_data$metric_value <- cell_data[[metric_col]]
  
  # convert to sf object
  if (verbose) message("[calculate_cell_metrics] Converting to sf object...")
  result <- sf::st_as_sf(cell_data, wkt = "geom_wkt", crs = 4326) |>
    dplyr::select(cell_id, metric_value, everything())
  
  if (verbose) message("[calculate_cell_metrics] Created sf object with ", nrow(result), " cells")
  result
}

#' Get unique ships summary
#' @param data Filtered ship data
#' @param verbose Show debug messages
#' @return Data frame with ship summaries
get_ships_summary <- function(data, verbose = interactive()) {
  if (verbose) message("[get_ships_summary] Summarizing ships from ", nrow(data), " records")
  
  if (nrow(data) == 0) {
    if (verbose) message("[get_ships_summary] No data to summarize")
    return(data.frame(
      mmsi = character(),
      name_of_ship = character(),
      operator = character(),
      n_records = integer(),
      date_min = as.Date(character()),
      date_max = as.Date(character())
    ))
  }
  
  result <- data |>
    dplyr::group_by(mmsi) |>
    dplyr::summarise(
      name_of_ship = dplyr::first(name_of_ship),
      operator = dplyr::first(operator),
      n_records = n(),
      date_min = min(date, na.rm = TRUE),
      date_max = max(date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(desc(n_records))
  
  if (verbose) message("[get_ships_summary] Found ", nrow(result), " unique ships")
  result
}

#' Create time series data
#' @param data Filtered ship data
#' @param metric Selected metric to calculate
#' @param weight_by Weight speeds by "hours" or "kilometers"
#' @param verbose Show debug messages
#' @return Data frame with date and metric value
create_time_series_data <- function(data, metric, weight_by = "hours", verbose = interactive()) {
  
  if (verbose) message("[create_time_series_data] Creating time series for metric: ", metric)
  
  if (nrow(data) == 0) {
    if (verbose) message("[create_time_series_data] No data for time series")
    return(data.frame(date = as.Date(character()), value = numeric()))
  }
  
  # aggregate by date
  ts_data <- data |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      avg_speed = if (weight_by == "hours") {
        weighted.mean(avg_speed_knots, hours, na.rm = TRUE)
      } else {
        weighted.mean(avg_speed_knots, dist_travelled_km, na.rm = TRUE)
      },
      min_speed = min(avg_speed_knots, na.rm = TRUE),
      max_speed = max(avg_speed_knots, na.rm = TRUE),
      km_traveled = sum(dist_travelled_km, na.rm = TRUE),
      hrs_traveled = sum(hours, na.rm = TRUE),
      n_ships = n_distinct(mmsi),
      n_records = n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(date)
  
  # select the requested metric
  metric_col <- switch(metric,
    "avg speed" = "avg_speed",
    "min speed" = "min_speed",
    "max speed" = "max_speed", 
    "km traveled" = "km_traveled",
    "hrs traveled" = "hrs_traveled",
    "# unique ships" = "n_ships",
    "# ship-cell records" = "n_records"
  )
  
  ts_data$value <- ts_data[[metric_col]]
  
  result <- ts_data |> dplyr::select(date, value)
  
  if (verbose) message("[create_time_series_data] Created time series with ", nrow(result), " dates")
  result
}

# visualization functions ----

#' Create choropleth map with mapgl
#' @param cell_sf sf object with cells and metric values
#' @param metric_name Name of the metric being displayed
#' @param verbose Show debug messages
#' @return mapgl map object
create_choropleth_map <- function(cell_sf, metric_name, verbose = interactive()) {
  
  if (verbose) message("[create_choropleth_map] Creating map for: ", metric_name)
  
  if (is.null(cell_sf) || nrow(cell_sf) == 0) {
    if (verbose) message("[create_choropleth_map] No data to map, returning empty map")
    # return empty map centered on Gulf of America
    return(
      mapgl::mapboxgl(
        center = c(-89, 26),
        zoom = 5,
        style = "mapbox://styles/mapbox/light-v11"
      )
    )
  }
  
  # create color palette
  pal <- colorNumeric(
    palette = "Spectral",
    domain = cell_sf$metric_value,
    reverse = TRUE
  )
  
  # add colors to sf object
  cell_sf$fill_color <- pal(cell_sf$metric_value)
  
  # create map
  map <- mapgl::mapboxgl(
    center = c(
      mean(sf::st_coordinates(sf::st_centroid(cell_sf))[,1]),
      mean(sf::st_coordinates(sf::st_centroid(cell_sf))[,2])
    ),
    zoom = 5,
    style = "mapbox://styles/mapbox/light-v11"
  ) |>
    mapgl::add_fill_layer(
      id = "cells",
      source = cell_sf,
      fill_color = "fill_color",
      fill_opacity = 0.7,
      popup = paste0(
        "<b>", metric_name, ":</b> ", 
        round(cell_sf$metric_value, 2)
      )
    )
  
  return(map)
}

#' Create time series plot with dygraphs
#' @param ts_data Data frame with date and value columns
#' @param metric_name Name of the metric being displayed
#' @param verbose Show debug messages
#' @return dygraph object
create_time_series_plot <- function(ts_data, metric_name, verbose = interactive()) {
  
  if (verbose) message("[create_time_series_plot] Creating plot for: ", metric_name)
  
  if (nrow(ts_data) == 0) {
    if (verbose) message("[create_time_series_plot] No data, using dummy data")
    ts_data <- data.frame(
      date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
      value = 0
    )
  }
  
  # convert to xts for dygraphs
  ts_xts <- xts::xts(ts_data$value, order.by = ts_data$date)
  
  dygraphs::dygraph(ts_xts, main = paste(metric_name, "over time")) |>
    dygraphs::dySeries("V1", label = metric_name) |>
    dygraphs::dyRangeSelector() |>
    dygraphs::dyOptions(
      strokeWidth = 2,
      drawPoints = TRUE,
      pointSize = 3
    )
}