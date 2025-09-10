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
  
  c(
    min = lubridate::month(dates$min_date),
    max = lubridate::month(dates$max_date))
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
  
  c(
    min = speeds$min_speed, 
    max = speeds$max_speed)
}

# data processing functions ----

#' Build WHERE clause for filtering
#' @param con Database connection
#' @param company_filter Company filter selection
#' @param month_range Month range (numeric 1-12)
#' @param speed_range Speed range
#' @param selected_ships Selected ship MMSIs
#' @return SQL WHERE clause
build_where_clause <- function(con, company_filter = NULL, month_range = NULL, 
                              speed_range = NULL, selected_ships = NULL) {
  
  # Company clause
  company_clause <- switch(
    company_filter,
    "All" = NULL,
    "All Companies" = "EXISTS (SELECT 1 FROM company c2 WHERE s.operator ILIKE '%' || c2.operator_lookup || '%')",
    "Other Companies" = "NOT EXISTS (SELECT 1 FROM company c2 WHERE s.operator ILIKE '%' || c2.operator_lookup || '%')",
    {
      glue::glue_sql(
        "EXISTS (SELECT 1 FROM company c2 WHERE c2.company = {company_filter} AND s.operator ILIKE '%' || c2.operator_lookup || '%')",
        .con = con
      )
    }
  )
  
  # Month clause
  month_clause <- NULL
  if (!is.null(month_range) && length(month_range) == 2 && !identical(month_range, c(1, 12))) {
    month_clause <- glue::glue_sql(
      "EXTRACT(MONTH FROM sc.date) BETWEEN {month_range[1]} AND {month_range[2]}",
      .con = con
    )
  }
  
  # Speed clause
  speed_clause <- NULL
  if (!is.null(speed_range) && length(speed_range) == 2) {
    speed_clause <- glue::glue_sql(
      "sc.avg_speed_knots BETWEEN {speed_range[1]} AND {speed_range[2]}",
      .con = con
    )
  }
  
  # Ship MMSI clause
  ship_clause <- NULL
  if (!is.null(selected_ships) && length(selected_ships) > 0) {
    ship_clause <- glue::glue_sql(
      "sc.mmsi IN ({vals*})",
      vals = selected_ships,
      .con = con
    )
  }
  
  # Assemble WHERE clause
  predicates <- Filter(Negate(is.null), list(company_clause, month_clause, speed_clause, ship_clause))
  
  if (length(predicates)) {
    DBI::SQL(paste0("WHERE ", paste(predicates, collapse = " AND ")))
  } else {
    DBI::SQL("")
  }
}

#' Get map data with cell metrics aggregated in database
#' @param con Database connection
#' @param company_filter Selected company filter
#' @param month_range Selected month range (numeric 1-12)
#' @param speed_range Selected speed range
#' @param selected_ships Optional vector of selected ship MMSIs
#' @param metric Selected metric to calculate
#' @param weight_by Weight speeds by "hours" or "kilometers"
#' @param verbose Show debug messages
#' @return sf object with cells and calculated metric
get_map_data <- function(con, company_filter, month_range, speed_range, 
                        selected_ships = NULL, metric, weight_by = "hours", 
                        verbose = interactive()) {
  
  where_sql <- build_where_clause(con, company_filter, month_range, speed_range, selected_ships)
  
  # Build metric calculation based on selection
  metric_sql <- switch(
    metric,
    "avg speed" = if (weight_by == "hours") {
      "SUM(sc.avg_speed_knots * sc.hours) / NULLIF(SUM(sc.hours), 0)"
    } else {
      "SUM(sc.avg_speed_knots * sc.dist_travelled_km) / NULLIF(SUM(sc.dist_travelled_km), 0)"
    },
    "min speed" = "MIN(sc.avg_speed_knots)",
    "max speed" = "MAX(sc.avg_speed_knots)",
    "km traveled" = "SUM(sc.dist_travelled_km)",
    "hrs traveled" = "SUM(sc.hours)",
    "# unique ships" = "COUNT(DISTINCT sc.mmsi)",
    "# ship-cell records" = "COUNT(*)"
  )
  
  # Main aggregation query
  query <- glue::glue_sql("
    SELECT
      c.cell_id,
      c.cell_ll_lon,
      c.cell_ll_lat,
      ST_AsText(c.geom) AS geom_wkt,
      {DBI::SQL(metric_sql)} AS metric_value
    FROM cell c
    JOIN ship_cell sc ON c.cell_id = sc.cell_id
    JOIN ship s ON sc.mmsi = s.mmsi
    {where_sql}
    GROUP BY c.cell_id, c.cell_ll_lon, c.cell_ll_lat, c.geom
    HAVING {DBI::SQL(metric_sql)} IS NOT NULL
  ", .con = con)
  
  if (verbose) {
    message("[get_map_data] Running aggregation query:")
    message("  Company filter: ", company_filter)
    message("  Month range: ", ifelse(is.null(month_range), "All", paste(month_range, collapse="-")))
    message("  Speed range: ", ifelse(is.null(speed_range), "All", paste(speed_range, collapse="-")))
    message("  Selected ships: ", ifelse(is.null(selected_ships), "None", length(selected_ships)))
    message("  Metric: ", metric)
    message("  Weight by: ", weight_by)
  }
  
  start_time <- Sys.time()
  result <- DBI::dbGetQuery(con, query)
  query_time <- Sys.time() - start_time
  
  if (verbose) {
    message("[get_map_data] Query completed in ", round(query_time, 2), " ", units(query_time))
    message("[get_map_data] Returned ", nrow(result), " cells")
  }
  
  if (nrow(result) == 0) {
    return(NULL)
  }
  
  # Convert to sf object
  sf::st_as_sf(result, wkt = "geom_wkt", crs = 4326)
}

#' Get ships summary aggregated in database
#' @param con Database connection
#' @param company_filter Selected company filter
#' @param month_range Selected month range (numeric 1-12)
#' @param speed_range Selected speed range
#' @param selected_ships Optional vector of selected ship MMSIs
#' @param verbose Show debug messages
#' @return Data frame with ship summaries
get_ships_data <- function(con, company_filter, month_range, speed_range, 
                          selected_ships = NULL, verbose = interactive()) {
  
  where_sql <- build_where_clause(con, company_filter, month_range, speed_range, selected_ships)
  
  query <- glue::glue_sql("
    SELECT
      sc.mmsi,
      MAX(s.name_of_ship) AS name_of_ship,
      MAX(s.operator) AS operator,
      COUNT(*) AS n_records,
      MIN(sc.date) AS date_min,
      MAX(sc.date) AS date_max
    FROM ship_cell sc
    JOIN ship s ON sc.mmsi = s.mmsi
    JOIN cell c ON sc.cell_id = c.cell_id
    {where_sql}
    GROUP BY sc.mmsi
    ORDER BY COUNT(*) DESC
  ", .con = con)
  
  if (verbose) {
    message("[get_ships_data] Running ship summary query")
  }
  
  start_time <- Sys.time()
  result <- DBI::dbGetQuery(con, query)
  query_time <- Sys.time() - start_time
  
  if (verbose) {
    message("[get_ships_data] Query completed in ", round(query_time, 2), " ", units(query_time))
    message("[get_ships_data] Found ", nrow(result), " unique ships")
  }
  
  tibble::as_tibble(result)
}

#' Get time series data aggregated in database
#' @param con Database connection
#' @param company_filter Selected company filter
#' @param month_range Selected month range (numeric 1-12)
#' @param speed_range Selected speed range
#' @param selected_ships Optional vector of selected ship MMSIs
#' @param metric Selected metric to calculate
#' @param weight_by Weight speeds by "hours" or "kilometers"
#' @param verbose Show debug messages
#' @return Data frame with date and metric value
get_time_series_data <- function(con, company_filter, month_range, speed_range,
                                selected_ships = NULL, metric, weight_by = "hours",
                                verbose = interactive()) {
  
  where_sql <- build_where_clause(con, company_filter, month_range, speed_range, selected_ships)
  
  # Build metric calculation
  metric_sql <- switch(
    metric,
    "avg speed" = if (weight_by == "hours") {
      "SUM(sc.avg_speed_knots * sc.hours) / NULLIF(SUM(sc.hours), 0)"
    } else {
      "SUM(sc.avg_speed_knots * sc.dist_travelled_km) / NULLIF(SUM(sc.dist_travelled_km), 0)"
    },
    "min speed" = "MIN(sc.avg_speed_knots)",
    "max speed" = "MAX(sc.avg_speed_knots)",
    "km traveled" = "SUM(sc.dist_travelled_km)",
    "hrs traveled" = "SUM(sc.hours)",
    "# unique ships" = "COUNT(DISTINCT sc.mmsi)",
    "# ship-cell records" = "COUNT(*)"
  )
  
  query <- glue::glue_sql("
    SELECT
      sc.date,
      {DBI::SQL(metric_sql)} AS value
    FROM ship_cell sc
    JOIN ship s ON sc.mmsi = s.mmsi
    JOIN cell c ON sc.cell_id = c.cell_id
    {where_sql}
    GROUP BY sc.date
    HAVING {DBI::SQL(metric_sql)} IS NOT NULL
    ORDER BY sc.date
  ", .con = con)
  
  if (verbose) {
    message("[get_time_series_data] Running time series query for metric: ", metric)
  }
  
  start_time <- Sys.time()
  result <- DBI::dbGetQuery(con, query)
  query_time <- Sys.time() - start_time
  
  if (verbose) {
    message("[get_time_series_data] Query completed in ", round(query_time, 2), " ", units(query_time))
    message("[get_time_series_data] Returned ", nrow(result), " dates")
  }
  
  if (nrow(result) == 0) {
    # Return dummy data for empty results
    return(data.frame(
      date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
      value = 0
    ))
  }
  
  tibble::as_tibble(result)
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
  pal <- leaflet::colorNumeric(
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


# Legacy functions kept for compatibility but marked as deprecated ----

#' @rdname deprecated-functions
#' @export
filter_ship_data <- function(...) {
  .Deprecated("get_map_data, get_ships_data, or get_time_series_data")
  stop("This function has been replaced. Use get_map_data(), get_ships_data(), or get_time_series_data() instead.")
}

#' @rdname deprecated-functions
#' @export
calculate_cell_metrics <- function(...) {
  .Deprecated("get_map_data")
  stop("This function has been replaced. Use get_map_data() instead.")
}

#' @rdname deprecated-functions  
#' @export
get_ships_summary <- function(...) {
  .Deprecated("get_ships_data")
  stop("This function has been replaced. Use get_ships_data() instead.")
}

#' @rdname deprecated-functions
#' @export
create_time_series_data <- function(...) {
  .Deprecated("get_time_series_data")
  stop("This function has been replaced. Use get_time_series_data() instead.")
}