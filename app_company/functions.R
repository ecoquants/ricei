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

#' Filter ship data based on user inputs (DuckDB + glue_sql, ILIKE semantics)
#' @param con DBI connection (DuckDB)
#' @param company_filter One of "All", "All Companies", "Other Companies", or a specific company name
#' @param month_range length-2 numeric (1-12). Omit/NULL or c(1,12) => no month filter
#' @param speed_range length-2 numeric. Omit/NULL => no speed filter
#' @param selected_ships optional numeric/character vector of MMSIs. Omit/empty => no ship filter
#' @param verbose logical
#' @return data.frame (tibble) of filtered rows
filter_ship_data <- function(
    con,
    company_filter = NULL,
    month_range    = NULL,
    speed_range    = NULL,
    selected_ships = NULL,
    verbose        = interactive()
) {
  
  # ---- Company clause (ILIKE via EXISTS) ----
  company_clause <- switch(
    company_filter,
    "All" = NULL,  # no company restriction
    "All Companies" =
      # Any ship whose operator ILIKE any operator_lookup in company table
      "EXISTS (SELECT 1
               FROM company c2
               WHERE s.operator ILIKE '%' || c2.operator_lookup || '%')",
    "Other Companies" =
      # Ship operators that do NOT ILIKE any operator_lookup in company table
      "NOT EXISTS (SELECT 1
                   FROM company c2
                   WHERE s.operator ILIKE '%' || c2.operator_lookup || '%')",
    {
      # Specific company: ILIKE any operator_lookup belonging to that company
      glue::glue_sql(
        "EXISTS (SELECT 1
                 FROM company c2
                 WHERE c2.company = {company_filter}
                   AND s.operator ILIKE '%' || c2.operator_lookup || '%')",
        .con = con
      )
    }
  )
  
  # ---- Month clause (only if not full range 1..12 and provided) ----
  month_clause <- NULL
  if (!is.null(month_range) && length(month_range) == 2) {
    # Only add if not the full 1..12
    if (!(identical(month_range, c(1, 12)))) {
      month_clause <- glue::glue_sql(
        "EXTRACT(MONTH FROM sc.date) BETWEEN {month_range[1]} AND {month_range[2]}",
        .con = con
      )
    }
  }
  
  # ---- Speed clause (only if provided) ----
  speed_clause <- NULL
  if (!is.null(speed_range) && length(speed_range) == 2) {
    speed_clause <- glue::glue_sql(
      "sc.avg_speed_knots BETWEEN {speed_range[1]} AND {speed_range[2]}",
      .con = con
    )
  }
  
  # ---- Ship MMSI clause (only if provided and non-empty) ----
  ship_clause <- NULL
  if (!is.null(selected_ships) && length(selected_ships) > 0) {
    ship_clause <- glue::glue_sql(
      "sc.mmsi IN ({vals*})",
      vals = selected_ships,
      .con = con
    )
  }
  
  # ---- Assemble WHERE predicates (only those that exist) ----
  predicates <- Filter(Negate(is.null), list(company_clause, month_clause, speed_clause, ship_clause))
  
  where_sql <- if (length(predicates)) {
    DBI::SQL(paste0("WHERE ", paste(predicates, collapse = " AND ")))
  } else {
    DBI::SQL("") # no WHERE
  }
  
  # ---- Main query (use glue_sql for identifiers & safety) ----
  query <- glue::glue_sql("
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
      ST_AsText(c.geom) AS geom_wkt
    FROM cell c
    JOIN ship_cell sc ON c.cell_id = sc.cell_id
    JOIN ship s      ON sc.mmsi   = s.mmsi
    {where_sql}
  ", .con = con)
  
  if (verbose) {
    message("[filter_ship_data] Running main query:")
    message("  Company filter: ", company_filter)
    if (!is.null(month_range)) message("  Month range: ",
                                       if (identical(month_range, c(1,12))) "All months"
                                       else paste0(month_range[1], "-", month_range[2]))
    if (!is.null(speed_range)) message("  Speed range: ",
                                       paste(speed_range, collapse = " - "), " knots")
    message("  Selected ships: ",
            if (is.null(selected_ships)) "None" else length(selected_ships))
    message("  Query preview: ", substr(as.character(query), 1, 200), "...")
  }
  
  # browser()
  
  t0 <- Sys.time()
  result <- DBI::dbGetQuery(con, query)
  dt <- Sys.time() - t0
  
  if (verbose) {
    message("[filter_ship_data] Query completed in ", round(dt, 2), " ", units(dt))
    message("[filter_ship_data] Returned ", nrow(result), " rows")
  }
  
  tibble::as_tibble(result)
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