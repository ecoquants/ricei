## 2025-09-26.a fix ship selection

```bash
cd /Users/bbest/Github/ecoquants/ricei
claude
```

Something is amiss with the Shiny app's reactivity for ship selection in
@app_company/server.R. When I select a ship (e.g. mmsi = 367703000, the first in 
the Ships panel datatable) the selection flickers and then returns to all ships. 
Here's the diagnostic output in the Console when trying to select a ship:

```
[get_ships_data] Query completed in 0.01 secs
[get_ships_data] Found 1 unique ships
[get_ships_data] Running ship summary query
[get_ships_data] Query completed in 0.37 secs
[get_ships_data] Found 6495 unique ships
```

Try to fix the problem and meanwhile add more diagnostic information to the
reactive functions for debugging.

⏺ Fixed the issue and added diagnostic logging

  The problem was that rx_ships_data() was re-running whenever selected_ships() changed, causing the DataTable to re-render and lose its selection state.

  Key changes made:

  1. Fixed the reactivity cycle in rx_ships_data() (line 58) by removing the selected_ships parameter - the ships table should always show all ships regardless of
  selection
  2. Added comprehensive debug logging throughout the reactive functions:
    - [rx_map_data] logs when triggered with current ship selection
    - [rx_time_series_data] logs when triggered with current ship selection
    - [observe ship selection] logs selected rows and MMSIs
    - [ship_subset_text] logs current selection state
    - [reset_ships] logs when reset button is clicked

  The ship selection should now persist properly without flickering. The diagnostic messages will help track the reactive flow and identify any remaining issues.

## 2025-09-10.a optimize queries by folding in filtering and aggregation

Separating the `filtered_data()` from `cell_metrics()` in @app_company/server.R
makes logical sense, but the query runs way too slow since so many rows are
getting returned from `filtered_data()`. Instead, fold `cell_metrics()` in 
@app_company/server.R into `filtered_data()` (i.e., input of `input$weighting`, 
`input$metric`) and therefore also into `filter_ship_data()` in 
@app_company/functions.R. This way, the SQL query can do all the filtering and 
aggregation in the database.

Similarly update the `ships_summary()` and `time_series_data()` functions in
@app_company/server.R and there corresponding functions `get_ships_summary()`
and `create_time_series_data()` in @app_company/functions.R. This will reduce the
amount of data being pulled from the database and speed up the app significantly.

Also rename consistently all reactive functions in @app_company/server.R
to be prefixed with `rx_` and apply the nav panel type (e.g., `rx_map_data()`, 
and the corresponding functions in @app_company/functions.R to be prefixed with 
`get_` (e.g., `get_map_data()`).

⏺ Perfect! I've successfully refactored the Shiny app to optimize performance by:

  Major Performance Improvements

  1. Database-Level Aggregation

  - Before: Pulled all raw ship_cell data into R, then aggregated in memory
  - After: All aggregation happens in the database using SQL GROUP BY and aggregation functions
  - Impact: Dramatically reduces data transfer and leverages database optimization

  2. New Function Architecture

  - get_map_data(): Aggregates metrics by cell directly in SQL
  - get_ships_data(): Summarizes ships directly in SQL
  - get_time_series_data(): Aggregates time series directly in SQL
  - build_where_clause(): Shared filtering logic

  3. Consistent Naming

  - Reactive functions: rx_map_data(), rx_ships_data(), rx_time_series_data()
  - Database functions: get_map_data(), get_ships_data(), get_time_series_data()

  4. Smart SQL Generation

  - Uses glue_sql() for safe parameter binding
  - Conditional WHERE clauses (only adds filters when needed)
  - Proper handling of company filtering with ILIKE and EXISTS subqueries
  - Weighted averages calculated directly in SQL

  5. Key Performance Benefits

  Before (pulling all data):
  -- Could return millions of rows
  SELECT * FROM cell c
  JOIN ship_cell sc ON c.cell_id = sc.cell_id
  JOIN ship s ON sc.mmsi = s.mmsi
  WHERE [filters]

  After (aggregated results):
  -- Returns only aggregated cells (~thousands of rows max)
  SELECT c.cell_id, c.cell_ll_lon, c.cell_ll_lat,
         ST_AsText(c.geom) AS geom_wkt,
         SUM(sc.hours) AS metric_value
  FROM cell c JOIN ship_cell sc ON c.cell_id = sc.cell_id
  JOIN ship s ON sc.mmsi = s.mmsi
  WHERE [filters]
  GROUP BY c.cell_id, c.cell_ll_lon, c.cell_ll_lat, c.geom

  6. Legacy Function Handling

  - Old functions are marked as deprecated with helpful error messages
  - Prevents accidental use of slow functions
  - Clear migration path for any external code

  The app should now be significantly faster because:
  - Reduced data transfer: From millions of rows to thousands
  - Database optimization: Leverages DuckDB's columnar storage and vectorized operations
  - Parallel processing: Each reactive function runs independently
  - Cached results: Shiny's reactive system caches results until inputs change

  The verbose logging will show you exactly how much faster the queries run, and you should see query times drop from potentially
  minutes to seconds or less.

## 2025-09-09.c debug with `verbose=interactive()` function arguments

Ok, looking good! Except the app hangs on a duckdb query 0% and then crashes. 
Add verbose flags to functions with default values set to boolean of 
`interactive()` so when I interactively run the Shiny app in R, the Console 
shows me which function and query is running (and hanging) for debugging

⏺ I've added comprehensive verbose logging to all the functions in the Shiny app. Here's what I've done:

  1. Added verbose = interactive() parameter to all functions - This will automatically enable debug messages when running
  interactively in R
  2. Added detailed logging messages that show:
    - Function entry points with [function_name] prefix
    - SQL queries being executed
    - Query parameters (company filter, month range, speed range, etc.)
    - Query execution time
    - Number of rows returned
    - Processing steps
  3. Created a test script (test_db.R) that will help diagnose database connection issues:
    - Checks if database file exists
    - Tests basic connection
    - Lists tables
    - Tests spatial extension loading
    - Runs simple queries on each table
    - Tests the specific queries used by the app
    - Tests join queries

  To debug your hanging issue:

  1. First run the test script to identify where the problem is:
  source("app_company/test_db.R")
  2. Then run the Shiny app and watch the console output:
  shiny::runApp("app_company")

  The verbose output will show you exactly:
  - Which function is being called
  - What query is being executed
  - Where the app is hanging

  Common issues that could cause hanging:
  - Large query results (the verbose output shows row counts)
  - Missing spatial extension
  - Database file permissions
  - Complex joins taking too long
  - Missing indexes on join columns

  The test script will help identify if it's a database connection issue, spatial extension issue, or query-specific problem. Once
  you run it and see where it fails, we can address the specific issue.


## 2025-09-09.b app_company creation

Think deeply about creating a Shiny app (with separate `global.R`, `ui.R` and 
`server.R` files) in folder `app_company` that allows users to explore and 
visualize data about shipping data from companies in the Gulf of America 
duck database (`goa.duckdb`).

See @goa.qmd for paths and details on the database, also
summarized by the following one to many (`->`) relationships:

```
cell.cell_id   -> ship_cell.cell_id
ship_cell.mmsi -> ship.mmsi
ship.operator  -> company.operator_lookup
```

Use the llms.txt documentation for the following R libraries:
- mapgl: @tmp/mapgl_llms.txt
- bslib: @tmp/bslib_llms.txt

The app should have the following features using `bslib::navset_card_tab()` layout:
- `bslib::sidebar` for filtering:
  - Company: `shiny::selectInput` from `company.company`, plus options:
    - "All Companies" for all those in `company.company`
    - "Other Companies" besides those listed in `company.company`
    - "All"
  - Months:  `shiny::sliderInput` to filter by month (Jan-Dec; using `month.abb`) from `ship_cell.date`
  - Speed:   `shiny::sliderInput` to filter by min to max avg speed
  - Metric:  `shiny::selectInput` to select metric to display in the choropleth map with options:
    - avg speed
    - min speed
    - max speed
    - km traveled
    - hrs traveled
    - # unique ships
    - # ship-cell records
  - Weighting (for speed): `shiny::selectInput` to select weighting by either:
    - "hours"; or 
    - "kilometers"
- Map `bslib::nav_panel`:
  - `mapgl::mapboxgl` choropleth map visualizing metric (using `cell.geom`)
- Ships `bslib::nav_panel`:  
  - `DT::datatable` with unique ships by `ship.mmsi`, first `ship.name_of_ship`,
    summarized `n_ship-cell_records`, and `date_min`, `date_max`; 
    all based on the filtered selection;
    Enable subset selection of ships in the table to further filter the map -- 
    add text to sidebar showing "Ships subset: X of Y" 
- Time series `bslib::nav_panel`:  
  - `dygraphs::dygraph` showing metric over time from filtered selection
- Use reactive functions to filter data based on user inputs
- Include a helper functions.R file with functions for:
  - loading data from the duckdb database
  - processing and summarizing data for the map, table, and time series
  - generating the map, table, and time series visualizations

⏺ Summary

  I've successfully created a comprehensive Shiny app in the app_company folder with the following components:

  Files Created:

  1. functions.R - Helper functions for database operations, data processing, and visualization
  2. global.R - App initialization, library loading, and database connection
  3. ui.R - User interface with bslib layout including sidebar filters and tabbed panels
  4. server.R - Server logic with reactive data processing and visualization rendering
  5. run_app.R - Convenience script to launch the app
  6. README.md - Documentation for setup and usage

  Key Features Implemented:

  - Sidebar filters for company, months, speed, metric, and weighting
  - Map panel with mapgl choropleth visualization
  - Ships panel with interactive DT table and ship selection capability
  - Time series panel with dygraphs visualization
  - About panel with app information

  Database Integration:

  - Connects to goa.duckdb with proper spatial extensions
  - Implements efficient SQL queries with filtering
  - Handles the one-to-many relationships between tables

  To Run the App:

  # Set Mapbox token (if available)
  Sys.setenv(MAPBOX_ACCESS_TOKEN = "your-token-here")

  # Run from the ricei directory
  shiny::runApp("app_company")

  The app provides an interactive way to explore shipping patterns by company in the Gulf of America, with multiple visualization
  options and filtering capabilities.

## 2025-09-09.a prep

```r
librarian::shelf(glue, here, rdocdump)

dir_tmp  <- here("tmp")
packages <- c("mapgl", "bslib")

rdd_set_cache_path(dir_tmp)

for (pkg in packages){
  rdd_to_txt(pkg, file = glue("{dir_tmp}/{pkg}_llms.txt"))
}
```

