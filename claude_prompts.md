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

