# test_db.R - Test database connection and queries

# Load necessary libraries
library(DBI)
library(duckdb)
library(glue)

# Set paths
dir_data <- "~/My Drive/projects/ricei/data"
goa_db <- glue("{dir_data}/raw/ships/gulf_of_mexico_2023/goa.duckdb")

# Test 1: Database file exists?
message("Test 1: Checking if database file exists...")
if (file.exists(goa_db)) {
  message("✓ Database file found: ", goa_db)
  message("  File size: ", round(file.info(goa_db)$size / 1024^2, 2), " MB")
} else {
  stop("✗ Database file not found: ", goa_db)
}

# Test 2: Basic connection
message("\nTest 2: Testing basic connection...")
tryCatch({
  con <- dbConnect(duckdb(), dbdir = goa_db, read_only = TRUE)
  message("✓ Connection successful")
}, error = function(e) {
  stop("✗ Connection failed: ", e$message)
})

# Test 3: List tables
message("\nTest 3: Listing tables...")
tryCatch({
  tables <- dbListTables(con)
  message("✓ Found ", length(tables), " tables: ", paste(tables, collapse = ", "))
}, error = function(e) {
  stop("✗ Failed to list tables: ", e$message)
})

# Test 4: Spatial extension
message("\nTest 4: Loading spatial extension...")
tryCatch({
  # Try to install/load spatial
  dbExecute(con, "INSTALL spatial; LOAD spatial;")
  message("✓ Spatial extension loaded")
}, error = function(e) {
  message("✗ Failed to load spatial extension: ", e$message)
  message("  Trying alternative method...")
  tryCatch({
    duckdbfs::load_spatial(con)
    message("✓ Spatial extension loaded via duckdbfs")
  }, error = function(e2) {
    message("✗ Both methods failed: ", e2$message)
  })
})

# Test 5: Simple queries on each table
message("\nTest 5: Testing simple queries...")
for (table in tables) {
  message("\n  Testing table: ", table)
  
  # Get row count
  tryCatch({
    count_query <- glue("SELECT COUNT(*) as n FROM {table}")
    result <- dbGetQuery(con, count_query)
    message("    Row count: ", result$n)
  }, error = function(e) {
    message("    ✗ Count failed: ", e$message)
  })
  
  # Get column names
  tryCatch({
    cols <- dbListFields(con, table)
    message("    Columns: ", paste(cols, collapse = ", "))
  }, error = function(e) {
    message("    ✗ Failed to get columns: ", e$message)
  })
  
  # Get first row
  tryCatch({
    sample_query <- glue("SELECT * FROM {table} LIMIT 1")
    sample <- dbGetQuery(con, sample_query)
    message("    Sample row retrieved successfully")
  }, error = function(e) {
    message("    ✗ Sample query failed: ", e$message)
  })
}

# Test 6: Test specific queries used in the app
message("\nTest 6: Testing app-specific queries...")

# Test company query
tryCatch({
  message("\n  Testing company query...")
  companies <- dbGetQuery(con, "SELECT DISTINCT company FROM company ORDER BY company")
  message("  ✓ Found ", nrow(companies), " companies")
}, error = function(e) {
  message("  ✗ Company query failed: ", e$message)
})

# Test date range query
tryCatch({
  message("\n  Testing date range query...")
  dates <- dbGetQuery(con, "SELECT MIN(date) as min_date, MAX(date) as max_date FROM ship_cell")
  message("  ✓ Date range: ", dates$min_date, " to ", dates$max_date)
}, error = function(e) {
  message("  ✗ Date range query failed: ", e$message)
})

# Test speed range query
tryCatch({
  message("\n  Testing speed range query...")
  speeds <- dbGetQuery(con, "SELECT MIN(avg_speed_knots) as min_speed, MAX(avg_speed_knots) as max_speed FROM ship_cell WHERE avg_speed_knots IS NOT NULL")
  message("  ✓ Speed range: ", speeds$min_speed, " to ", speeds$max_speed, " knots")
}, error = function(e) {
  message("  ✗ Speed range query failed: ", e$message)
})

# Test 7: Test a join query (simplified version of main app query)
message("\nTest 7: Testing join query...")
tryCatch({
  test_query <- "
    SELECT COUNT(*) as n
    FROM cell c
    JOIN ship_cell sc ON c.cell_id = sc.cell_id
    LIMIT 1
  "
  result <- dbGetQuery(con, test_query)
  message("✓ Simple join successful")
}, error = function(e) {
  message("✗ Join query failed: ", e$message)
})

# Test 8: Test spatial query if extension loaded
message("\nTest 8: Testing spatial query...")
tryCatch({
  spatial_query <- "SELECT COUNT(*) as n FROM cell WHERE geom IS NOT NULL"
  result <- dbGetQuery(con, spatial_query)
  message("✓ Found ", result$n, " cells with geometry")
  
  # Try ST_AsText
  tryCatch({
    wkt_query <- "SELECT ST_AsText(geom) as wkt FROM cell LIMIT 1"
    result <- dbGetQuery(con, wkt_query)
    message("✓ ST_AsText works")
  }, error = function(e) {
    message("✗ ST_AsText failed: ", e$message)
  })
}, error = function(e) {
  message("✗ Spatial query failed: ", e$message)
})

# Clean up
dbDisconnect(con, shutdown = TRUE)
message("\n✓ All tests completed. Database connection closed.")