# Gulf of America Shipping by Company

A Shiny application for exploring vessel traffic data in the Gulf of America region.

## Features

- **Interactive Map**: Choropleth visualization of shipping metrics using mapgl
- **Company Filtering**: Filter by specific companies or view all/other companies
- **Time-based Analysis**: Filter by months and view temporal patterns
- **Speed Analysis**: Filter vessels by speed range
- **Multiple Metrics**: Visualize various metrics including speed, distance, time, and ship counts
- **Ship Selection**: Select specific ships to focus analysis
- **Time Series**: View temporal patterns of selected metrics

## Requirements

- R 4.0 or higher
- DuckDB database file: `goa.duckdb`
- Mapbox access token (for map display)

## Setup

1. Set your Mapbox access token:
```r
Sys.setenv(MAPBOX_ACCESS_TOKEN = "your-token-here")
```

2. Ensure the database file exists at the expected location:
`~/My Drive/projects/ricei/data/raw/ships/gulf_of_mexico_2023/goa.duckdb`

## Running the App

From the main ricei directory:
```r
shiny::runApp("app_company")
```

Or use the provided run script:
```r
source("app_company/run_app.R")
```

## Data Sources

- **Vessel presence data**: Global Fishing Watch (2023)
- **Ship information**: IHS Markit vessel database
- **Spatial resolution**: 0.1° x 0.1° grid cells

## Database Schema

The app uses four main tables:
- `cell`: Spatial grid cells (0.1° resolution)
- `ship_cell`: Vessel presence records
- `ship`: Vessel information
- `company`: Company lookup table