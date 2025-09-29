# global.R - Setup and initialization for Gulf of America shipping app

# load libraries ----
librarian::shelf(
  bsicons, bslib, DBI, DT, duckdb, duckdbfs, dplyr, dygraphs, glue, here, leaflet, 
  lubridate, # mapgl after setting mapbox token
  sf, shiny, xts, viridisLite,
  quiet = T)

# source helper functions ----
source("functions.R")

# set paths ----
is_server      <-  Sys.info()[["sysname"]] == "Linux"
dir_private    <- ifelse(is_server, "/share/private", "~/My Drive/private")
mapbox_tkn_txt <- glue("{dir_private}/mapbox_token_bdbest.txt")
dir_data       <- "~/My Drive/projects/ricei/data"
goa_db         <- glue("{dir_data}/raw/ships/gulf_of_mexico_2023/goa.duckdb")
verbosity      <- interactive()

# mapbox setup ----
Sys.setenv(MAPBOX_ACCESS_TOKEN=readLines(mapbox_tkn_txt))
librarian::shelf(mapgl)

# connect to database ----
con <- connect_to_db(goa_db)

# get initial choices for UI ----
company_choices  <- get_company_choices(con)
month_range_init <- get_month_range(con)
speed_range_init <- get_speed_range(con)

# metric options ----
metric_options <- c(
  "Average Speed"               = "avg speed",
  "Minimum Speed"               = "min speed",
  "Maximum Speed"               = "max speed",
  "Kilometers Traveled"         = "km traveled",
  "Hours Traveled"              = "hrs traveled",
  "Number of Unique Ships"      = "# unique ships",
  "Number of Ship-Cell Records" = "# ship-cell records")

# weighting options ----
weight_options <- c(
  "Hours"      = "hours",
  "Kilometers" = "kilometers")

# ply_cells ----
# get spatial cells once, so construct geom once
ply_cells <- tbl(con, "cell") |> 
  mutate(
    geom_wkt = sql("ST_AsText(geom)")) |> 
  select(cell_id, cell_ll_lon, cell_ll_lat, geom_wkt) |>
  collect() |> 
  st_as_sf(wkt = "geom_wkt", crs = 4326) |> 
  st_set_geometry("geom")

# cleanup on app stop ----
onStop(function() {
  DBI::dbDisconnect(con, shutdown = TRUE)
})