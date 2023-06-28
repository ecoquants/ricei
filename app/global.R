librarian::shelf(
  bslib, dplyr, glue, leaflet, readr, sf, shiny)

whales_geo     <- here("data/whales.geojson")
whales_csv     <- here("data/whales.csv")

ply_whales <- read_sf(whales_geo) |> 
  left_join(
    read_csv(whales_csv),
    by = "hexid")

