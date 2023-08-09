# inputs: raw data  ----
dir_raw    <- "/Users/bbest/My Drive/projects/ricei/data/raw" # Google Drive
ships_shp  <- glue("{dir_raw}/ships/boem_gom_grid_pl_albers/boem_gom_grid_pl_albers.shp")
ships_xls  <- glue("{dir_raw}/ships/BOEM GoMex 2020 BiOp AIS Data for Rices Whale Risk Analysis.xlsx")
whales_shp <- glue("{dir_raw}/whales/Rices_Whale_Monthly_Density.shp")
bia_shp    <- glue("{dir_raw}/whales/CetMap_BIA_WGS84/CetMap_BIA_WGS84.shp")
depth_tif  <- glue("{dir_raw}/depth/gebco_2023_n31.0_s23.0_w-98.0_e-81.0.tif")

# inputs: raw data, less used  ----
whales_roberts2016_img <- glue("{dir_raw}/whales/Duke_GOM_Brydes_whale/GOM_Brydes_whale_abundance.img")
# r_whales_roberts2016 <- rast(whales_roberts2016_img)
# wind_gdb     <- here("data/raw/boem/BOEMWindLayers_4Download.gdb")
wea_gdb    <- glue("{dir_raw}/boem/WEA_option_I_M_shapes_w_metadata")
# * [Gulf of Mexico Draft WEAs | Bureau of Ocean Energy Management](https://www.boem.gov/renewable-energy/state-activities/gulf-mexico-draft-weas)

# outputs ----
whales_geo  <- here("data/whales.geojson") # hex_id:  whale densities by hexagon
ships_geo   <- here("data/ships.geojson")  # grid_id: ship traffic by grid
cells_geo   <- here("data/cells.geojson")  # cell_id: cells for output rasters
units_gpkg  <- here("data/units.gpkg")     # unit_id: unit intersections for analysis
study_gpkg  <- here("data/study.gpkg")
ships_csv   <- here("data/ships.csv")
whales_csv  <- here("data/whales.csv")
rast_tif    <- here("data/rasters.tif")
depth_contours_geo <- here("data/depth_contours.geojson")
whales01_geo <- here("data/whales_gteq0.01per100km2.geojson")
wab_geo      <- here("data/whale_area_bioop.geojson")
wan_geo      <- here("data/whale_area_new.geojson")
bia_geo      <- here("data/whale_area_bia.geojson")

whales_n_by_area_csv <- here("data/tbl_whales_n_by_area.csv")
ships_overview_csv <- here("data/tbl_ships_overview.csv")
risk_overview_csv <- here("data/tbl_risk_overview.csv")
risk_reduction_by_areas_csv <- here("data/risk_reduction_by_areas.csv")

# constants ----
n_whales_Garrison2020 <- 51.3
