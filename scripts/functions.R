# packages ----
librarian::shelf(
  classInt, dplyr, fs, geojsonsf, ggplot2, ggspatial, glue, gt, here, janitor, knitr, leafem, 
  leaflet, leaflet.extras, lwgeom, mapview, mregions, noaa-onms/onmsR, purrr, 
  rnaturalearth,
  readr, readxl, scales, sf, stars, stringr, terra, tibble, tidyr, units,
  quiet = T)
# select = dplyr::select
options(readr.show_col_types = F)

# functions ----
drop_lyr <- function(lyr){
  
  if (lyr %in% names(stk_rast)){
    tmp_tif <- tempfile(fileext = ".tif")
    rast(rast_tif) |>
      subset(lyr, negate=T) |>
      writeRaster(tmp_tif)
    file_delete(rast_tif)
    file_move(tmp_tif, rast_tif)
  }
}

add_lyr <- function(r, lyr){
  # r <- add_lyr(
  # r_whales * r_ships_avg_boem_gt01,
  # "risk_avg_boem_gt01")
  # r <- r_whales * r_ships_avg_boem_gt01
  # lyr <- "risk_avg_boem_gt01"
  
  names(r) <- lyr
  
  drop_lyr(lyr)
  writeRaster(r, rast_tif, gdal="APPEND_SUBDATASET=YES")
  
  stk_rast <- rast(rast_tif)
  stopifnot(lyr %in% names(stk_rast))
  
  stk_rast[lyr]
}

calc_lyr <- function(
    lyr,
    filter_expr,
    redo = F){
  
  if (lyr %in% names(stk_rast) & !redo)
    return(stk_rast[lyr])
  
  lyr_pfx <- str_split_1(lyr, "_")[1]
  stopifnot(lyr_pfx %in% c("ships","whales"))
  
  if (lyr_pfx == "ships"){
    d <- tbl_units |> 
      left_join(
        tbl_ships |> 
          filter({{ filter_expr }}) |> 
          # filter(
          #   timespan == "year" & 
          #     year   == !!yr &
          #     metric == !!m) |> 
          select(grid_id, value),
        by = "grid_id")
  } else if(lyr_pfx == "whales"){
    d <- tbl_units |> 
      left_join(
        tbl_whales |> 
          filter(
            timespan == "avg",
            metric   == "n_per_km2") |> 
          mutate(
            value = value * 100) |> # convert n_per_km2 to n_per_100km2
          select(hex_id, value),
        by = "hex_id")
  }
  
  ply <- ply_cells |>
    inner_join(
      d |>
        group_by(cell_id) |>
        summarize(
          value = weighted.mean(value, area_km2),
          .groups = "drop"),
      by = "cell_id")
  
  # debug...
  # mapView(ply, zcol="value") + 
  #   mapView(ply_ships_v, zcol="value")
  # ply |> st_drop_geometry() |> pull(value) |> sum(na.rm=T) |> format(big.mark=",") # 35,457,972
  # mapView(ply_ships, zcol="area_km2")
  # ply_ships_v <- ply_ships |> 
  #   left_join(
  #     tbl_ships |> 
  #       filter(
  #         timespan == "year",
  #         year == 2018,
  #         metric == "all_gt01"), # |>            # 24,582,837: Table 46
  #       # summarize(value = sum(value, na.rm=T)) # 23,555,127
  #       by = "grid_id")
  # mapView(ply_ships, zcol="area_km2")
  # mapView(ply_ships_v, zcol="value") +
  #   mapView(stk_rast["ships_2018_all_gt01"] |> raster::raster())
  
  r <- ply |> 
    st_transform(3857) |> 
    rasterize(
      r_cells, 
      field = "value")
  names(r) <- lyr
  # global(r, "sum", na.rm=T) |> pull(sum) |> format(big.mark=",") # 33,703,993
  
  add_lyr(r, lyr)
}

map_ply_val <- function(
    ply, 
    label, legend,
    colors = "Spectral",
    zoomControl        = knitr::is_html_output(),
    attributionControl = knitr::is_html_output(),
    fullScreenControl  = knitr::is_html_output(),
    fillOpacity        = 1){
  pal <- colorNumeric(colors, domain = ply$val, reverse = T)
  
  m <- leaflet(
    ply,
    options = leafletOptions(
      zoomControl        = zoomControl,
      attributionControl = attributionControl)) |>
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base")) |>
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference")) |> 
    addPolygons(
      fillColor    = ~pal(val),
      weight       = 0,
      
      fillOpacity  = fillOpacity,
      smoothFactor = 0,
      label        = ~glue("{label}: {signif(val, 4)}")) |> 
    addLegend(
      pal      = pal, 
      values   = ~val, 
      opacity  = 0.7, 
      title    = legend,
      position = "topright")
  
  if (fullScreenControl)
    m <- m |>
    leaflet.extras::addFullscreenControl()
  m
}

map_rast <- function(
    r, 
    legend_title, 
    colors             = "Spectral",
    group              = "raster",
    fillOpacity        = 1,
    add_depth_contours = T,
    add_ply_bia        = F,
    add_ply_wab        = F,
    add_ply_wan        = F,
    add_zoomControl        = knitr::is_html_output(),
    add_attributionControl = knitr::is_html_output(),
    add_fullScreenControl  = knitr::is_html_output(),
    add_layersControl      = knitr::is_html_output(),
    add_mouseCoordinates   = knitr::is_html_output()){
  # legend             = "whales<br><small>(# / 100 km<sup>2</sup>)</small>"
  # colors             = "Spectral"
  # zoomControl        = T
  # attributionControl = T
  # fullScreenControl  = T
  # fillOpacity        = 1
  # add_depth_contours = T

  
  pal <- colorNumeric(colors, domain = values(r), reverse = T, na.color = "transparent")
  
  n_arrow_img <- "https://cdn.pixabay.com/photo/2013/07/12/17/54/arrow-152596_960_720.png"
  
  m <- leaflet(
    options = leafletOptions(
      zoomControl        = add_zoomControl,
      attributionControl = add_attributionControl)) |>
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base")) |>
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference")) |> 
    addRasterImage(
      r, project = F,
      colors = pal,
      group  = group) |> 
    addScaleBar(
      position = c("bottomleft")) |> 
    leafem::addLogo(n_arrow_img) |> 
    addLegend(
      pal      = pal, 
      values   = values(r), 
      opacity  = 0.7, 
      title    = legend_title,
      position = "topright")
  
  lyrs <- c(group)
  
  if (add_depth_contours){
    grps  <- c("100 m contour", "400 m contour")
    cols  <- c("black", "black")
    wts   <- c(1, 2)
    dashs <- c("5", "10")
    
    m <- m |> 
      addPolylines(
        data = lns_depth_contours |> 
          filter(depth_m == "100"),
        group = grps[1],
        color = cols[1], opacity = 1.0,
        weight = wts[1], dashArray = dashs[1]) |> 
      addPolylines(
        data = lns_depth_contours |> 
          filter(depth_m == "400"),
        group = grps[2],
        color = cols[2], opacity = 1.0,
        weight = wts[2], dashArray = dashs[2])
    
    lyrs <- c(lyrs, grps)
  }
  
  if (add_ply_bia){ # whale area - Biologically Important Area (LaBrecque, 2016)
    grp <- "Bio. Imp. Area (LaBrecque, 2016)"

    m <- m |> 
      addPolygons(
        data = ply_bia, 
        color="pink", fill = F, 
        opacity = 1.0, weight=2,
        group = grp)
    
    lyrs <- c(lyrs, grp)
  }
  
  if (add_ply_wab){ # whale area - Biogical Opinion (NOAA, 2020)
    grp <- "Whale Area (NOAA, 2020)"

    m <- m |> 
      addPolygons(
        data = ply_wab, 
        color="purple", fill = F, 
        opacity = 1.0, weight=2,
        group = grp)
    
    lyrs <- c(lyrs, grp)
  }
  
  if (add_ply_wan){ # whale area - New
    grp <- "Whale Area, New"

    m <- m |> 
      addPolygons(
        data = ply_wan, 
        color="red", fill = F, 
        opacity = 1.0, weight=2,
        group = grp)
    
    lyrs <- c(lyrs, grp)
  }
  
  if (add_layersControl)
    m <- m |> 
      addLayersControl(
        overlayGroups = lyrs,
        options = layersControlOptions(collapsed = T))
  
  if (add_mouseCoordinates)
    m <- addMouseCoordinates(m)
  
  if (add_fullScreenControl)
    m <- addFullscreenControl(m)
  
  m
}

map_rast_jenks <- function(
    r, 
    legend_title, 
    colors             = "Spectral",
    group              = "raster",
    n_breaks           = 7,
    fillOpacity        = 1,
    add_depth_contours = T,
    add_ply_bia        = F,
    add_ply_wab        = F,
    add_ply_wan        = F,
    add_zoomControl        = knitr::is_html_output(),
    add_attributionControl = knitr::is_html_output(),
    add_fullScreenControl  = knitr::is_html_output(),
    add_layersControl      = knitr::is_html_output(),
    add_mouseCoordinates   = knitr::is_html_output()){
  # legend             = "whales<br><small>(# / 100 km<sup>2</sup>)</small>"
  # colors             = "Spectral"
  # zoomControl        = T
  # attributionControl = T
  # fullScreenControl  = T
  # fillOpacity        = 1
  # add_depth_contours = T
  
  # browser()
  v <- values(r, na.rm=T)[,1]
  brks <- classInt::classIntervals(v, style = "fisher", n=n_breaks)$brks
  stopifnot(min(brks) <= min(v) & max(brks) >= max(v))
  
  pal <- colorBin(colors, bins = brks, pretty = T, reverse = T, na.color = "transparent")
  
  n_arrow_img <- "https://cdn.pixabay.com/photo/2013/07/12/17/54/arrow-152596_960_720.png"
  
  m <- leaflet(
    options = leafletOptions(
      zoomControl        = add_zoomControl,
      attributionControl = add_attributionControl)) |>
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Base")) |>
    addProviderTiles(
      "Esri.OceanBasemap",
      options = providerTileOptions(
        variant = "Ocean/World_Ocean_Reference")) |> 
    addRasterImage(
      r, project = F,
      colors = pal,
      group  = group) |> 
    addScaleBar(
      position = c("bottomleft")) |> 
    leafem::addLogo(n_arrow_img) |> 
    addLegend(
      pal      = pal, 
      values   = v, 
      opacity  = 0.7, 
      title    = legend_title,
      labFormat = labelFormat(
        # digits=1, between = ' <br> &nbsp;&nbsp;&ndash; '),
        digits=1, between = ' <br> &nbsp;&nbsp; to '),
      position = "topright")
  
  lyrs <- c(group)
  
  if (add_depth_contours){
    grps  <- c("100 m contour", "400 m contour")
    cols  <- c("black", "black")
    wts   <- c(1, 2)
    dashs <- c("5", "10")
    
    m <- m |> 
      addPolylines(
        data = lns_depth_contours |> 
          filter(depth_m == "100"),
        group = grps[1],
        color = cols[1], opacity = 1.0,
        weight = wts[1], dashArray = dashs[1]) |> 
      addPolylines(
        data = lns_depth_contours |> 
          filter(depth_m == "400"),
        group = grps[2],
        color = cols[2], opacity = 1.0,
        weight = wts[2], dashArray = dashs[2])
    
    lyrs <- c(lyrs, grps)
  }
  
  if (add_ply_bia){ # whale area - Biologically Important Area (LaBrecque, 2016)
    grp <- "Bio. Imp. Area (LaBrecque, 2016)"
    
    m <- m |> 
      addPolygons(
        data = ply_bia, 
        color="pink", fill = F, 
        opacity = 1.0, weight=2,
        group = grp)
    
    lyrs <- c(lyrs, grp)
  }
  
  if (add_ply_wab){ # whale area - Biogical Opinion (NOAA, 2020)
    grp <- "Whale Area (NOAA, 2020)"
    
    m <- m |> 
      addPolygons(
        data = ply_wab, 
        color="purple", fill = F, 
        opacity = 1.0, weight=2,
        group = grp)
    
    lyrs <- c(lyrs, grp)
  }
  
  if (add_ply_wan){ # whale area - New
    grp <- "Whale Area, New"
    
    m <- m |> 
      addPolygons(
        data = ply_wan, 
        color="red", fill = F, 
        opacity = 1.0, weight=2,
        group = grp)
    
    lyrs <- c(lyrs, grp)
  }
  
  if (add_layersControl)
    m <- m |> 
    addLayersControl(
      overlayGroups = lyrs,
      options = layersControlOptions(collapsed = T))
  
  if (add_mouseCoordinates)
    m <- addMouseCoordinates(m)
  
  if (add_fullScreenControl)
    m <- addFullscreenControl(m)
  
  m
}