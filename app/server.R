function(input, output, session) {


  output$map <- renderLeaflet({
    
    mo <- ifelse(
      input$sld_mo == 0, 
      "avg",
      tolower(month.abb)[input$sld_mo])
    
    var <- glue("{mo}_{input$sel_var}")
    
    d <- ply_whales |> 
      select(hexid, v := var)
    
    pal <- colorNumeric("Spectral", domain = d$v, reverse = T)
    
    leaflet(d) |> 
      # options = leafletOptions(
      #   zoomControl        = zoomControl,
      #   attributionControl = attributionControl)) |>
      addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Base")) |>
      addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Reference")) |> 
      addPolygons(
        fillColor    = ~pal(v),
        weight       = 0,
        fillOpacity  = input$sld_opacity,
        smoothFactor = 0,
        label        = ~glue("{var}: {signif(v, 4)}")) |> 
      addLegend(
        pal      = pal, 
        values   = ~v, 
        opacity  = 0.7, 
        title    = var,
        position = "topright")
    
  })
  
}
