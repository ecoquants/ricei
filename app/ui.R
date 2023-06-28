page_sidebar(
  title = "Ricei Risk",
  sidebar = sidebar(
    title = "Selection",
    selectInput(
      "sel_tbl", "Data",
      c("ships","whales"), "whales"),
    # conditionalPanel(
    #   condition = "input.sel_tbl == 'whales'",
    selectInput(
      "sel_var", "Variable",
      c("n","se")),
    sliderInput(
      "sld_opacity", "Opacity",
      0,1,1, step=0.2) ), # skip: "cv"
  card(
    card_header("Map"),
    leafletOutput("map"),
    sliderInput(
      "sld_mo", "Month",
      min = 0, max = 12, value = 0, step = 1, round = T, ticks = T,
      animate = animationOptions(
        interval = 3000,
        loop = T), 
      width = "100%"),
    shiny::helpText("Choose 0 for annual average"))
)
