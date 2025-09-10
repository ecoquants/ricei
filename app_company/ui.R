# ui.R - User interface for Gulf of America shipping app

page_sidebar(
  title = "Gulf of America Shipping by Company",
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  
  # sidebar ----
  sidebar = sidebar(
    width = 300,
    
    # company filter ----
    selectInput(
      "company",
      "Company:",
      choices = company_choices,
      selected = "All"
    ),
    
    # months filter ----
    sliderInput(
      "months",
      "Months:",
      min   = 1,
      max   = 12,
      value = month_range_init,
      step  = 1,
      ticks = FALSE
    ),
    tags$div(
      style = "margin-top: -15px; margin-bottom: 15px; font-size: 12px; color: #666;",
      textOutput("month_labels")
    ),
    
    # speed filter ----
    sliderInput(
      "speed",
      "Speed (knots):",
      min   = speed_range_init[1],
      max   = speed_range_init[2],
      value = speed_range_init,
      step  = 0.5
    ),
    
    # metric selection ----
    selectInput(
      "metric",
      "Metric to Display:",
      choices = metric_options,
      selected = "avg speed"
    ),
    
    # weighting selection ----
    conditionalPanel(
      condition = "input.metric.includes('speed')",
      selectInput(
        "weighting",
        "Weight Speed By:",
        choices = weight_options,
        selected = "hours"
      )
    ),
    
    # ship subset info ----
    tags$hr(),
    tags$div(
      id = "ship_subset_info",
      style = "font-weight: bold; color: #0066cc;",
      textOutput("ship_subset_text")
    ),
    
    # reset button ----
    conditionalPanel(
      condition = "output.has_ship_selection",
      actionButton(
        "reset_ships",
        "Clear Ship Selection",
        class = "btn-warning btn-sm",
        style = "margin-top: 10px; width: 100%;"
      )
    )
  ),
  
  # main content ----
  navset_card_tab(
    id = "main_tabs",
    
    # map panel ----
    nav_panel(
      "Map",
      icon = icon("map"),
      card_body(
        fill = TRUE,
        mapgl::mapboxglOutput("map", height = "600px")
      )
    ),
    
    # ships panel ----
    nav_panel(
      "Ships",
      icon = icon("ship"),
      card_body(
        p("Select ships to filter the map and time series. Use Shift+Click or Ctrl+Click for multiple selection."),
        DT::dataTableOutput("ships_table")
      )
    ),
    
    # time series panel ----
    nav_panel(
      "Time Series",
      icon = icon("chart-line"),
      card_body(
        dygraphs::dygraphOutput("time_series", height = "400px")
      )
    ),
    
    # about panel ----
    nav_panel(
      "About",
      icon = icon("info-circle"),
      card_body(
        h4("Gulf of America Shipping Data Explorer"),
        p("This application allows you to explore vessel traffic data in the Gulf of America region."),
        h5("Data Sources:"),
        tags$ul(
          tags$li("Vessel presence data: Global Fishing Watch (2023)"),
          tags$li("Ship information: IHS Markit vessel database"),
          tags$li("Spatial resolution: 0.1° x 0.1° grid cells")
        ),
        h5("Features:"),
        tags$ul(
          tags$li("Filter by company, date range, and vessel speed"),
          tags$li("View choropleth maps of various shipping metrics"),
          tags$li("Explore individual vessel information"),
          tags$li("Analyze temporal patterns in shipping activity")
        ),
        h5("Metrics:"),
        tags$ul(
          tags$li(tags$b("Speed metrics:"), " Can be weighted by hours or kilometers traveled"),
          tags$li(tags$b("Distance/Time:"), " Total kilometers or hours of vessel presence"),
          tags$li(tags$b("Ship counts:"), " Unique vessels or total observation records")
        )
      )
    )
  )
)