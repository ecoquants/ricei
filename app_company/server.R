# server.R - Server logic for Gulf of America shipping app

function(input, output, session) {
  
  # reactive values ----
  selected_ships <- reactiveVal(NULL)
  
  # month labels output ----
  output$month_labels <- renderText({
    month_nums <- input$months
    if (length(month_nums) == 2) {
      paste(month.abb[month_nums[1]], "-", month.abb[month_nums[2]])
    } else {
      month.abb[month_nums]
    }
  })
  
  # filtered data ----
  filtered_data <- reactive({
    req(input$company, input$months, input$speed)
    
    withProgress(message = 'Loading data...', value = 0, {
      setProgress(0.3, detail = "Querying database...")
      
      # browser()
      
      data <- filter_ship_data(
        con            = con,
        company_filter = input$company,
        month_range    = if (setequal(input$months, month_range_init)) NULL else input$months,
        speed_range    = if (setequal(input$speed,  speed_range_init)) NULL else input$speed,
        selected_ships = selected_ships() )
      
      setProgress(1, detail = "Complete")
      data
    })
  })
  
  # cell metrics for map ----
  cell_metrics <- reactive({
    req(filtered_data(), input$metric)
    
    weight_by <- if (!is.null(input$weighting)) input$weighting else "hours"
    
    calculate_cell_metrics(
      data      = filtered_data(),
      metric    = input$metric,
      weight_by = weight_by
    )
  })
  
  # ships summary ----
  ships_summary <- reactive({
    req(filtered_data())
    get_ships_summary(filtered_data())
  })
  
  # time series data ----
  time_series_data <- reactive({
    req(filtered_data(), input$metric)
    
    weight_by <- if (!is.null(input$weighting)) input$weighting else "hours"
    
    create_time_series_data(
      data      = filtered_data(),
      metric    = input$metric,
      weight_by = weight_by
    )
  })
  
  # render map ----
  output$map <- mapgl::renderMapboxgl({
    req(cell_metrics())
    
    create_choropleth_map(
      cell_sf     = cell_metrics(),
      metric_name = names(metric_options)[metric_options == input$metric]
    )
  })
  
  # render ships table ----
  output$ships_table <- DT::renderDataTable({
    req(ships_summary())
    
    DT::datatable(
      ships_summary(),
      options = list(
        pageLength = 10,
        scrollX    = TRUE,
        order      = list(list(3, 'desc')),  # sort by n_records descending
        columnDefs = list(
          list(targets = 0, visible = FALSE)  # hide mmsi column
        )
      ),
      selection = list(
        mode   = 'multiple',
        target = 'row'
      ),
      rownames = FALSE,
      colnames = c(
        'MMSI'            = 'mmsi',
        'Ship Name'       = 'name_of_ship',
        'Operator'        = 'operator',
        'Records'         = 'n_records',
        'First Seen'      = 'date_min',
        'Last Seen'       = 'date_max'
      )
    ) |>
      DT::formatDate(columns = c('date_min', 'date_max'), method = 'toLocaleDateString')
  })
  
  # handle ship selection ----
  observe({
    selected_rows <- input$ships_table_rows_selected
    
    if (length(selected_rows) > 0) {
      ships_data <- ships_summary()
      selected_mmsi <- ships_data$mmsi[selected_rows]
      selected_ships(selected_mmsi)
    } else {
      selected_ships(NULL)
    }
  })
  
  # ship subset text ----
  output$ship_subset_text <- renderText({
    if (is.null(selected_ships())) {
      "All ships shown"
    } else {
      total_ships <- nrow(ships_summary())
      selected_count <- length(selected_ships())
      paste("Ships subset:", selected_count, "of", total_ships)
    }
  })
  
  # has ship selection (for conditional panel) ----
  output$has_ship_selection <- reactive({
    !is.null(selected_ships())
  })
  outputOptions(output, "has_ship_selection", suspendWhenHidden = FALSE)
  
  # reset ship selection ----
  observeEvent(input$reset_ships, {
    selected_ships(NULL)
    # clear table selection
    DT::dataTableProxy("ships_table") |>
      DT::selectRows(NULL)
  })
  
  # render time series ----
  output$time_series <- dygraphs::renderDygraph({
    req(time_series_data())
    
    create_time_series_plot(
      ts_data     = time_series_data(),
      metric_name = names(metric_options)[metric_options == input$metric]
    )
  })
  
  # update map when switching tabs (ensure proper rendering) ----
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "Map") {
      # trigger map resize
      session$sendCustomMessage("mapgl_resize", "map")
    }
  })
}