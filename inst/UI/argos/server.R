


shinyServer(function(input, output, session) {

  output$dbtable <- renderUI({
      selectInput("table", "Table", db_structure[db == input$db ]$table ,  multiple = FALSE)
     })

  output$tagID <- renderUI({
    selectInput("tagID", "Bird ID", db_structure[db == input$db & table == input$table ]$tagID ,  multiple = FALSE)
    })


  filteredData <- reactive({
    x = try( locationTags_data(input$tagID, user = user, host = host), silent =TRUE)
    if(inherits(x,  "try-error")) x = data.table(tagID = c(0,0), latitude = c(47.9, 48), longitude = c(11.22, 11.23), datetime_ = Sys.time() )

    x[, col := colorRampPalette(brewer.pal(11,"Spectral")[11:1]  )(.N) ]

  })

  output$map <- renderLeaflet({
    leaflet()
  })

  # Incremental changes to the map should be performed in an observer with leafletProxy()

  observe({

    d = filteredData()
    midloc  = d[, .(lng = median(longitude), lat= median(latitude))]
    lastpop = paste0('Last location:', d[.N]$datetime_,'<br> ','Time recorded:', difftime(d[.N]$datetime_ ,d[1]$datetime_) %>% round,' days' )


    leafletProxy("map") %>%

    #clearBounds() %>%
    setView(midloc$lng, midloc$lat, zoom = 4) %>%

    addTiles(urlTemplate   = leafletBaseMap(input$mapID)$http) %>%

    clearMarkers()%>%
    addCircleMarkers(lng = d$longitude, lat = d$latitude , radius = 4, color = d$col, weight = 1, opacity = 0.6, fillColor = d$col, fillOpacity = 0.6, popup  = d$datetime_) %>%

    clearPopups() %>%
    addPopups( lng = d[.N,longitude], lat = d[.N,latitude],  popup = lastpop )


  })




})