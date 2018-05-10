


shinyServer(function(input, output, session) {
  
  observe( on.exit( assign('input', reactiveValuesToList(input) , envir = .GlobalEnv)) )

  autoInvalidate <- reactiveTimer(1000*60*15) 

  activeData <- reactive({
      GMdata(tagID = input$tagID)
  })


  output$map <- renderLeaflet({
    leaflet()
  })


  observe({

    autoInvalidate()

    # DATA 
    A = activeData()
    X = A[[1]]
    LL = A[[2]]

    midloc  = X[, .(lng = median(longitude), lat= median(latitude))]

        
    # MAP
      leafletProxy("map") %>%
      setView(midloc$lng, midloc$lat, zoom = 4) %>%
      addTiles(urlTemplate   = leafletBaseMap(input$mapID)$http) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolylines(data = LL , weight = 2, color = "black", popup  = LL$pp) %>%
      addCircleMarkers(data = X , lng = X$longitude, lat = X$latitude , radius = 5, 
        color = X$col, weight = 1,  
          opacity = 0.8, fillColor = X$col, fillOpacity = 0.4, popup  = X$pp ) 





   

  })




})