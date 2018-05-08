


shinyServer(function(input, output, session) {
  
  observe( on.exit( assign('input', reactiveValuesToList(input) , envir = .GlobalEnv)) )

  autoInvalidate <- reactiveTimer(1000*60*15) 




  output$map <- renderLeaflet({
    leaflet()
  })


  observe({

    autoInvalidate()

    # DATA 
    con = dbConnect(RSQLite::SQLite(), '~/argoSoap_scinam.sqlite' ) 
    X =get_argos(con)
    dbDisconnect(con)
    X = X[speed < 150 & locationDate > as.POSIXct('2018-04-26 16:00:00')]
    X = merge(X, ptt, by = 'tagID')
    X[, pp := paste(tagID, paste0('class=',locationClass), paste0('lat=',latitude), paste0('lon=',longitude), locationDate, sep = '<br>') ]

    midloc  = X[, .(lng = median(longitude), lat= median(latitude))]

    LL  = argos2SpatialLinesDataFrame(X)
    LL$pp = paste( paste(LL$tagID, 'last location'), paste0('lat=',LL$latitude), paste0('lon=',LL$longitude), LL$locationDate, sep = '<br>')


        
    # MAP
      leafletProxy("map") %>%
      setView(midloc$lng, midloc$lat, zoom = 4) %>%
      addTiles(urlTemplate   = leafletBaseMap(input$mapID)$http) %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolylines(data = LL , weight = 2, color = "black", popup  = LL$pp) %>%
      addCircleMarkers(data = X , lng = X$longitude, lat = X$latitude , radius = 5, color = X$col, weight = 1,  
          opacity = 0.8, fillColor = X$col, fillOpacity = 0.4, popup  = X$pp ) 





   

  })




})