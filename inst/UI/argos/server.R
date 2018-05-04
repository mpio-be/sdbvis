


shinyServer(function(input, output, session) {

  autoInvalidate <- reactiveTimer(1000*60*15) 


  output$map <- renderLeaflet({
    leaflet()
  })


  observe({

    autoInvalidate()

    con = dbConnect(RSQLite::SQLite(), '~/argoSoap_scinam.sqlite' ) 
    d =get_argos(con)
    dbDisconnect(con)
    d = d[speed < 150 & locationDate > as.POSIXct('2018-04-26 16:00:00')]
    d[, col := factor(tagID, labels = cols[1:length(unique(tagID))])]


    midloc  = d[, .(lng = median(longitude), lat= median(latitude))]
    
    lastpop = Sys.time()


    leafletProxy("map", data = argos2SpatialLines(d) ) %>%

    setView(midloc$lng, midloc$lat, zoom = 4) %>%

    addTiles(urlTemplate   = leafletBaseMap(input$mapID)$http) %>%

    clearMarkers() %>%
    addPolylines() %>%
    addCircleMarkers(lng = d$longitude, lat = d$latitude , radius = 5, color = d$col, weight = 1,  
                    opacity = 0.8, fillColor = d$col, fillOpacity = 0.8, 
                    popup  = paste(d$tagID, paste0('class=',d$locationClass), d$locationDate, sep = '<br>') )

   

  })




})