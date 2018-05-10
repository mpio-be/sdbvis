miniPage(

gadgetTitleBar("", 
    right= selectInput('mapID', NA, leafletBaseMap()$type  , leafletBaseMap('Satellite')$type),
    left = div(class="form-inline",
            strong('PTT:'), 
            selectInput('tagID', NA,  c( 'ALL', ptt$tagID) , multiple = FALSE) )

    ),
 


  miniContentPanel(padding = 0,
    leafletOutput("map", height = "100%")
  )




)