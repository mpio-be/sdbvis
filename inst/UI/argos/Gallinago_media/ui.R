miniPage(

gadgetTitleBar("GS", 
    right= selectInput('mapID', NA, leafletBaseMap()$type  , leafletBaseMap('Satellite')$type),
    left = div()),
# 
# fluidRow(
#          checkboxInput('selectID', 'select PTT', value = FALSE), 
#           conditionalPanel("input.selectID == 1", 
#             selectInput('tagID', NA,  ptt$tagID , multiple = FALSE)  )
#          )
# 


  miniContentPanel(padding = 0,
    leafletOutput("map", height = "100%")
  )




)