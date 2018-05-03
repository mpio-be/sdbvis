shinyUI(

bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),

  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(top = pos[1] ,  right = 10,
    selectInput('mapID', 'Mapy type',leafletBaseMap()$type  , leafletBaseMap('Satellite')$type ) ),

  absolutePanel(top = pos[2] ,  right = 10,
    selectInput("db", "Database", db_structure[,unique(db)] ,  multiple = FALSE) ),

  absolutePanel(top = pos[3] ,  right = 10,
    uiOutput("dbtable") ),

  absolutePanel(top = pos[4] ,  right = 10,
    uiOutput("tagID") )

)

)