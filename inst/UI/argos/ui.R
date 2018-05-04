shinyUI(

bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),

  leafletOutput("map", width = "100%", height = "100%"),

  absolutePanel(top = pos[1] ,  right = 10,
    selectInput('mapID', HTML('<p style="color:red">Base map.</p>') ,leafletBaseMap()$type  , leafletBaseMap('Satellite')$type ) ),

  absolutePanel(top = pos[2] ,  right = 10, uiOutput("tagID") )

)

)