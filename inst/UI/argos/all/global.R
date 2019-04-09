
#  shiny::runApp('inst/UI/argos/all', launch.browser = TRUE, port = 1111)


invisible( sapply(c('sdbvis', 'shiny', 'leaflet', 'leafgl', 'miniUI', 'magrittr', 'mapview'), function(x)
suppressPackageStartupMessages(
require(x , character.only = TRUE, quietly = TRUE) ) ) )


argosdata <- function() {

	con = dbcon('mihai')
	on.exit(dbDisconnect(con))    

	dbq(con, 'select * from ARGOS.allARGOS', enhance = TRUE)[!is.na(latitude)]

}



m = leaflet() %>%
	addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
	addGlPoints(data = sf::st_as_sf(argosdata(), coords = c("longitude", "latitude"), crs = 4326)  , group = "pts") %>%
	addMouseCoordinates() %>%
	setView(lng = 10.5, lat = 49.5, zoom = 6) %>%
	addLayersControl(overlayGroups = "pts")


ui <- fluidPage(
    leafglOutput("mymap")
)

server <- function(input, output, session) {
    output$mymap <- renderLeaflet(m)
}

shinyApp(ui, server)