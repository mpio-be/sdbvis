
#' @export
kml.document <- function(title = 'kml_file', body = '#---#') {

    top =HTML( 
    '<?xml version="1.0" encoding="UTF-8"?>
    <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:kml="http://www.opengis.net/kml/2.2">')
    
    name_and_body =   tagList(tag('name', title) , body) 

    tagList(
        top, 
        tag('Document', name_and_body )
     )

 }

#' @export
kml.folder<- function(name = 'folder_1', body = '#---#') {
    tagList(
         tag('Folder', 
            list(tag('name', name), 
                tagList(body)
                ) 
            )
        )

 }

#' @export
kml.placemark.line <- function(color = '#A40000', width =3 , name = 'line1', coords = cbind(c(1,1,1), c(2.1, 2.2, 2.3)) ) {
    
    nam  = tag('name', name)
    
    col     = tag('color', color)
    W       = tag('width', width)
    lineSty = tag('LineStyle', list(col, W) ) %>% tagList
    Style   = tag('Style', lineSty)
    
    coordinates = tagList(paste(apply(coords, 1, paste, collapse = ','), collapse = '\n'), ''  )
    LineString = tag('LineString', coordinates)

    tag('Placemark', 
        tagList(
            Style,
            nam,
            LineString
           ) 
        )

 }


#' @export
kml.placemark.points <- function(color = '#4E9A06', scale = 1 , name = 'pt1', 
    icon = 'http://maps.google.com/mapfiles/kml/pal2/icon18.png' , coords = c(1,2.1), datetime = Sys.time() ) {
    
    nam     = tag('name', name)

    col     = tag('color', color)
    scale   = tag('scale', scale)
    ico     = tag('Icon',  list ( tag('href', icon)  )  )
    icoSty  = tag('IconStyle', list(col, scale, ico) ) %>% tagList
    labSty  = tag('LabelStyle', list(scale) ) %>% tagList
    
    Style   = tag('Style', list(icoSty,labSty)   )
    
    coordinates = tag('coordinates', paste(coords, collapse = ',' ) )
    point   = tag('Point', list(coordinates))

    timest = tag('TimeStamp',  format(datetime, "%Y-%m-%dT%H:%M:%SZ") )


    tag('Placemark', 
        tagList(
            nam,    
            Style,
            point
           ) 
        )

}








#' @title        Export tracks to kml 
#' @description  Export tracks to kml. Each ID is kept in a separate kml _<Folder>_
#' @param        A spatial points data.frame
#' @return       path to kml
#' @export
#' @author       MV
#' @examples     
#' \dontrun{

#'  }        
#' 
kml_save <- function() {

    kml.document( body =  
                    kml.folder('folder1', 
                       list(
                       kml.placemark.line (), 
                       kml.placemark.points()
                       )
                    )
                )


 }