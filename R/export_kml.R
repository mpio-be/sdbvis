
#' @export
kml.document <- function(title = 'kml_file', body = '#---#') {

    tagList(
        tag('Document', 
            tagList(
                tag('name', title) , 
                body) 
         )
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
    coordinates = tag('coordinates', coordinates)
    LineString = tag('LineString', list(coordinates) )

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
#' @importFrom   htmltools  tag tags tagList HTML
#' @examples     
#' \dontrun{
 #   kml()
#'  }        
#' 
kml <- function(file = '~/Desktop/temp.kml' ) {
    return(file)
    
    .kmlstart = HTML('<?xml version="1.0" encoding="UTF-8"?>
    <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:kml="http://www.opengis.net/kml/2.2">')   

    .body = kml.document( body =  
                kml.folder('folder1', 
                   list(
                   kml.placemark.line (), 
                   kml.placemark.points()
                   )
                )
            )
    .kmlstop = HTML('</kml>') 

    o = tagList(.kmlstart, .body , .kmlstop )



    cat( as.character(o), file = file)

 }