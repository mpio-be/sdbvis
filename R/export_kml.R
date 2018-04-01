#' @export
hexBinary <- function(col =  "#A40000") {

    # https://developers.google.com/kml/documentation/kmlreference#colorstyle
    # aabbggrr aa=alpha (00 to ff); bb=blue (00 to ff); gg=green (00 to ff); rr=red (00 to ff)

    col2rgb(col, alpha =  TRUE)  %>%
    sapply(., function(i)  sprintf("%02X",i) ) %>%
    rev %>%
    paste(collapse = '')
 
 }


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
kml.placemark.line <- function(color = '#A40000', width =3 , name = 'line1', 
    coords = cbind(c(24.71, 24.71, 24.73), c(45.59,45.61,45.60) ) ) {
    
    nam  = tag('name', name)
    
    col     = tag('color', hexBinary(color) )
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
    icon = 'http://maps.google.com/mapfiles/kml/pal2/icon18.png' , coords = c(24.71, 45.59), datetime = Sys.time() ) {
    
    nam     = tag('name', name)

    col     = tag('color', hexBinary(color) )
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


#' @export
#' @importFrom scales col_factor
#' @examples
#' d = randomTracks()
#' 
build.kml.folders <- function(d, id = ~ id, width = 3, scale =1, 
                    colfun =  col_factor("Paired", NULL)  ) {

 d[, cols := colfun(substitute(id))]

 d[, folder := kml.folder(substitute(id)[1], 
                   list(
                   kml.placemark.line(width = width), 
                   kml.placemark.points(color = cols)
                   )
                ) , by = substitute(id)]

    
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

    o = tagList( .kmlstart, .body , .kmlstop )
    print(o)

    
    cat( as.character(o), file = file)
    file    
 }


