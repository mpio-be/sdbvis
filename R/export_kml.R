#' @export
#' @rdname build.kml.folders
hexBinary <- function(col =  "#A40000") {

    # https://developers.google.com/kml/documentation/kmlreference#colorstyle
    # aabbggrr aa=alpha (00 to ff); bb=blue (00 to ff); gg=green (00 to ff); rr=red (00 to ff)

    col2rgb(col, alpha =  TRUE)  %>%
    sapply(., function(i)  sprintf("%02X",i) ) %>%
    rev %>%
    paste(collapse = '')
 
 }


#' @export
#' @rdname build.kml.folders 
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
#' @rdname build.kml.folders
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
#' @rdname build.kml.folders 
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
#' @rdname build.kml.folders 
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

    timest =  tag('TimeStamp', list(
                tag('when',  format(datetime, "%Y-%m-%dT%H:%M:%SZ") )
                ) )


    tag('Placemark', 
        tagList(
            nam,    
            Style,
            point,
            timest
           ) 
        )

 }

#'  kml building blocks
#' @description builds kml folders using kml building blocks
#' @param dat a data.table containing id, datetime_, lat, lon
#' @param  width line width
#' @param  scale symbol scale
#' @param  colfun  a function , default to scales::col_factor()
#' @note  for now dat is hardwired but this will change 
#' (dat should be a proper spacetime object or it should take id, datetime_, lat, lon as arguments)
#' @export
#' @importFrom scales col_factor
#' @examples
#' x = randomTracks() %>% build.kml.folders
#' 
build.kml.folders <- function(dat, width = 3 , scale = 1 ,  colfun =  col_factor("Paired", NULL)  ) {
 
    dat[, cols := colfun(id) ]
    dat[, k := 1:.N, by = id]

    dl = split(dat, dat$id)

    foreach(i = dl) %do% {

        POINTS = i[, .(P = 
            list(kml.placemark.points(color = cols, scale = scale, name = k, coords = c(lon, lat), datetime = datetime_) ) ), 
        by = k]$P %>% tagList
            
        LINE = kml.placemark.line(color= i$cols[1], width = width, name = paste0('track_', i$id[1]), coords = i[,.(lon, lat)] )

        kml.folder(paste0('ID_', i$id[1]), 
            list(
            POINTS,
            LINE)
            )
     }

  }


#' @title        Export tracks to kml 
#' @description  Export tracks to kml. Each ID is kept in a separate kml _<Folder>_
#' @param        A spatial points data.frame
#' @param        ... goes to build.kml.folders
#' @return       path to kml
#' @export
#' @author       MV
#' @importFrom   htmltools  tag tags tagList HTML
#' @examples     
#' \dontrun{
#' kml(dat = randomTracks() )
#'  }        
#' 
kml <- function(file = '~/Desktop/temp.kml', ... ) {
    
    .kmlstart = HTML('<?xml version="1.0" encoding="UTF-8"?>
        <kml xmlns="http://www.opengis.net/kml/2.2" xmlns:kml="http://www.opengis.net/kml/2.2">')   
    .body = kml.document( body = build.kml.folders(...) )
    .kmlstop = HTML('</kml>') 

    o = tagList( .kmlstart, .body , .kmlstop )
   
    cat( as.character(o), file = file)
    file    
 }


