#' Data contains last location
#' @export
argos2SpatialLinesDataFrame<- function(x)  {

  o = x[, .(spLines = list (list( sp::Line(cbind(longitude, latitude)) ) )) , by = tagID]
  o[, spLines := list ( list( sp::Lines( spLines[[1]], ID = tagID) ) ) , by = tagID]
  o[, spLines := list ( list( sp::SpatialLines( spLines[1] ) ) ) , by = tagID]


  x[, maxd := max(locationDate) , by = tagID]
  dat = x[maxd == locationDate][, maxd := NULL]

  o = merge(o, dat , by = 'tagID')

  o[, spLines :=  list( list(sp::SpatialLinesDataFrame(spLines[[1]], 
    data = data.frame( tagID, latitude, longitude, locationDate, row.names = tagID)) ) ) , by = tagID ]

  do.call(rbind, o$spLines)



  }


#' @title        argoSpeed  
#' @description  argos speed along a track
#' @param        x a data.table containing tagID, locationDate,latitude, longitude.
#' @return       an updated data.table with speed as a new column.
#' @export
#' @importFrom geosphere distGeo 
#' @author       MV
#' @examples \dontrun{
#' 
#' argoSpeed(x)
#' } 

argoSpeed <- function(x) {
    setorder(x, tagID, locationDate)
    x[, ':=' (latitude2 = shift(latitude), longitude2 = shift(longitude), locationDate2 = shift(locationDate) ), by = tagID ]
    x[, dist := geosphere::distGeo(cbind(longitude, latitude), cbind(longitude2, latitude2)  )/1000, by = 1:nrow(x)]
    
    x[, timeDiff := difftime(locationDate, locationDate2, units = 'hour')%>%as.numeric, by = 1:nrow(x)]
    x[, speed := dist/timeDiff]
    x[, ':=' (latitude2 = NULL, longitude2 = NULL, locationDate2 = NULL )]
    x
 }





#' @title        get_argos 
#' @description  loads argos data. 
#' @param        con db connection 
#' @param        tab table name containing the data. should have tagID, locationDate,latitude, longitude
#' @return       data.table
#' @export
#' @importFrom anytime anytime 
#' @author       MV
#' @examples     
#' \dontrun{
#' 
#' con = dbConnect(RSQLite::SQLite(), '~/argoSoap_scinam.sqlite' ) 
#' x =get_argos(con)
#'  }        
#' 
get_argos <- function(con, tab = 'argos') {
    x = dbGetQuery(con, paste('select * from ', tab)) %>% data.table
    x[, locationDate := anytime(locationDate)]
    argoSpeed(x)
    x
}