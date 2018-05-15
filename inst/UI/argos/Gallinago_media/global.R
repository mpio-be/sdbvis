
#  shiny::runApp('inst/UI/argos/Gallinago_media', launch.browser = TRUE, port = 1111)


    invisible( sapply(c('sdbvis', 'leaflet', 'miniUI'), function(x) 
    suppressPackageStartupMessages(
    require(x , character.only = TRUE, quietly = TRUE) ) ) )

    ptt = dbq(user = 'mihai', host = '127.0.0.1', q = 'select PTT from GRSNatPOLAND.CAPTURES')$PTT

    brange = rgdal::readOGR( system.file('UI', 'argos', 'Gallinago_media', 'brange.kml', package = 'sdbvis'), verbose = FALSE)

    
    db = 'ARGOS'
    view = 'GRSN'


    GMdata <- function(tagID = 'ALL') {
        con = dbcon('mihai', host = '127.0.0.1') 
        X = get_argos(con, tagID = tagID, tab = paste(db,view,sep = '.' ) )
        dbDisconnect(con)

        X[Sex == 'M', col := 'red']
        X[Sex == 'F', col := 'blue']

        X = X[speed < 150]
        X[, pp := paste(tagID, paste0('class=',locationClass), paste0('lat=',latitude), paste0('lon=',longitude), locationDate, sep = '<br>') ]

        LL  = argos2SpatialLinesDataFrame(X)
        LL$pp = paste( paste(LL$tagID, 'last location'), paste0('lat=',LL$latitude), paste0('lon=',LL$longitude), LL$locationDate, sep = '<br>')

        list(X, LL)

        }