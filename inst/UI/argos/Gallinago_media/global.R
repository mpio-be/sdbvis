
#  shiny::runApp('inst/UI/argos/Gallinago_media', launch.browser = TRUE)


    invisible( sapply(c('leaflet', 'sdbvis', 'miniUI'), function(x) 
    suppressPackageStartupMessages(
    require(x , character.only = TRUE, quietly = TRUE) ) ) )


    ptt = fread(system.file('UI', 'argos', 'Gallinago_media', 'ptts.txt', package = 'sdbvis'))
    ptt[Sex == 'M', col := 'red']
    ptt[Sex == 'F', col := 'blue']


    GMdata <- function(file = '~/argoSoap_scinam.sqlite', tagID) {
        con = dbConnect(RSQLite::SQLite(), file) 
        X =get_argos(con, tagID = tagID)
        dbDisconnect(con)


        X = X[speed < 150 & locationDate > as.POSIXct('2018-04-26 16:00:00')]
        X = merge(X, ptt, by = 'tagID')
        X[, pp := paste(tagID, paste0('class=',locationClass), paste0('lat=',latitude), paste0('lon=',longitude), locationDate, sep = '<br>') ]

        LL  = argos2SpatialLinesDataFrame(X)
        LL$pp = paste( paste(LL$tagID, 'last location'), paste0('lat=',LL$latitude), paste0('lon=',LL$longitude), LL$locationDate, sep = '<br>')

        list(X, LL)

        }