
#  shiny::runApp('inst/UI/argos/Gallinago_media', launch.browser = TRUE)


    invisible( sapply(c('leaflet', 'sdbvis', 'miniUI'), function(x) 
    suppressPackageStartupMessages(
    require(x , character.only = TRUE, quietly = TRUE) ) ) )


    ptt = fread(system.file('UI', 'argos', 'Gallinago_media', 'ptts.txt', package = 'sdbvis'))
    ptt[Sex == 'M', col := 'red']
    ptt[Sex == 'F', col := 'blue']