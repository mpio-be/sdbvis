
#  shiny::runApp('inst/UI/argos', launch.browser = TRUE)




    invisible( sapply(c('leaflet', 'sdbvis', 'RColorBrewer'), function(x) 
    suppressPackageStartupMessages(
    require(x , character.only = TRUE, quietly = TRUE) ) ) )


    pos = round(seq(15, 255, length.out = 5))

    cols = brewer.pal.info[brewer.pal.info$category == 'qual',] 
    cols =unlist(mapply(brewer.pal, cols$maxcolors, rownames(cols))) %>% unique  %>% .[1:60]



