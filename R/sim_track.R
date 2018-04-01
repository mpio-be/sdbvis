
#' @export
#' @importFrom scales rescale 

randomTracks <- function(n=10, N = 3 , lon = -156.6740, lat =71.321, R = 1) {

    d = data.table(id = rep(1:N, each = n))
    d[, datetime_ := seq.POSIXt(Sys.time(), by = 3600, length.out = n ), by = id]
    d[, lat := diffinv(rnorm(n) )[-1] %>% rescale( c( lat,lat + R) ), by = id]
    d[, lon := diffinv(rnorm(n) )[-1] %>% rescale( c( lon,lon + R) ), by = id]
  
    d
  
}

