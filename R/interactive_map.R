

#' @export
leafletBaseMap <-  function(type) {

      x = data.frame(
      type = c(
      'Topographic',
      'Satellite',
      'Nat. Geo.' ,
      'Open Street',
      'Terrain',
      'Watercolor'
      ),
      http = c(
      'http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}',
      'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
      'http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}',
      'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
      'http://a{s}.acetate.geoiq.com/tiles/terrain/{z}/{x}/{y}.png',
      'http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png'

      ),

      stringsAsFactors  = FALSE)


      if(missing(type))
      return(x) else
      return(x[x$type == type, ])


}