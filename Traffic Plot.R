install.packages("shiny")
require(shiny)

require(RJSONIO); require(rCharts); require(RColorBrewer); require(httr)
options(stringsAsFactors = F)

setwd('/Users/evansadler/Desktop/Traffic Accidents')

data <- read.csv("Traffic_Accidents.csv")






dat_list <- toJSONArray2(subset(data, select = c(Latitude, Longitude))[1:1000,], json = F)
plotMap <- function(network = 'citibikenyc', width = 1600, height = 800){
  L1 <- Leaflet$new()
  L1$tileLayer(provider = 'Stamen.TonerLite')
  L1$set(width = width, height = height)
  L1$setView(c(47.6097, -122.3331), 12)
  L1$geoJson(toGeoJSON(dat_list, lat = 'Latitude', lon = 'Longitude'),
             onEachFeature = '#! function(feature, layer){
      layer.bindPopup(feature.properties.label)
   } !#',
               pointToLayer =  "#! function(feature, latlng){
      return L.circleMarker(latlng, {
        radius: 4,
        fillColor: feature.properties.Color || 'red',    
        color: '#000',
        weight: 0,
        fillOpacity: 0.2
      })
   } !#"         
  )
  L1$enablePopover(TRUE)
  L1$fullScreen(TRUE)
  return(L1)
}

plotMap('citibikenyc', 600, 300)





#All of Seattle
if(FALSE) {
  
  
  #ADD OTHER DATA
  require(rgdal)
  shape <- readShapePoints("~/workspace/SHAPEFILE.shp")
  
  # Plotting
  library(ggmap)
  qmap("seattle", zoom = 14, source = "osm")
  library(ggplot2)
  
  Seattle = qmap("Seattle", zoom = 12, source = "osm")
  Seattle + geom_point(data=data, aes(x=Longitude, y=Latitude, colour = data$Zone), alpha=.2, size=.9)
  
  
  seattle = c(lat = 47.6097, lon =  122.3331)
  
  seattle.map = get_map("Seattle", zoom = 12, color = "bw")
  
  ggmap(seattle.map, extent = "panel", maprange=FALSE) + geom_density2d(data = data, aes(x = Longitude, y = Latitude)) +
    stat_density2d(data = data, aes(x = Longitude, y = Latitude,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 20, geom = 'polygon') + 
    scale_fill_gradient(low = "green", high = "red") + 
    scale_alpha(range = c(0.00, 1), guide = FALSE) +
    theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))
  
  seattle.map = get_map("Seattle", zoom = 15, color = "bw")
  
  seattlezone <- data[data$Zone %in% c('D1','D2','D3','M1','M2','M3','K1','K2','K3'),]
  ggmap(seattle.map, extent = "panel", maprange=FALSE) + geom_density2d(data = seattlezone, aes(x = Longitude, y = Latitude)) +
    stat_density2d(data = seattlezone, aes(x = Longitude, y = Latitude,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 20, geom = 'polygon') + 
    scale_fill_gradient(low = "green", high = "red") + 
    scale_alpha(range = c(0.00, 0.4), guide = FALSE) +
    theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))
}

