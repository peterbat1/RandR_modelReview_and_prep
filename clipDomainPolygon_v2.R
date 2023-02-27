


# library(sp)
# library(rgdal)
# library(rgeos)
# 
# clipBounds <- function(boundsFile)
# {
#   d <- readOGR(boundsFile)
#   
#   clippedDomainName <- sub(".geojson","_clipped.geojson",boundsFile)
#   
#   clippedDomain <- SpatialPolygonsDataFrame(gIntersection(oz,d), data = data.frame(ID = "1"))
#   
#   writeOGR(clippedDomain, clippedDomainName, "GeoJSON", driver = "GeoJSON")
# }
# 
# oz <- readOGR("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/resources/australia_polygon.shp", "australia_polygon")
# 
# filePath <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR-webtool-maintenance/New species staging/domain"
# 
# #theFiles <- list.files(filePath,"*.geojson", full.names = TRUE)
# # theFiles <- c("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Acacia _longifolia_domain.geojson",
# #               "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Acacia_terminalisD_domain.geojson",
# #               "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Acacia_terminalisDQK3_domain.geojson")
# thisFile <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Corymbia_eximia_domain.geojson"
# #for (thisFile in theFiles)
# {
#   clipBounds(thisFile)
# }
# 



library(sf)
library(ggplot2)


ozPolygon <- sf::st_read("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/resources/australia_polygon.shp")
ozPolygon <- sf::st_transform(ozPolygon, 4326)

load("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_app3/www/models/gdm/Acacia_linifolia_genetic_model.Rd")
rawDomain <- sf::st_as_sf(md$confidence_polygon)
# rawDomain <- st_read("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Acacia_longifolia_domain.geojson")
#rawDomain <- sf::st_transform(rawDomain, 4326)
sf::st_crs(rawDomain) <- 4326

sf::st_write(rawDomain, dsn = "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_app3/www/models/domain/Acacia_linifolia_domain_new.geojson", driver = "GeoJSON")

clippedDomain <- sf::st_union(sf::st_intersection(rawDomain, ozPolygon))

sf::st_write(clippedDomain, dsn = "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_app3/www/models/domain/Acacia_linifolia_domain_new_clipped.geojson", driver = "GeoJSON")

testPlot <- ggplot() +
  geom_sf(data = ozPolygon, colour = "light grey") +
  geom_sf(data = rawDomain, colour = "blue", fill = "blue", alpha = 0.2) +
  geom_sf(data = clippedDomain, colour = "orange", fill = "orange", alpha = 0.2)

plot(testPlot)



