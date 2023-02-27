


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
#library(ggplot2)

# Modified to prepare new webtool model review and prep dataset
#
# Peter D. Wilson
# Visiting Scientist
# Research Centre for Ecological Restoration
# Australian Institute of Botanical Science
# Royal Botanic Garden, Sydney, Australia
#
# 2022-11-15


ozPolygon <- sf::st_read("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/resources/australia_polygon.shp")
ozPolygon <- sf::st_transform(ozPolygon, 4326)

model_files <- list.files("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/Webtool_new_dataset/models/", "*.Rd", full.names = TRUE)
#this_model <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/gdm/Hakea_sericea_genetic_model.Rd"
for (this_model in model_files)
{
  load(this_model)
  rawDomain <- sf::st_as_sf(md$confidence_polygon)
  #rawDomain <- st_read("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Acacia_longifolia_domain.geojson")
  #rawDomain <- sf::st_transform(rawDomain, 4326)
  sf::st_crs(rawDomain) <- 4326
  
  sf::st_write(rawDomain,
               dsn = paste0("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/Webtool_new_dataset/domain/",
                            gsub("genetic_model.Rd", "domain.geojson", basename(this_model), fixed = TRUE)),
               driver = "GeoJSON",
               append = FALSE)
  
  clippedDomain <- sf::st_union(sf::st_intersection(rawDomain, ozPolygon))
  
  sf::st_write(clippedDomain, dsn = paste0("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/Webtool_new_dataset/domain/",
                                           gsub("genetic_model.Rd", "domain_clipped.geojson", basename(this_model), fixed = TRUE)),
               driver = "GeoJSON",
               append = FALSE)
  
  # testPlot <- ggplot() +
  #   geom_sf(data = ozPolygon, colour = "light grey") +
  #   geom_sf(data = rawDomain, colour = "blue", fill = "blue", alpha = 0.2) +
  #   geom_sf(data = clippedDomain, colour = "orange", fill = "orange", alpha = 0.2)
  # 
  # plot(testPlot)
}


