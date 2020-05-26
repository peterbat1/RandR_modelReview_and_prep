


library(sp)
library(rgdal)
library(rgeos)

clipBounds <- function(boundsFile)
{
  d <- readOGR(boundsFile)
  
  clippedDomainName <- sub(".geojson","_clipped.geojson",boundsFile)
  
  clippedDomain <- SpatialPolygonsDataFrame(gIntersection(oz,d), data = data.frame(ID = "1"))
  
  writeOGR(clippedDomain, clippedDomainName, "GeoJSON", driver = "GeoJSON")
}

oz <- readOGR("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/resources/australia_polygon.shp", "australia_polygon")

filePath <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR-webtool-maintenance/New species staging/domain"

#theFiles <- list.files(filePath,"*.geojson", full.names = TRUE)
# theFiles <- c("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Acacia _longifolia_domain.geojson",
#               "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Acacia_terminalisD_domain.geojson",
#               "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Acacia_terminalisDQK3_domain.geojson")
thisFile <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/domain/Acacia_ulicifolia_domain.geojson"
#for (thisFile in theFiles)
{
  clipBounds(thisFile)
}
