

library(raster)
library(RandR.modelReview2)

# theFiles <- c("/home/RandR/qData/eastOZ/Hakea_teretifolia_Qprops1.tif",
#               "/home/RandR/qData/eastOZ/Banksia_serrata_Qprops1.tif",
#               "/home/RandR/qData/eastOZ/Banksia_serrata_Qprops2.tif",
#               "/home/RandR/qData/eastOZ/Doryphora_sassafras_Qprops1.tif",
#               "/home/RandR/qData/eastOZ/Tristaniopsis_collina_Qprops1.tif",
#               "/home/RandR/qData/eastOZ/Tristaniopsis_laurina_Qprops1.tif")

theFiles <- "/home/RandR/qData/eastOZ/Elaeocarpus_reticulatus_Qprops1.tif"

for (thisFile in theFiles)
{
  Qras <- raster(thisFile)
  
  Qras@crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  Qras_slope <- terrain(Qras)
  plot(Qras_slope, main = "Qras_slope")
  writeRaster(Qras_slope, paste0("~/Downloads/", sub(".tif", "_slope.tif", basename(thisFile))), overwrite = TRUE)
  
  #cutVal <- median(values(Qras_slope), na.rm = TRUE)
  cutVal <- 0.5*sum(range(values(Qras_slope), na.rm = TRUE))/2
  newVals <- ifelse(values(Qras_slope) >= cutVal, 1, NA)
  
  Qras_zone <- Qras_slope
  values(Qras_zone) <- newVals
  
  plot(Qras_zone, main = "Qras_zone")
  zoneFileName <- paste0("/home/peterw/Downloads/", sub(".tif$", "_zone.tif", basename(thisFile)))
  writeRaster(Qras_zone, zoneFileName, overwrite = TRUE)
  
  sessionDir <- "/home/peterw/Downloads/"
  #RandR_gdal_polygonizeR(zoneFileName, sessionDir)
  
  
  pyPath <- Sys.which('gdal_polygonize.py')
  
  old_wd <- getwd()
  
  setwd(sessionDir)
  
  pathTo_jsonFile <- paste0(tools::file_path_sans_ext(zoneFileName), ".geojson")
  
  if (file.exists(pathTo_jsonFile)) file.remove(pathTo_jsonFile)
  
  system2('python3', args=(sprintf('"%1$s" "%2$s" -q -f "%3$s" "%4$s"',
                                  pyPath, zoneFileName, "GeoJSON", pathTo_jsonFile)))
  
  setwd(old_wd)
  
}


