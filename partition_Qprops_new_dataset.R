

zone_folder <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/Webtool_new_dataset/zones/"

library(raster)

theFiles <- list.files("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/Webtool_new_dataset/qData/eastOZ/", "*.tif", full.names = TRUE)

for (thisFile in theFiles)
{
  cat(basename(thisFile), ":\n")
  
  cat("  Loading raster...")
  Qras <- raster::raster(thisFile)
  
  Qras@crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  cat("done\n  Computing & saving slope raster..")
  Qras_slope <- raster::terrain(Qras)
  #plot(Qras_slope, main = "Qras_slope")
  raster::writeRaster(Qras_slope, paste0(zone_folder, sub(".tif", "_slope.tif", basename(thisFile))), overwrite = TRUE)
  cat("done\n  Recoding slope raster to create representative layer & saving it...")
  #cutVal <- median(values(Qras_slope), na.rm = TRUE)
  cutVal <- 0.5*sum(range(raster::values(Qras_slope), na.rm = TRUE))/2
  newVals <- ifelse(raster::values(Qras_slope) >= cutVal, 1, NA)
  
  Qras_zone <- Qras_slope
  raster::values(Qras_zone) <- newVals
  
  #plot(Qras_zone, main = "Qras_zone")
  zoneFileName <- paste0(zone_folder, sub(".tif$", "_zone.tif", basename(thisFile)))
  raster::writeRaster(Qras_zone, zoneFileName, overwrite = TRUE)
  cat("done\n Polygonising raster...")
  sessionDir <- tmpDir()
  
  pyPath <- Sys.which('gdal_polygonize.py')
  
  old_wd <- getwd()
  
  setwd(sessionDir)
  
  pathTo_jsonFile <- paste0(tools::file_path_sans_ext(zoneFileName), ".geojson")
  
  if (file.exists(pathTo_jsonFile)) file.remove(pathTo_jsonFile)
  
  system2('python3', args=(sprintf('"%1$s" "%2$s" -q -f "%3$s" "%4$s"',
                                  pyPath, zoneFileName, "GeoJSON", pathTo_jsonFile)))
  
  setwd(old_wd)
  cat("done\n")
}

cat("*** End of processing\n")
