
library(raster)
library(RandR.webtool.dataPrep)

the_models <- list.files("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/Webtool_new_dataset/models/", "*.Rd", full.names = TRUE)
#this_model <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/gdm/Hakea_sericea_genetic_model.Rd"
for (this_model in the_models)
{
  cat(basename(this_model), "\n")
  this_taxon <- sub("_genetic_model.Rd", "", basename(this_model))
  
  setModelCovars(this_model,
                 thisTaxon = this_taxon,
                 envDataPath = "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/envData/eastOZ",
                 qDataPath = "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/qData/eastOZ",
                 numQfiles = 0, newThreshold = NULL, trace = TRUE)
  
}
