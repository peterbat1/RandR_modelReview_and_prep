
model_files <- list.files("/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/models/gdm/", "*.Rd")

taxa <- unlist(lapply(strsplit(model_files, "_genetic", fixed = TRUE), function(el) {gsub("_", " ", el[1]) }))

ans <- data.frame(taxon = taxa, onWebtool = rep("Yes", length(taxa)), stringsAsFactors = FALSE)

write.csv(ans, "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/resources/onWebtool_lookup.csv", row.names = FALSE)
