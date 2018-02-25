setwd("C:/Users/dyeany/Dropbox/2018_ShaleGas_Analysis/2018data")

coords <- read.csv("pointID_coord.csv", stringsAsFactors = FALSE)
bird <- read.csv("bird_veg_2017_test.csv", stringsAsFactors = FALSE)

coords <- coords[c(3,7:8)]

bird <- merge(bird,coords,by="pt_id")
bird <- bird[c(2:3,1,4:106)]

write.csv(bird,"bird_veg_2017_xy.csv", row.names=FALSE)
