setwd("E:/Dropbox (PNHP @ WPC)/2018_ShaleGas_Analysis")

coords <- read.csv("pointID_coord.csv", stringsAsFactors = FALSE)
bird <- read.csv("bird_veg_2017.csv", stringsAsFactors = FALSE)

coords <- coords[c(3,7:8)]

bird <- merge(bird,coords,by="pt_id")

write.csv(bird,"bird_veg_2017xy.csv")
