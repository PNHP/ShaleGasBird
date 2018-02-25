###################################################################################################
## CUSTOM CODE FOR ANALYZING PENNSYLVANIA WILDS POINT COUNT DATA
## AUTHOR: Nicole Michel, National Audubon Society
## DATE: February 2016
## 
## SOME CODE ORIGINALLY MODIFIED FROM SOLYMOS ET AL. 2013 MEE
###################################################################################################


## @@@@ USER INPUT REQUIRED HERE @@@@
# set working directory (where your scripts are stored)
setwd("E:/Dropbox (PNHP @ WPC)/2018_ShaleGas_Analysis/ShaleGasBird")

# read in source code with functions
source("WesternPA_Density_Functions.R")

## @@@@ USER INPUT REQUIRED HERE @@@@
# define the path to where your input and output data files are stored
pathtofiles <- "E:/Dropbox (PNHP @ WPC)/2018_ShaleGas_Analysis/2018data/"

library(detect)

##############################################################################################
## read in data and calculate covariates (Jdate, time since local sunrise) 
## @@@@ ONLY RUN THIS SECTION ON NEW DATASETS. COMPLETE FOR 2015!! @@@@

#get a list of what's in the directory
fileList <- dir(pathtofiles, pattern = ".csv$")
fileList

#look at the output and choose which shapefile you want to run
#enter its location in the list (first = 1, second = 2, etc)
n <- 13

# load data, QC ----
fileName <- fileList[[n]]
csvName <- strsplit(fileName,"\\.")[[1]][[1]]

rawdat <- read.csv(file=paste(pathtofiles,fileName,sep=""))

# convert date to jdate
rawdat$Jdate <- as.POSIXlt(rawdat$date_start, format="%m/%d/%Y")$yday+1

# calculate local sunrise time
library(StreamMetabolism)
rawdat$Sunrise <- NA
for (i in 1:nrow(rawdat)){
  if (!is.na(rawdat$Y[i])){
    temp <- as.POSIXlt(sunrise.set(lat=rawdat$Y[i], long=rawdat$X[i], date=format(as.Date(rawdat$date_start[i], format="%m/%d/%Y"), "%Y/%m/%d"), timezone = "UTC+4")$sunrise, format="%Y-%m-%d %h:%M:%s")
    rawdat$Sunrise[i] <- as.numeric(format(temp, "%H")) + as.numeric(format(temp, "%M"))/60
  }
}
# ignore the warnings

# fill in mean sunrise time for records lacking lat/longs
jdlist <- unique(rawdat$Jdate[which(is.na(rawdat$Y))])
for (i in 1:length(jdlist)){
  rawdat$Sunrise[which(is.na(rawdat$Y) & rawdat$Jdate==jdlist[i])] <- mean(rawdat$Sunrise[which(!is.na(rawdat$Y) & rawdat$Jdate==jdlist[i])])
}

# convert start time to decimal time
rawdat$starttime <- as.numeric(substr(rawdat$time_start, 0, nchar(rawdat$time_start)-2)) + as.numeric(substr(rawdat$time_start, nchar(rawdat$time_start)-1, nchar(rawdat$time_start)))/60

# calculate time since local sunrise
rawdat$tslr <- rawdat$starttime - rawdat$Sunrise

##############################################################################################
## clean the data, remove unidentified species, keep only the survey visit with the highest count,
##   add zeroes for all species-point combinations where not observed
## @@@@ ONLY RUN THIS SECTION ON NEW DATASETS. COMPLETE FOR 2015!! @@@@

library(plyr)

# get unique list of points and survey visits (unique_id)
UniqPtsVisits <- unique(rawdat$unique_id)

# remove records that have only outside or nosing records. Except keep point 29_17 (no birds recorded)
rawdat <- subset(rawdat, tot_sing>0 | pt_id=="29_17")

# check to see if any records still have missing distances, remove if they still exist
rawdat[which(is.na(rawdat$distance)),]
rawdat <- rawdat[which(!is.na(rawdat$distance)),]

# get species list. check for entry errors (including use of lowercase letters), unidentified species
speclist <- unique(rawdat$elem_name)

# replace any lowercase letters in elem_name field with uppercase
#rawdat$elem_name[which(rawdat$elem_name=="CSWA")] <- "CSWA"

# remove records of unidentified bird (UNBI), unidentified woodpecker (UNWO)
rawdat <- rawdat[which(!(rawdat$elem_name=="UNBI")),]
rawdat <- rawdat[which(!(rawdat$elem_name=="UNWO")),]
rawdat$elem_name <- factor(rawdat$elem_name) # reset the factor list
speclist <- unique(rawdat$elem_name) # get CSWAsed speclist

# add uniqueID field (unique by pt, spp, survey visit)
rawdat$uniqueID <- paste(rawdat$pt_id, rawdat$survey_vis, rawdat$elem_name, sep=".")

# get total count for each species * point * survey visit combination, summarized across distance bands
ptctTot <- ddply(rawdat, "uniqueID", summarize, sum(sing03 + sing35 + sing510))
colnames(ptctTot) <- c("uniqueID", "TotSing")
rawdat2 <- merge(ptctTot, rawdat, by.x=c("uniqueID"), by.y=c("uniqueID"))

# write cleaned file to csv
outFileName <- paste(csvName, "_JDate_TSLR_Cleaned.csv", sep="")
write.csv(rawdat2, file=paste(pathtofiles,outFileName, sep=""))

# scroll through by point, keep only the data from the survey visit with the highest counts
rawdat2$specpt <- paste(rawdat2$elem_name, rawdat2$pt_id, sep=".") # add field with unique combo of species and point

# copy to temp file for identifying survey visit with highest TotSing. Need to remove repeat lines for distance bands
temprawdat <- rawdat2[!(duplicated(rawdat2$uniqueID)),]

specptlist <- unique(temprawdat$specpt)

uniqueIDlist <- vector()
for (i in 1:length(specptlist)){
  temp <- subset(temprawdat, temprawdat$specpt==specptlist[i])
  maxct <- max(temp$TotSing)
  if (nrow(temp[which(temp$Totsing==max(temp$TotSing))])==1){
    uniqueIDlist[i] <- temp$uniqueID[which(temp$TotSing==max(temp$TotSing))]
  } else if (nrow(temp[which(temp$Totsing==max(temp$TotSing))])==2){ # if TotSing is the same at both periods, use the first survey visit
    uniqueIDlist[i] <- temp$uniqueID[which(temp$survey_vis==1)]
  }
}

rawdatsv <- rawdat2[which(rawdat2$uniqueID %in% uniqueIDlist),]
rawdatsv$uniqueID <- factor(rawdatsv$uniqueID)


# scroll through by species, add in 0 records for points/visits where they were not detected
zeropts <- data.frame()
speclist <- unique(rawdatsv$elem_name)
for (s in speclist){
  temp <- subset(rawdatsv, rawdatsv$elem_name==s)
  misspt <- unique(rawdatsv$pt_id[which(!(rawdatsv$pt_id %in% temp$pt_id))])
  zeropts <- rbind(zeropts, data.frame(pt_id=misspt, elem_name=rep(s, length(misspt)), TotSing=rep(0, length(misspt)), distance=rep(0, length(misspt)), sing03=rep(0, length(misspt)), sing35=rep(0, length(misspt)), sing510=rep(0, length(misspt)),specpt=paste(s,misspt,sep="."))) ## CT - added Sing510    sing510=rep(0, length(misspt)), 
}
# find which fields should be removed from rawdatsv
zeroNames <- names(zeropts)
zeroNames <- zeroNames[zeroNames!="pt_id"] # keep pt_id since the merge is done on that

# merge with rawdat to fill in other columns
zerodat <- merge(zeropts, rawdatsv[!duplicated(rawdatsv$pt_id),!(names(rawdatsv)%in%zeroNames)], by.x="pt_id", by.y="pt_id",all.x=T, all.y=F) #c(1,3:15,19,21:125)]

# combine zero data with count data
cleandat <- rbind(rawdatsv, zerodat)
cleandat$uniqueID <- NULL

# write compiled, cleaned file to csv
outFileName <- paste(csvName, "_JDate_TSLR_Cleaned_Zeroes.csv", sep="")
write.csv(cleandat, file=paste(pathtofiles,outFileName, sep=""))
