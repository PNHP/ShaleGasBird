###################################################################################################
## CUSTOM FUNCTIONS FOR ANALYZING PENNSYLVANIA WILDS POINT COUNT DATA
## AUTHOR: Nicole Michel, National Audubon Society
## DATE: February 2016
## 
## THIS CODE CONTAINS FUNCTIONS CALLED UP BY WesternPA_EstimateDensity.R
## SOME CODE ORIGINALLY MODIFIED FROM SOLYMOS ET AL. 2013 MEE
###################################################################################################

cat("WPA density functions loaded, version 20160220\n")


###########################################################
## DUMMY CODE CATEGORICAL DISTANCE VARIABLES

DummyCodeXDist <- function(ptctDist.spp,...)
{
# convert categorical covariates to dummy style coding
ftdummy <- cbind(ptctDist.spp$for_typ_code[which(ptctDist.spp$distance==0.5)],ptctDist.spp$for_typ_code[which(ptctDist.spp$distance==0.5)],ptctDist.spp$for_typ_code[which(ptctDist.spp$distance==0.5)],ptctDist.spp$for_typ_code[which(ptctDist.spp$distance==0.5)],ptctDist.spp$for_typ_code[which(ptctDist.spp$distance==0.5)],ptctDist.spp$for_typ_code[which(ptctDist.spp$distance==0.5)])
ftdummy[which(!ftdummy[,1]==2),1] <- 0
ftdummy[which(!ftdummy[,2]==3),2] <- 0
ftdummy[which(!ftdummy[,3]==4),3] <- 0
ftdummy[which(!ftdummy[,4]==5),4] <- 0
ftdummy[which(!ftdummy[,5]==6),5] <- 0
ftdummy[which(!ftdummy[,6]==7),6] <- 0
ftdummy[which(ftdummy[,1]>0),1] <- 1
ftdummy[which(ftdummy[,2]>0),2] <- 1
ftdummy[which(ftdummy[,3]>0),3] <- 1
ftdummy[which(ftdummy[,4]>0),4] <- 1
ftdummy[which(ftdummy[,5]>0),5] <- 1
ftdummy[which(ftdummy[,6]>0),6] <- 1

pagdummy <- cbind(ptctDist.spp$for_group_code[which(ptctDist.spp$distance==0.5)],ptctDist.spp$for_group_code[which(ptctDist.spp$distance==0.5)])
pagdummy[which(!pagdummy[,1]==2),1] <- 0
pagdummy[which(!pagdummy[,2]==3),2] <- 0
pagdummy[which(pagdummy[,1]>0),1] <- 1
pagdummy[which(pagdummy[,2]>0),2] <- 1

surdummy <- cbind(ptctDist.spp$surveyors[which(ptctDist.spp$distance==0.5)],ptctDist.spp$surveyors[which(ptctDist.spp$distance==0.5)])
surdummy[which(!surdummy[,1]==2),1] <- 0
surdummy[which(!surdummy[,2]==3),2] <- 0
surdummy[which(surdummy[,1]>0),1] <- 1
surdummy[which(surdummy[,2]>0),2] <- 1
out <- as.matrix(cbind(ftdummy, pagdummy, surdummy, ptctDist.spp$wind[which(ptctDist.spp$distance==0.5)]))
out
}



#################################################################
## RUN ALL REMOVAL MODELS
WPARunRemovalModels <- function(remdat,...)
{    
  Y.rem <- remdat$Y
  D.rem <- remdat$D
  X.remall <- remdat$X
  # build all possible combinations of models. Will build AIC table after
  rem.null <<- try(cmulti(Y.rem | D.rem ~ 1, type="rem"), silent=T)                      # constant singing rate (no covars)
  X.rem <-X.remall[,1]
  rem.jd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate
  X.rem <-X.remall[,c(1,2)]
  rem.jd2 <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + Jdate^2
  X.rem <-X.remall[,3]
  rem.ls <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr
  X.rem <-X.remall[,c(3,4)]
  rem.ls2 <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # tslr + tslr^2
  X.rem <-X.remall[,5]
  rem.sky <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)        # sky
  X.rem <-X.remall[,6]
  rem.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # wind
  X.rem <-X.remall[,7]
  rem.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # temp
  X.rem <-X.remall[,c(1,3)]
  rem.jd.ls <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + tslr
  X.rem <-X.remall[,c(1,5)]
  rem.jd.sky <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + sky
  X.rem <-X.remall[,c(1,6)]
  rem.jd.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + wind
  X.rem <-X.remall[,c(1,7)]
  rem.jd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + temp
  X.rem <-X.remall[,c(3,5)]
  rem.ls.sky <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr + sky
  X.rem <-X.remall[,c(3,6)]
  rem.ls.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr + wind
  X.rem <-X.remall[,c(3,7)]
  rem.ls.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr + temp
  X.rem <-X.remall[,c(5,6)]
  rem.sky.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # sky + wind
  X.rem <-X.remall[,c(5,7)]
  rem.sky.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # sky + temp
  X.rem <-X.remall[,c(6,7)]
  rem.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # wind + temp
  X.rem <-X.remall[,c(1,2,3)]
  rem.jd2.ls <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + Jdate^2 + tslr
  X.rem <-X.remall[,c(1,2,5)]
  rem.jd2.sky <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + Jdate^2 + sky
  X.rem <-X.remall[,c(1,2,6)]
  rem.jd2.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + Jdate^2 + wind
  X.rem <-X.remall[,c(1,2,7)]
  rem.jd2.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + Jdate^2 + temp
  X.rem <-X.remall[,c(1,3,5)]
  rem.jd.ls.sky <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + tslr + sky
  X.rem <-X.remall[,c(1,3,6)]
  rem.jd.ls.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + tslr + wind
  X.rem <-X.remall[,c(1,3,7)]
  rem.jd.ls.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + tslr + temp
  X.rem <-X.remall[,c(1,5,6)]
  rem.jd.sky.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + sky + wind
  X.rem <-X.remall[,c(1,5,7)]
  rem.jd.sky.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + sky + temp
  X.rem <-X.remall[,c(1,6,7)]
  rem.jd.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + wind + temp
  X.rem <-X.remall[,c(3,4,5)]
  rem.ls2.sky <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr + tslr^2 + sky
  X.rem <-X.remall[,c(3,4,6)]
  rem.ls2.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr + tslr^2 + wind
  X.rem <-X.remall[,c(3,4,7)]
  rem.ls2.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr + tslr^2 + temp
  X.rem <-X.remall[,c(3,5,6)]
  rem.ls.sky.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr + sky + wind
  X.rem <-X.remall[,c(3,5,7)]
  rem.ls.sky.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr + sky + temp
  X.rem <-X.remall[,c(3,6,7)]
  rem.ls.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # tslr + wind + temp
  X.rem <-X.remall[,c(5,6,7)]
  rem.sky.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # sky + wind + temp
  X.rem <-X.remall[,c(1,2,3,4)]
  rem.jd2.ls2 <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + Jdate^2 + tslr + tslr^2
  X.rem <-X.remall[,c(1,2,5,6)]
  rem.jd2.sky.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)  # Jdate + Jdate^2 + sky  + wind
  X.rem <-X.remall[,c(1,2,5,7)]
  rem.jd2.sky.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)  # Jdate + Jdate^2 + sky + temp
  X.rem <-X.remall[,c(1,2,6,7)]
  rem.jd2.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + Jdate^2 + wind + temp
  X.rem <-X.remall[,c(3,4,5,6)]
  rem.ls2.sky.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)  # tslr + tslr^2 + sky  + wind
  X.rem <-X.remall[,c(3,4,5,7)]
  rem.ls2.sky.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)  # tslr + tslr^2 + sky + temp
  X.rem <-X.remall[,c(3,4,6,7)]
  rem.ls2.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # tslr + tslr^2 + wind + temp
  X.rem <-X.remall[,c(1,3,5,6)]
  rem.jd2.sky.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)  # Jdate + tslr + sky + wind
  X.rem <-X.remall[,c(1,3,5,7)]
  rem.jd2.sky.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)  # Jdate + tslr + sky + temp
  X.rem <-X.remall[,c(1,3,6,7)]
  rem.jd.ls.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + tslr + wind + temp
  X.rem <-X.remall[,c(1,3,4,5)]
  rem.jd.ls2.sky <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + tslr + tslr^2 + sky
  X.rem <-X.remall[,c(1,3,4,6)]
  rem.jd.ls2.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + tslr + tslr^2 + wind
  X.rem <-X.remall[,c(1,3,4,7)]
  rem.jd.ls2.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + tslr + tslr^2 + temp
  X.rem <-X.remall[,c(1,2,3,5)]
  rem.jd2.ls.sky <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + Jdate^2 + tslr + sky
  X.rem <-X.remall[,c(1,2,3,6)]
  rem.jd2.ls.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + Jdate^2 + tslr + wind
  X.rem <-X.remall[,c(1,2,3,7)]
  rem.jd2.ls.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + Jdate^2 + tslr + temp
  X.rem <-X.remall[,c(1,5,6,7)]
  rem.jd.sky.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + sky + wind + temp
  X.rem <-X.remall[,c(1,3,6,7)]
  rem.jd.ls.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # tslr + sky + wind + temp
  X.rem <-X.remall[,c(1,2,3,4,5)]
  rem.jd2.ls2.sky <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)         # Jdate + Jdate^2 + tslr + tslr^2 + sky
  X.rem <-X.remall[,c(1,2,3,4,6)]
  rem.jd2.ls2.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)       # Jdate + Jdate^2 + tslr + tslr^2 + wind
  X.rem <-X.remall[,c(1,2,3,4,7)]
  rem.jd2.ls2.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)       # Jdate + Jdate^2 + tslr + tslr^2 + temp
  X.rem <-X.remall[,c(1,2,3,5,6)]
  rem.jd2.ls.sky.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)    # Jdate + Jdate^2 + tslr + sky + wind
  X.rem <-X.remall[,c(1,2,3,5,7)]
  rem.jd2.ls.sky.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)    # Jdate + Jdate^2 + tslr + sky + temp
  X.rem <-X.remall[,c(1,2,3,6,7)]
  rem.jd2.ls.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)    # Jdate + Jdate^2 + tslr + sky + wind
  X.rem <-X.remall[,c(1,3,4,5,6)]
  rem.jd.ls2.sky.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)      # Jdate + tslr + tslr^2 + sky + wind
  X.rem <-X.remall[,c(1,3,4,5,7)]
  rem.jd.ls2.sky.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + tslr + tslr^2 + sky + temp
  X.rem <-X.remall[,c(1,3,4,6,7)]
  rem.jd.ls2.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + tslr + tslr^2 + wind + temp
  X.rem <-X.remall[,c(1,3,5,6,7)]
  rem.jd.ls.sky.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + tslr + sky + wind + temp
  X.rem <-X.remall[,c(1,2,3,5,6,7)]
  rem.jd2.ls.sky.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + Jdate^2 + tslr + sky + wind + temp
  X.rem <-X.remall[,c(1,3,4,5,6,7)]
  rem.jd.ls2.sky.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + tslr + tslr^2 + sky + wind + temp
  X.rem <-X.remall[,c(1,2,3,4,5,6)]
  rem.jd2.ls2.sky.wd <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + Jdate^2 + tslr + tslr^2 + sky + wind 
  X.rem <-X.remall[,c(1,2,3,4,5,7)]
  rem.jd2.ls2.sky.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + Jdate^2 + tslr + tslr^2 + sky + temp
  X.rem <-X.remall[,c(1,2,3,4,6,7)]
  rem.jd2.ls2.wd.tm <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=T)   # Jdate + Jdate^2 + tslr + tslr^2 + wind + temp
  X.rem <-X.remall[,c(1,2,3,4,5,6,7)]
  rem.full <<- try(cmulti(Y.rem | D.rem ~ X.rem, type="rem"), silent=TRUE)   # Jdate + Jdate^2 + tslr + tslr^2 + sky + wind + temp
  
  # build candidate set and get AIC list
  rem.candset <- list(rem.null, rem.jd, rem.jd2, rem.ls, rem.ls2, rem.sky, rem.wd, rem.tm, rem.jd.ls, 
                      rem.jd.sky, rem.jd.wd, rem.jd.tm, rem.ls.sky, rem.ls.wd, rem.ls.tm, rem.sky.wd,
                      rem.sky.tm, rem.wd.tm, rem.jd2.ls, rem.jd2.sky, rem.jd2.wd, rem.jd2.tm, rem.jd.ls.sky, 
                      rem.jd.ls.wd, rem.jd.ls.tm, rem.jd.sky.wd, rem.jd.sky.tm, rem.jd.wd.tm, rem.ls2.sky, 
                      rem.ls2.wd, rem.ls2.tm, rem.ls.sky.wd, rem.ls.sky.tm, rem.ls.wd.tm, rem.sky.wd.tm, 
                      rem.jd2.ls2, rem.jd2.sky.wd, rem.jd2.sky.tm, rem.jd2.wd.tm, rem.ls2.sky.wd, rem.ls2.sky.tm, 
                      rem.ls2.wd.tm, rem.jd2.sky.wd, rem.jd2.sky.tm, rem.jd.ls.wd.tm, rem.jd.ls2.sky, rem.jd.ls2.wd, 
                      rem.jd.ls2.tm, rem.jd2.ls.sky, rem.jd2.ls.wd, rem.jd2.ls.tm, rem.jd.sky.wd.tm, rem.jd.ls.wd.tm, 
                      rem.jd2.ls2.sky, rem.jd2.ls2.wd, rem.jd2.ls2.tm, rem.jd2.ls.sky.wd, rem.jd2.ls.sky.tm, 
                      rem.jd2.ls.wd.tm, rem.jd.ls2.sky.wd, rem.jd.ls2.sky.tm, rem.jd.ls2.wd.tm, rem.jd.ls.sky.wd.tm, 
                      rem.jd2.ls.sky.wd.tm, rem.jd.ls2.sky.wd.tm, rem.jd2.ls2.sky.wd, rem.jd2.ls2.sky.tm, 
                      rem.jd2.ls2.wd.tm, rem.full)
  rem.modnames <- c("rem.null", "rem.jd", "rem.jd2", "rem.ls", "rem.ls2", "rem.sky", "rem.wd", "rem.tm", "rem.jd.ls", 
                    "rem.jd.sky", "rem.jd.wd", "rem.jd.tm", "rem.ls.sky", "rem.ls.wd", "rem.ls.tm", "rem.sky.wd", 
                    "rem.sky.tm", "rem.wd.tm", "rem.jd2.ls", "rem.jd2.sky", "rem.jd2.wd", "rem.jd2.tm", "rem.jd.ls.sky",  
                    "rem.jd.ls.wd", "rem.jd.ls.tm", "rem.jd.sky.wd", "rem.jd.sky.tm", "rem.jd.wd.tm", "rem.ls2.sky", 
                    "rem.ls2.wd", "rem.ls2.tm", "rem.ls.sky.wd", "rem.ls.sky.tm", "rem.ls.wd.tm", "rem.sky.wd.tm", 
                    "rem.jd2.ls2", "rem.jd2.sky.wd", "rem.jd2.sky.tm", "rem.jd2.wd.tm", "rem.ls2.sky.wd", "rem.ls2.sky.tm", 
                    "rem.ls2.wd.tm", "rem.jd2.sky.wd", "rem.jd2.sky.tm", "rem.jd.ls.wd.tm", "rem.jd.ls2.sky", "rem.jd.ls2.wd",  
                    "rem.jd.ls2.tm", "rem.jd2.ls.sky", "rem.jd2.ls.wd", "rem.jd2.ls.tm", "rem.jd.sky.wd.tm", "rem.jd.ls.wd.tm", 
                    "rem.jd2.ls2.sky", "rem.jd2.ls2.wd", "rem.jd2.ls2.tm", "rem.jd2.ls.sky.wd", "rem.jd2.ls.sky.tm", 
                    "rem.jd2.ls.wd.tm", "rem.jd.ls2.sky.wd", "rem.jd.ls2.sky.tm", "rem.jd.ls2.wd.tm", "rem.jd.ls.sky.wd.tm", 
                    "rem.jd2.ls.sky.wd.tm", "rem.jd.ls2.sky.wd.tm", "rem.jd2.ls2.sky.wd", "rem.jd2.ls2.sky.tm", 
                    "rem.jd2.ls2.wd.tm", "rem.full")
  
  # create model selection table with AIC and AIC weights
  rem.aic <- lapply(rem.candset, AIC)
  model.tbl.rem  <- data.frame(ModName=rem.modnames, ModAIC=unlist(rem.aic))
  model.tbl.rem = model.tbl.rem[order(model.tbl.rem$ModAIC),]
  model.tbl.rem$delta.AIC = model.tbl.rem$ModAIC - min(model.tbl.rem$ModAIC)
  wt = exp(-0.5*model.tbl.rem$delta.AIC)
  model.tbl.rem$Ak.wt = wt/sum(wt)
  model.tbl.rem$Ak.wt.cum = cumsum(model.tbl.rem$Ak.wt)
  out <- model.tbl.rem
  out
}



#################################################################
## RUN ALL DISTANCE MODELS
WPARunDistanceModels <- function(distdat,...)
{    
  Y.dist <- distdat$Y
  D.dist <- distdat$D
  X.distall <- distdat$X
  
  # build all possible combinations of models. Will build AIC table after
  dist.null <<- try(cmulti(Y.dist | D.dist ~ 1, type="dis"), silent=T)                      # constant EDR (no covars)
  X.dist <- X.distall[,1:6]
  dist.ft <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)                     # forest type
  X.dist <- X.distall[,7:8]
  dist.fg <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)                     # forest group
  X.dist <- X.distall[,9:10]
  dist.sur <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)                     # surveyors
  X.dist <- X.distall[,11]
  dist.wd <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)                     # wind
  X.dist <- X.distall[,c(1:8)]
  dist.ft.fg <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)             # forest type + forest group
  X.dist <- X.distall[,c(1:6,9:10)]
  dist.ft.sur <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)            # forest type + surveyors
  X.dist <- X.distall[,c(1:6,11)]
  dist.ft.sd <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)            # forest type + wind
  X.dist <- X.distall[,c(7:10)]
  dist.fg.sur <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)            # forest group + surveyors
  X.dist <- X.distall[,c(7:8,11)]
  dist.fg.wd <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)            # forest group + wind
  X.dist <- X.distall[,c(9:11)]
  dist.sur.wd <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)            # surveyors + wind
  X.dist <- X.distall[,c(1:10)]
  dist.ft.fg.sur <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)       # forest type + forest group + surveyors
  X.dist <- X.distall[,c(1:8,11)]
  dist.ft.fg.wd <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)       # forest type + forest group + wind
  X.dist <- X.distall[,c(7:11)]
  dist.fg.sur.wd <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)       # forest group + surveyors + wind
  X.dist <- X.distall[,c(1:11)]
  dist.full <<- try(cmulti(Y.dist | D.dist ~ X.dist, type="dis"), silent=T)            # forest type + forest group + surveyors + wind
  
  dist.candset <- list(dist.null, dist.ft, dist.fg, dist.sur, dist.wd, dist.ft.fg, dist.ft.sur, 
                       dist.ft.sd, dist.fg.sur, dist.fg.wd, dist.sur.wd, dist.ft.fg.sur, 
                       dist.ft.fg.wd, dist.fg.sur.wd, dist.full)
  
  dist.modnames <- c("dist.null", "dist.ft", "dist.fg", "dist.sur", "dist.wd", "dist.ft.fg", "dist.ft.sur", 
                     "dist.ft.sd", "dist.fg.sur", "dist.fg.wd", "dist.sur.wd", "dist.ft.fg.sur", 
                     "dist.ft.fg.wd", "dist.fg.sur.wd", "dist.full")
  
  # create model selection table with AIC and AIC weights
  dist.aic <- lapply(dist.candset, AIC)
  model.tbl.dist  <- data.frame(ModName=dist.modnames, ModAIC=unlist(dist.aic))
  model.tbl.dist = model.tbl.dist[order(model.tbl.dist$ModAIC),]
  model.tbl.dist$delta.AIC = model.tbl.dist$ModAIC - min(model.tbl.dist$ModAIC)
  wt = exp(-0.5*model.tbl.dist$delta.AIC)
  model.tbl.dist$Ak.wt = wt/sum(wt)
  model.tbl.dist$Ak.wt.cum = cumsum(model.tbl.dist$Ak.wt)
  out <- model.tbl.dist
  out
}


#############################################################################
# utility functions called up by WPAGetOffsets
edr_fun <- function(r, sigma) {
  sigma^2*(1-exp(-r^2/sigma^2))/r^2
}
sra_fun <- function(t, phi) {
  1-exp(-t*phi)
}



#############################################################################
WPAGetOffsets <- function(r, t, mods, labels, boot=FALSE, ...)
{
  # identify whether the models include covariates or not. If at least one model includes covars need to use covar function
  ifelse(length(mods$remmod$coefficients)==1 & length(mods$distmod$coefficients)==1, covars <- F, covars <- T) 
  
  if (covars==F){ # no covars in either best rem or dist models
    SPP <- list("rem"=mods$remmod$coefficients, "dist"=mods$distmod$coefficients)
    VCV <- list("rem"=mods$remmod$vcov, "dist"=mods$distmod$vcov)
    qfun <- edr_fun
    pfun <- sra_fun
    if (boot) {
      phi <- exp(rnorm(1, SPP$rem, sqrt(VCV$rem[1,1])))
    } else {
      phi <- exp(SPP$rem)
    }
    names(phi) <- NULL
    if (boot) {
      sigma <- exp(rnorm(1, SPP$dist, sqrt(VCV$dist[1,1])))
    } else {
      sigma <- exp(SPP$dist)
    }
    names(sigma) <- NULL
    t.nona <- !is.na(t)
    r.nona <- !is.na(r)
    tt <- t[t.nona]
    rr <- r[r.nona]
    unlim <- ifelse(rr==Inf, TRUE, FALSE)
    A <- q <- r
    p <- t
    A[r.nona] <- ifelse(unlim, pi*sigma^2, pi*rr^2)
    p[t.nona] <- pfun(tt, phi)
    q[r.nona] <- ifelse(unlim, 1, qfun(rr, sigma))
    out <- data.frame(A, p, q)
    if (!missing(labels))
      rownames(out) <- labels
    class(out) <- c("WPAcorrections", "NoCovars", "data.frame")
    out
    
################# 
    
  } else { # rem and/or dist model(s) includes covars
    if (boot)
      require(MASS)
    SPP <- list("rem"=mods$remmod$coefficients, "dist"=mods$distmod$coefficients)
    VCV <- list("rem"=mods$remmod$vcov, "dist"=mods$distmod$vcov)
    qfun <- edr_fun
    pfun <- sra_fun
    
    ## sra/rem
    if (length(mods$remmod$coefficients)>1){ # if there are covariates on removal
      Xt <- cbind(rep(1, nrow(as.data.frame(mods$remmod$model$X.rem))),  mods$remmod$model$X.rem) # design matrix, must precede by column of 1s
      if (boot) {
        tmp <- mvrnorm(1, SPP$rem, VCV$dist)
        phi <- exp(drop(Xt %*% tmp))
      } else {
        phi <- exp(drop(Xt %*% SPP$rem))
      }
      if (is.vector(mods$remmod$model$X.rem)){
        t.nona <- !is.na(t) & !is.na(mods$remmod$model$X.rem)
      } else if (ncol(mods$remmod$model$X.rem)>1){
        t.nona <- !is.na(t) & !is.na(mods$remmod$model$X.rem[,1])
      }
      phi <- phi[t.nona]
      names(phi) <- NULL
    } else { # if the null removal model is selected
      if (boot) {
        phi <- exp(rnorm(1, SPP$rem, sqrt(VCV$rem[1,1])))
      } else {
        phi <- exp(SPP$rem)
      }
      names(phi) <- NULL
      t.nona <- !is.na(t)
    }
    
    ## edr/dist
    if (length(mods$distmod$coefficients)>1){ # if there are covariates on distance
      Xr <- cbind(rep(1, nrow(as.data.frame(mods$distmod$model$X.dist))),  mods$distmod$model$X.dist) # design matrix, must precede by column of 1s
      
      ############!!!!! NEED TO CONVERT DESIGN MATRIX FOR CATEGORICAL PREDICTORS TO DUMMY FORMAT (INTERCEPT + COLUMNS FOR LEVEL 2:LAST. SEE Xr !!!!!!
      if (boot) {
        tmp <- mvrnorm(1, SPP$dist, VCV$dist)
        sigma <- exp(drop(Xr %*% tmp))
      } else {
        sigma <- exp(drop(Xr %*% SPP$dist))
      }
      if (is.vector(mods$distmod$model$X.dist)){
        r.nona <- !is.na(t) & !is.na(mods$distmod$model$X.dist)
      } else if (ncol(mods$distmod$model$X.dist)>1){
        r.nona <- !is.na(t) & !is.na(mods$distmod$model$X.dist[,1])
      }
      sigma <- sigma[r.nona]
      names(sigma) <- NULL
    } else { # if the null distance model is selected
      if (boot) {
        sigma <- exp(rnorm(1, SPP$dist, sqrt(VCV$dist[1,1])))
      } else {
        sigma <- exp(SPP$dist)
      }
      names(sigma) <- NULL
      r.nona <- !is.na(r)
    }
    ## output
    tt <- t[t.nona]
    rr <- r[r.nona]
    unlim <- ifelse(rr==Inf, TRUE, FALSE)
    A <- q <- r
    p <- t
    A[r.nona] <- ifelse(unlim, pi*sigma^2, pi*rr^2)
    p[t.nona] <- pfun(tt, phi)
    q[r.nona] <- ifelse(unlim, 1, qfun(rr, sigma))
    out <- data.frame(A, p, q)
    if (!missing(labels))
      rownames(out) <- labels
    class(out) <- c("WPAcorrections", "YesCovars", "data.frame")
    out
    
  } # end covars test loop
  
} # end WPAGetOffsets function
  

######################################################################
## creates offset
corrections2offset <- function(x, link="log", na.rm=FALSE) {
  link <- match.arg(link, c("log"))
  linkfun <- switch(link,
                    "log"=log)
  if (!inherits(x, "WPAcorrections"))
    stop("not WPAcorrections object")
  out <- rowSums(linkfun(x), na.rm=na.rm)
  attr(out, "link") <- link
  out
}




##########################################################################################
### TEST FOR OVERDISPERSION IN POISSON MODELS
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}



####################################################################################
## GBM PLOT 2.2
# function to plot gbm response variables, with the option
# of adding a smooth representation of the response if requested
# additional options in this version allow for plotting on a common scale
# note too that fitted functions are now centered by subtracting their mean
# 
# Nicole Michel, National Audubon Society, February 2016
# Modified from code by j. leathwick/j. elith - March 2007
#


gbm.plot22 <-
  function(gbm.object,                # a gbm object - could be one from gbm.step
           variable.no = 0,               # the var to plot - if zero then plots all
           smooth = FALSE,                # should we add a smoothed version of the fitted function 
           rug = TRUE,                    # plot a rug of deciles
           n.plots = length(pred.names),  # plot the first n most important preds
           common.scale = TRUE,           # use a common scale on the y axis
           write.title = TRUE,            # plot a title above the plot
           y.label = "fitted function",   # the default y-axis label
           x.label = NULL,                # the default x-axis label
           show.contrib = TRUE,           # show the contribution on the x axis     
           title.text = "title text here", # NM added 5/15/14
           plot.hires = FALSE,             # NM added 6/9/14
           file.name = "file name here",   # NM added 6/9/14
           plot.layout = c(1,1),          # define the default layout for graphs on the page
           ...      # other arguments to pass to the plotting
           # useful options include cex.axis, cex.lab, etc.
           
  )
  {
    
    if (! require(gbm) ) { stop ('you need to install the gbm package to run this function') }
    if (! require(splines) ) { stop ('you need to install the splines package to run this function') }
    
    gbm.call <- gbm.object$gbm.call
    gbm.x <- gbm.call$gbm.x
    pred.names <- gbm.call$predictor.names
    response.name <- gbm.call$response.name
    
    data <- gbm.call$dataframe
    
    max.plots <- plot.layout[1] * plot.layout[2]
    plot.count <- 0
    n.pages <- 1
    
    if (length(variable.no) > 1) { stop("only one response variable can be plotted at a time") }
    
    if (variable.no > 0) {   #we are plotting all vars in rank order of contribution
      n.plots <- 1
    }
    
    max.vars <- length(gbm.object$contributions$var)
    if (n.plots > max.vars) {
      n.plots <- max.vars
      warning("reducing no of plotted predictors to maximum available (",max.vars,")")
    }
    
    predictors <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
    responses <- list(rep(NA,n.plots)) # matrix(0,ncol=n.plots,nrow=100)
    
    for (j in c(1:n.plots)) {  #cycle through the first time and get the range of the functions
      if (n.plots == 1) {
        k <- variable.no
      } else {
        k <- match(gbm.object$contributions$var[j],pred.names)
      }
      if (is.null(x.label)) {
        var.name <- gbm.call$predictor.names[k]
      } else {
        var.name <- x.label
      }
      pred.data <- data[ , gbm.call$gbm.x[k]]
      
      response.matrix <- gbm::plot.gbm(gbm.object, k, return.grid = TRUE)
      
      predictors[[j]] <- response.matrix[,1]
      if (is.factor(data[,gbm.call$gbm.x[k]])) {
        predictors[[j]] <- factor(predictors[[j]],levels = levels(data[,gbm.call$gbm.x[k]]))
      }
      responses[[j]] <- response.matrix[,2] - mean(response.matrix[,2])
      
      if(j == 1) {
        ymin = min(responses[[j]])
        ymax = max(responses[[j]])
      } else {
        ymin = min(ymin,min(responses[[j]]))
        ymax = max(ymax,max(responses[[j]]))
      }
    }
    
    # now do the actual plots
    
    op <- par(no.readonly = TRUE) 
    par(mfrow = plot.layout, pin=c(6,4),...)
    
    for (j in c(1:n.plots)) {
      
      if (plot.count == max.plots) {
        plot.count = 0
        n.pages <- n.pages + 1
      }
      
      plot.count <- plot.count + 1
      
      if (n.plots == 1) {
        k <- match(pred.names[variable.no],gbm.object$contributions$var)
        if (show.contrib) {
          x.label <- paste(var.name,"  (",round(gbm.object$contributions[k,2],1),"%)",sep="")
        }
      } else {
        k <- match(gbm.object$contributions$var[j],pred.names)
        var.name <- gbm.call$predictor.names[k]
        if (show.contrib) {
          x.label <- paste(var.name,"  (",round(gbm.object$contributions[j,2],1),"%)",sep="")
        } else x.label <- var.name
      }
      
      if (common.scale) {
        plot(predictors[[j]],responses[[j]],ylim=c(ymin,ymax), type='l',
             xlab = x.label, ylab = y.label, cex.axis=1.4, cex.lab=1.4, ...)
      } else {
        plot(predictors[[j]],responses[[j]], type='l', 
             xlab = x.label, ylab = y.label, cex.axis=1.4, cex.lab=1.4, ...)
      }
      if (smooth & is.vector(predictors[[j]])) {
        temp.lo <- loess(responses[[j]] ~ predictors[[j]], span = 0.3)
        lines(predictors[[j]],fitted(temp.lo), lty = 2, col = 2)
      }
      if (plot.count == 1) {
        if (write.title) {
          title(paste(response.name," - page ",n.pages,sep=""))
        }
        if (rug & is.vector(data[,gbm.call$gbm.x[variable.no]])) {
          rug(quantile(data[,gbm.call$gbm.x[variable.no]], probs = seq(0, 1, 0.1), na.rm = TRUE))
        }
      } else {
        if (write.title & j == 1) {
          title(response.name)
        }
        if (rug & is.vector(data[,gbm.call$gbm.x[k]])) {
          rug(quantile(data[,gbm.call$gbm.x[k]], probs = seq(0, 1, 0.1), na.rm = TRUE))
        }
      }
    }
    par(op)
    
  }


