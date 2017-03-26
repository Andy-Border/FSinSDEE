## Created by Andy ZHao
## 20-2-2017
#_______________________________________________________________________________
# Initialization
library(dplyr)
library(infotheo)
library(graphics)
library(ggplot2)
library(ggthemes)
setwd("D:/R/FSinSDEEV1")
rm(list = ls())
source("import_ISBSG_2.R")
# import the ISBSG dataset
# ISBSG is Currently  6006 obs. of 25 variables
source("filter_projects.R")
source("select_features.R")
# source("select_featuresOrigin.R")
source("esem2016_pre.R")
source("Distribution.R")
# ISBSG$Data.Quality.Rating <- NULL
# ISBSG$UFP.rating <- NULL
# ISBSG$Count.Approach <- NULL
# ISBSG$Summary.Work.Effort <- NULL

# Por filtrado RL=1
ISBSG$Resource.Level <- NULL

# Render tfhe dataset in good shape.
# Following operations are processed.
# (a)Filter the projects.
# (b)Remove variables used in filtering.
# (c)Remove variables with too many NAs, over a
# certain threshold(40%).
# (d)After filtering, some of the levels are useless,
# drop these levels.

# ISBSG is Currently  621 obs. of 12 variables
#_______________________________________________________________________________
#InformationGain <- sort.by.Information.gain(Summary.Work.Effort~.,ISBSG)
#print(InformationGain)

###########################################################################
################ Author: Andy Zhao ###############
################ Last update: 2016/02/25 ###############
################ Note: Supervised Mutual Information Feature Selection ###############
###########################################################################
#Load library#

dData = ISBSG
responseVNum  <-  which(names(ISBSG) %in% "Normalised.Work.Effort.Level.1")
#######MIFS########
#source("MIFS.R")
#fSet<- MIFS(dData = ISBSG, rFNum = responseVNum, Be = 1.0,k=5);print(colnames(dData)[fSet])
# fit <- lm(dData[,responseVNum]~dData[,17]+dData[,1]+dData[,13])

#######mRMR########
source("mRMR.R")
fSet<- mRMR(dData = ISBSG, rFNum = responseVNum,k=ncol(dData)-1)
print(colnames(dData)[fSet$S])
print(fSet$mrmr)
print(fSet$featureNames)
fSet$featureNames <- recode(fSet$featureNames,
                            Application.Group="AG",
                            Used.Methodology ="UM",
                            Primary.Programming.Language ="PPL",
                            Industry.Sector ="IS",
                            Adjusted.Function.Points = "AFP",
                            X1st.Data.Base.System = "1DBS",
                            Language.Type="LT"     ,
                            Project.Elapsed.Time="PET",
                            Development.Type = "DT",
                            Used.Methodology="UM",
                            Functional.Size = "FSZ",
                            Development.Platform="DP"
)
MIdistribution <- arrange(fSet,desc(vmi))
MIdistribution$fNamesForDistribution <- factor(MIdistribution$featureNames, levels=unique(MIdistribution$featureNames))
drawPic <- ggplot(MIdistribution,aes(x=fNamesForDistribution,y=vmi))
drawPic+ geom_bar(stat= 'identity', width = 0.5)+ 
  theme_economist()+
  ggtitle(paste("mRMR of Features"))+
  theme(axis.text.x=element_text(hjust=1,vjust=0.5))


drawPic <- ggplot(fSet,aes(x=featureNames,y=mrmr))
drawPic+ geom_bar(stat= 'identity', width = 0.5)+ 
  theme_economist()+
  ggtitle(paste("mRMR of Features"))+
  theme(axis.text.x=element_text(hjust=1,vjust=0.5))
#results <- ordered.by.mRMR("Summary.Work.Effort",dData)
#fSet<- MIFS(dData = ISBSG, rFNum = responseVNum, Be = 1.0,k=5);print(colnames(dData)[fSet])
results <- ordena.por.coeficiente("Normalised.Work.Effort.Level.1",ISBSG)
print(results)
