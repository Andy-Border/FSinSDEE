## Created by Andy Zhao
## 20-3-2017
#Part 1: Data Preparation
rm(list = ls())
setwd("D:/R/FSinSDEEV2")
library("dplyr")
library("caret")
source("import_ISBSG.R")
source("filter_projects.R")
#source("select_features.R")
source("esem2016_pre.R")
#source("MICalculation.R")
#source("Evaluation.R")
source("mRMR.R")
source("All_Functions.R")
dData = ISBSG
responseVNum  <-  which(names(ISBSG) %in% "Normalised.Work.Effort.Level.1")
dData=mutate(dData,ResponseVariable=Normalised.Work.Effort.Level.1)
dData=dData[,-responseVNum]
responseVNum <- ncol(dData)
sData=dData[,-responseVNum]
entp <- getEntropy(sData)
vecColType <- c(1,1,1,1,1,1,0,0,0,1,1)
#getMatrixs(data = dData,entp = entp)

#Part 2: Feature Selection
resultOfFS<- mRMR(dData = dData, rFNum = ncol(dData),k=ncol(dData)-1)
vecBestSubset <- resultOfFS$S

weight <- getFeatureWeights_None(vecBestSubset)
print("feature weight:");print(weight)
#Part 3: Devide dataSet to k folds
kFoldnum=10

folds <- createFolds(dData$ResponseVariable, k = kFoldnum, list = TRUE, returnTrain = FALSE)
vecMMRE = 0
vecPRED = 0
vecMdMRE = 0
result <- data.frame(MMRE=numeric(kFoldnum), 
                     PRED=numeric(kFoldnum), 
                     MdMRE=numeric(kFoldnum),
                     stringsAsFactors=FALSE) 
for(i in 1:kFoldnum){
  ##Seperate input data into two parts: TrainingSet and TestingSet
  testingData <- dData[folds[[i]],]
  trainingData <-dData[-folds[[i]],]
  evaluation <-  EvalTesting(testingData, trainingData, weight, vecBestSubset,vecColType)
  result[i,'MMRE'] = MMREFunc(evaluation, nrow(testingData))
  result[i,'PRED'] = PREDFunc(evaluation, nrow(testingData), 0.25)
  result[i,'MdMRE'] = MdMREFunc(evaluation, nrow(testingData))
}
result[-11,]
result["Mean" ,] <- colMeans(result,na.rm=T)

View(result)


