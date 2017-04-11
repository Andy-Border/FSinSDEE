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
source("MIFS2.R")
source("mRMR.R")
source("INMIFS.R")
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
mRMRresult<- mRMR(dData = dData, rFNum = ncol(dData),k=ncol(dData)-1)
INMIFSresult<- INMIFS(dData = dData, rFNum = ncol(dData),k=ncol(dData)-1)
MIFSResult<- MIFS(dData = dData, rFNum = ncol(dData),k=ncol(dData)-1,Be=0.75)
resultOfFS <- MIFSResult

weight <- getFeatureWeights_None(resultOfFS$S)
print("feature weight:");print(weight)

#resultOfFS$S <- c(7,9,2,10,3,5,11,8,4,1,6)
print(names(dData[resultOfFS$S]))
result <- data.frame(MMRE=numeric(ncol(dData)-1), 
                     PRED=numeric(ncol(dData)-1), 
                     MdMRE=numeric(ncol(dData)-1),
                     stringsAsFactors=FALSE)

numOfExperiment=10;
AllExpResult <- data.frame(numOfFeatures=numeric(numOfExperiment), 
                           MMRE=numeric(numOfExperiment), 
                           PRED=numeric(numOfExperiment), 
                           MdMRE=numeric(numOfExperiment),
                           stringsAsFactors=FALSE)

#Start Evaluation

for(expNum in 1:numOfExperiment){
  #Part 3: Devide dataSet to k folds
  kFoldnum=10
  folds <- createFolds(dData$ResponseVariable, k = kFoldnum, list = TRUE, returnTrain = FALSE)
  
  for(k in 1:ncol(dData)-1){
    vecBestSubset <- resultOfFS$S[1:k]
    tempResult <- data.frame(MMRE=numeric(kFoldnum), 
                             PRED=numeric(kFoldnum), 
                             MdMRE=numeric(kFoldnum),
                             stringsAsFactors=FALSE)
    for(i in 1:kFoldnum){
      ##Seperate input data into two parts: TrainingSet and TestingSet
      testingData <- dData[folds[[i]],]
      trainingData <-dData[-folds[[i]],]
      evaluation <-  EvalTesting(testingData, trainingData, weight,vecBestSubset ,vecColType)
      tempResult[i,'MMRE'] = MMREFunc(evaluation, nrow(testingData))
      tempResult[i,'PRED'] = PREDFunc(evaluation, nrow(testingData), 0.25)
      tempResult[i,'MdMRE'] = MdMREFunc(evaluation, nrow(testingData))
    }
    tempResult["Mean" ,] <- colMeans(tempResult,na.rm=T)
    result[k,] <- tempResult["Mean" ,] 
  }
  #print(result)
  maxK=which.max(result[,'PRED'])
  AllExpResult[expNum,'numOfFeatures'] <- maxK
  AllExpResult[expNum,'MMRE'] <- result[maxK,'MMRE']
  AllExpResult[expNum,'PRED'] <- result[maxK,'PRED']
  AllExpResult[expNum,'MdMRE'] <- result[maxK,'MdMRE']
}
print(AllExpResult)
