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
kFoldnum=3

#for(evNum in 1:kFoldnum){
##Seperate input data into two parts: TrainingSet and TestingSet

folds <- createFolds(dData$ResponseVariable, k = 10, list = TRUE, returnTrain = FALSE)

testingData <- dData[folds[[1]],]
trainingData <-dData[-folds[[1]],]


vecMMRE = 0
vecPRED = 0
vecMdMRE = 0

evaluation <-
  EvalTesting(testingData, trainingData, weight, vecBestSubset,vecColType)

#collect the experiment results
result <- NULL

result$MMRE = MMREFunc(evaluation, nrow(testingData))
result$PRED = PREDFunc(evaluation, nrow(testingData), 0.25)
result$MdMRE = MdMREFunc(evaluation, nrow(testingData))
print(result)
#print("TestingSet Result:")
#print(result);
# # vecMMRE[z] <- result[1]
# # 
# # vecPRED[z] <- result[2]
# # 
# # vecMdMRE[z] <- result[3]
# # 
# 
# #Part 4: Case Selection
# #Part 5: Case Adaption
# #Part 6: Evaluation
# #}
# print(vecMMRE)
# 
# print("PREDs:")
# print(vecPRED)
# 
# print("MdMREs:")
# print(vecMdMRE)
# 
# print("Average in MMRE:")
# print(mean(vecMMRE))
# print("Average in PRED:")
# print(mean(vecPRED))
# print("Average in MdMRE:")
# print(mean(vecMdMRE))

