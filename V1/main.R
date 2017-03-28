## Created by Andy Zhao
## 20-3-2017
#Part 1: Data Preparation
rm(list = ls())
setwd("D:/R/FSinSDEEV2")
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
sData=ISBSG[,-responseVNum]
entp <- getEntropy(sData)

getMatrixs(data = ISBSG,entp = entp)

#Part 2: Feature Selection
resultOfFS<- mRMR(dData = ISBSG, rFNum = responseVNum,k=ncol(dData)-1)
vecBestSubset <- resultOfFS$S

weight <- getFeatureWeights_None(vecBestSubset)
print("feature weight:");print(weight)
#Part 3: Devide dataSet to k folds
kFoldnum=3

#for(evNum in 1:kFoldnum){
##Seperate input data into two parts: TrainingSet and TestingSet
rtnList <- seperateSets(dData, kFoldNbr)
vecTestingIds = rtnList$var1
vecTrainingIds = rtnList$var2
testingData <-
  matrix(nrow = length(vecTestingIds), ncol = ncol(dData))
trainingData <-
  matrix(nrow = length(vecTrainingIds), ncol = ncol(dData))

for (i in 1:length(vecTestingIds)) {
  testingData[i, ] = dData[vecTestingIds[i], ]
}
for (i in 1:length(vecTrainingIds)) {
  trainingData[i, ] = dData[vecTrainingIds[i], ]
}
#evaluate testing set
evaluation <-
  EvalTesting(testingData, trainingData, weight, vecBestSubset)

vecMMRE = 0
vecPRED = 0
vecMdMRE = 0

evaluation <-
  EvalTesting(testingData, trainingData, weight, vecBestSubset)

#collect the experiment results
result <- vector(length = 3)

result[1] = MMREFunc(evaluation, nrow(testingData))

result[2] = PREDFunc(evaluation, nrow(testingData), 0.25)

result[3] = MdMREFunc(evaluation, nrow(testingData))

#print("TestingSet Result:")
#print(result);
vecMMRE[z] <- result[1]

vecPRED[z] <- result[2]

vecMdMRE[z] <- result[3]


#Part 4: Case Selection
#Part 5: Case Adaption
#Part 6: Evaluation
#}
print(vecMMRE)

print("PREDs:")
print(vecPRED)

print("MdMREs:")
print(vecMdMRE)

print("Average in MMRE:")
print(mean(vecMMRE))
print("Average in PRED:")
print(mean(vecPRED))
print("Average in MdMRE:")
print(mean(vecMdMRE))

