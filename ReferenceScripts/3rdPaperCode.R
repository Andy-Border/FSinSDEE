###########################################################################
################ Author: Shihai Shi ###############
################ Last update: 2013/08/11 ###############
################ Note: Unsupervised Feature Selection ###############
###########################################################################
#Log#
#Add cross validation into unsupervised feature selection method#
#Load library#
library(infotheo)

library(graphics)

#Configuration list#
config <- list(
  wd = "D:/R/TestFolderFor3rdPaper",
  fileName = c("deshstd.txt", "r8std.txt"),
  similarityMatrix = c("mi", "su", "nmi", "mici"),
  featureWeight = c("none", "su"),
  featureSelection = c("supervised", "unsupervised"),
  evaluationApproach = c("kfold", "leave_one_out"),
  kCluster = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  kfold = c(3, 5, 10),
  kNearestNeighbour = c(1, 2, 3, 4, 5)
)
#Settings of this experiment#
wd <- config[["wd"]]
#working directory
fileName <- config[["fileName"]][1]
#file name
vecColType <-
  vector(length = 11)
#feature column type: "1" for categorical data
# and "0" for numeric data
if (fileName == config[["fileName"]][1]) {
  vecColType <- c(0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0)
  ##deshstd.txt
} else if (fileName == config[["fileName"]][2]) {
  vecColType <- c(1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0)
  ##r8std.txt ISBSG Release8
}
k = config[["kNearestNeighbour"]][3]
#number of nearest neighbours in KNN
kCluster = config[["kCluster"]][2]
#number of clusters in hierachical clustering
similarityMatrix = config[["similarityMatrix"]][3]
#the approach for similarity measurement
kFoldNbr = config[["kfold"]][3] # 10

#Data used in this experiment
setwd(wd)

dData = as.matrix(read.table(fileName, header = TRUE))

colNumber = ncol(dData)

sData = dData[, -colNumber]
#eliminate the "effort" column for unsupervised learning
#Main entrance of this program#
mainFunc <- function(wd, fileName) {
  ###############Unsupervised Feature Selection######################
  #Entropy of each feature#
  entp = getEntropy(sData)
  
  smltMatrix = 0
  #similarity matrix
  dissmltMatrix = 0
  #dissimilarity matrix
  if ("mi" == similarityMatrix) {
    #mi == nmi
    smltMatrix = getMiMatrix(sData)
    #mutual information matrix#
    dissmltMatrix = getDissimilarityMatrix_MI(smltMatrix)
    
  } else if ("su" == similarityMatrix) {
    miMat = getMiMatrix(sData)
    #symmetric uncertainty matrix#
    smltMatrix = getSymmetricUncertainty(miMat, entp)
    
    dissmltMatrix = getDissimilarityMatrix_SU(smltMatrix)
    
  } else if ("nmi" == similarityMatrix) {

    miMat = getMiMatrix(sData)
    #normalized mutual information matrix#
    smltMatrix = getNormalizedMiMat(miMat, entp)
    
    dissmltMatrix = getDissimilarityMatrix_NMI(smltMatrix)
    
  } else if ("mici" == similarityMatrix) {
    dissmltMatrix = getDissimilarityMatrix_MICI(sData)
    
  }
  #get triangle distance matrix
  tDSM = getTriangleDSM(dissmltMatrix)
  
  #Hierarchical clustering#
  hc = hclust(tDSM, "complete")
  
  #print("Cluster results:");
  print(hc$merge)
  
  plot(hc)
  
  plot(hc, hang = -1)
  
  #cluster matrix: in each row i, clusterMatrix[i,j]==1 means that feature j is selected into
  #one cluster in clustering step i.
  clusterMatrix = getClusterFeatures(hc$merge, colNumber - 1)
  
  print("Cluster Matrix:")
  
  print(clusterMatrix)
  
  parseClusterResults = parseClusters(hc$merge, kCluster)
  
  #get representative feature in each cluster: the feature with smallest distance sum to all the
  #other features in the cluster will be selected
  vecRepresentativeFeatures = getRepresentativeFeature_MI(parseClusterResults, clusterMatrix
                                                          , kCluster)
  
  #vecRepresentativeFeatures=getRepresentativeFeature_TopKDis(clusterMatrix,dissmltMatrix,kCluster);
  #get the needed features for evaluation
  vecBestSubset = vecRepresentativeFeatures
  
  print("Selected Features:")
  
  print(vecBestSubset)
  
  ###########################Evaluate model performance#########################
  #Evaluate estimation model: leave-one-out#
  vecMMRE = 0
  
  vecPRED = 0
  
  vecMdMRE = 0
  
  #kFold=nrow(dData);
  #get feature weight equation for case selection
  weight <- getFeatureWeights_None(vecBestSubset)
  
  print("feature weight:")
  
  print(weight)
  
  #Each case will act as testing set once and the other cases act as training set#
  for (z in 1:kFoldNbr) { # = 3
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
    
  }
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
}
###############################################################################
##### Part A: Get value of entropy, mutual information, dissimilarity #####
###############################################################################
##A.1 Calculate entropy value of each column
getEntropy <- function(dData) {
  entp <- vector(length = ncol(dData))
  
  for (i in 1:ncol(dData)) {
    ##discretize continuous data and calculate entropy
    entp[i] = entropy(discretize(dData[, i]))
    
  }
  #print("Entropy vector:");
  #print(entp);
  return(entp)
  
}
##A.2 Calculate mutual information matrix between two columns
getMiMatrix <- function(dData) {
  ##Allocate a new matrix to store MI results
  miMat <- matrix(nrow = ncol(dData), ncol = ncol(dData))
  ##Get MI of every two cols (Independent-Independent & Independent-Response)
  for (i in 1:ncol(dData)) {
    for (j in 1:ncol(dData)) {
      ## ##discretize continuous data
      miMat[i, j] = mutinformation(discretize(dData[, i]), discretize(dData[, j]))
      
    }
  }
  #print("Mutual informatin matrix:");
  #print(miMat)
  return(miMat)
}
##A.3 Calculate normalized mutual information matrix between two columns
getNormalizedMiMat <- function(miMat, entp) {
  NMiMat = matrix(nrow = length(entp), ncol = length(entp))
  
  for (i in 1:length(entp)) {
    for (j in 1:length(entp)) {
      NMiMat[i, j] = miMat[i, j] / (min(entp[i], entp[j]))
      
    }
  }
  return(NMiMat)
  
}
##A.4 Calculate symmetric uncertaity between two features
getSymmetricUncertainty <- function(miMat, entp) {
  miWeight = matrix(nrow = length(entp), ncol = length(entp))
  
  for (i in 1:length(entp)) {
    for (j in 1:length(entp)) {
      miWeight[i, j] = (2 * miMat[i, j]) / (entp[i] + entp[j])
      
    }
  }
  #print("Symmetric uncertainty matrix:");
  #print(miWeight);
  return (miWeight)
  
}
##A.5 Get dissimilarity matrix symmetric uncertainty
getDissimilarityMatrix_SU <- function(miSU) {
  mat = 1 - miSU[c(1:10), c(1:10)]
  
  #print("Dissimilarity matrix(SU):");
  #print(mat);
  return(mat)
  
}
##A.6 Get dissimilarity matrix of standard mutual inforamtion
getDissimilarityMatrix_MI <- function(miMat) {
  mat = 1 - miMat[c(1:10), c(1:10)]
  
  #print("Dissimilarity matrix(MI):");
  #print(mat);
  return(mat)
  
}
##A.7 Get dissimilarity matrix of normalized mutual inforamtion
getDissimilarityMatrix_NMI <- function(NMiMat) {
  mat = 1 - NMiMat[c(1:10), c(1:10)]
  
  #print("Dissimilarity matrix(NMI):");
  #print(mat);
  return(mat)
  
}
##A.8 Get dissimilarity matrix of maximal information compression index
getDissimilarityMatrix_MICI <- function(sData) {
  colNbr = ncol(sData)
  
  MICIMat = matrix(nrow = colNbr, ncol = colNbr)
  
  varVector = vector(length = colNbr)
  
  for (i in 1:length(varVector)) {
    varVector[i] = var(sData[, i])
    
  }
  ccMat = matrix(nrow = colNbr, ncol = colNbr)
  
  for (i in 1:colNbr) {
    for (j in 1:colNbr) {
      temp1 = cov(sData[, i], sData[, j])
      
      temp2 = sqrt(varVector[i] * varVector[j])
      
      ccMat[i, j] = temp1 / temp2
      
    }
  }
  #print("Correlation Coefficient:");
  #print(ccMat);
  for (i in 1:colNbr) {
    for (j in 1:colNbr) {
      temp1 = varVector[i] + varVector[j]
      
      temp2 = sqrt((varVector[i] + varVector[j]) ^ 2 - 4 * varVector[i] *
                     varVector[j] * (1 - ccMat[i, j] ^ 2))
      
      MICIMat[i, j] = (temp1 - temp2) / 2
      
    }
  }
  #print("MICI Matrix:");
  #print(MICIMat);
  return(MICIMat)
  
}
##A.9 Get dissimilarity matrix in triangle format using "as.dist" function
getTriangleDSM <- function(dsm) {
  tDSM = dsm
  
  colNum = ncol(dsm)
  
  for (i in 1:colNum) {
    for (j in i:colNum) {
      tDSM[i, j] = 0
      
    }
  }
  #print(tDSM);
  return(as.dist(tDSM))
  
}
###############################################################################
# Part B: Unsupervised feature selection to get representative feature of each cluster #
###############################################################################
#B1. Get representative feature from each cluster
getRepresentativeFeature_TopKDis <- function(clusterMatrix, dsm, k) {
  clusterMatrixRow = nrow(clusterMatrix)
  
  clusterMatrixCol = ncol(clusterMatrix)
  
  vecIsFeatureSelected = c(rep(0, 10))
  
  vecRepresentativeFeatures = vector(length = k)
  
  for (i in 1:k) {
    vecRepresentativeFeatures[i] = getRepresentativeFeature_Core_MinDisSum(clusterMatrix[clusterMatrixRow +
                                                                                           1 - i, ], dsm, vecIsFeatureSelected)
    
    
    vecIsFeatureSelected[vecRepresentativeFeatures[i]] = 1
    
  }
  #print(vecRepresentativeFeatures);
  return(vecRepresentativeFeatures)
  
}
#B2.1. Get representative feature from each cluster (Core:Minimum distance sum)
getRepresentativeFeature_Core_MinDisSum <-
  function(clusterMatrixRow,
           dsm,
           vecIsFeatureSelected) {
    representativeFeature = 0
    
    disMin = 100
    
    for (i in 1:length(clusterMatrixRow)) {
      disSum = 0
      
      if (1 == clusterMatrixRow[i] && 0 == vecIsFeatureSelected[i]) {
        for (j in 1:length(clusterMatrixRow)) {
          if (1 == clusterMatrixRow[j] && j != i) {
            disSum = disSum + dsm[i, j]
            
          }
        }
        if (disSum < disMin) {
          disMin = disSum
          
          representativeFeature = i
          
        }
      }
    }
    if (0 == representativeFeature) {
      for (i in 1:length(vecIsFeatureSelected)) {
        if (0 == vecIsFeatureSelected[i]) {
          representativeFeature = i
          
        }
      }
    }
    return(representativeFeature)
    
  }
#B2.2. Get representative feature from each cluster (Core:Maximal distance sum)
getRepresentativeFeature_Core_MaxDisSum <-
  function(clusterMatrixRow,
           dsm,
           vecIsFeatureSelected) {
    representativeFeature = 0
    
    disMax = 0
    
    for (i in 1:length(clusterMatrixRow)) {
      disSum = 0
      
      if (1 == clusterMatrixRow[i] && 0 == vecIsFeatureSelected[i]) {
        for (j in 1:length(clusterMatrixRow)) {
          if (1 == clusterMatrixRow[j] && j != i) {
            disSum = disSum + dsm[i, j]
            
          }
        }
        if (disSum > disMax) {
          disMax = disSum
          
          representativeFeature = i
          
        }
      }
    }
    if (0 == representativeFeature) {
      for (i in 1:length(vecIsFeatureSelected)) {
        if (0 == vecIsFeatureSelected[i]) {
          representativeFeature = i
          
        }
      }
    }
    return(representativeFeature)
    
  }
#B2.3. Get representative feature from each cluster (Core:mutual information with target
#feature)
getRepresentativeFeature_MI <-
  function(parseClusterResult,
           clusterMatrix,
           kValue) {
    representativeFeature = vector(length = kValue)
    
    miMat = getMiMatrix(dData)
    
    targetFeatureColNbr = ncol(dData)
    
    if (1 == length(parseClusterResult)) {
      maxMiValue = 0
      
      for (i in 1:ncol(clusterMatrix)) {
        if (miMat[i, targetFeatureColNbr] > maxMiValue) {
          maxMiValue = miMat[i, targetFeatureColNbr]
          
          representativeFeature[1] = i
          
        }
      }
      return (representativeFeature)
      
    }
    for (i in 1:kValue) {
      tempValue = parseClusterResult[i]
      
      if (tempValue < 0) {
        representativeFeature[i] = 0 - tempValue
        
      } else{
        colNbr = ncol(clusterMatrix)
        
        maxMiValue = 0
        
        for (j in 1:colNbr) {
          if (1 == clusterMatrix[tempValue, j]) {
            if (miMat[j, targetFeatureColNbr] > maxMiValue) {
              maxMiValue = miMat[j, targetFeatureColNbr]
              
              representativeFeature[i] = j
              
            }
          }
        }
      }
    }
    #print("Representative feature:");
    #print(representativeFeature);
    return(representativeFeature)
    
  }
#B3. Get hierachical clustering matrix: each row represents one iteration in the clustering
getClusterFeatures <- function(clusterResult, featureNumber) {
  clusterMatrix = matrix(0:0, nrow = (featureNumber - 1), ncol = featureNumber)
  
  iteration = featureNumber - 1
  
  for (i in 1:iteration) {
    temp1 = clusterResult[i, 1]
    
    if (temp1 < 0) {
      clusterMatrix[i, abs(temp1)] = 1
      
    } else{
      for (x in 1:featureNumber) {
        if (1 == clusterMatrix[temp1, x]) {
          clusterMatrix[i, x] = 1
          
        }
      }
    }
    temp2 = clusterResult[i, 2]
    
    if (temp2 < 0) {
      clusterMatrix[i, abs(temp2)] = 1
      
    } else{
      for (y in 1:featureNumber) {
        if (1 == clusterMatrix[temp2, y]) {
          clusterMatrix[i, y] = 1
          
        }
      }
    }
  }
  #print("Cluster Matrix:");
  #print(clusterMatrix);
  return(clusterMatrix)
  
}
#B4. Parse all the clusters in each step
parseClusters <- function(mergeMat, kValue) {
  result = c()
  
  mergeMatRowNbr = nrow(mergeMat)
  
  if (1 == kValue) {
    result = 0
    
  } else{
    for (i in 1:(kValue - 1)) {
      pos = mergeMatRowNbr + 1 - i
      
      if (0 != length(result)) {
        for (j in 1:length(result)) {
          if (pos == result[j]) {
            result = result[-j]
            
            break
            
          }
        }
      }
      leftValue = mergeMat[mergeMatRowNbr + 1 - i, 1]
      
      result[length(result) + 1] = leftValue
      
      rightValue = mergeMat[mergeMatRowNbr + 1 - i, 2]
      
      result[length(result) + 1] = rightValue
      
    }
  }
  #print("Parse Cluster:");
  #print(result);
  return(result)
  
}
###############################################################################
########################### Part C: Feature weight ############################
###############################################################################
#C.1 none weight: all feature weight is "1"
getFeatureWeights_None <- function(vecS) {
  ##Initilize a vector to store weight values of independent variables
  weightVector <- vector(length = length(vecS))
  
  for (i in 1:10) {
    weightVector[i] = 1
    
  }
  return(weightVector)
  
}
#C.2 Use symmetric uncertainty as feature weight
getFeatureWeights_SU <- function(vecS) {
  miMat = getMiMatrix(dData)
  
  entp = getEntropy(dData)
  
  suMatrix = getSymmetricUncertainty(miMat, entp)
  
  weightVector = vector(length = length(vecS))
  
  targetFeature = ncol(dData)
  
  for (i in 1:length(vecS)) {
    weightVector[i] = suMatrix[vecS[i], targetFeature]
    
  }
  return(weightVector)
  
}
###############################################################################
######################## Part D: Evaluate performance ######################
###############################################################################
#D.1 Devide raw data into two parts: training set and testing set
seperateSets <- function(dData, kFold) {
  ##Pick out TestingSet(nrow(dData)/kFold records) by random sampling
  dataRange <- 0
  
  dataRange <- 1:nrow(dData)
  
  vecTestingIds <- 0
  
  vecTestingIds <-
    sample(dataRange, round(nrow(dData) / kFold), replace = FALSE)
  
  ##Pick out TrainingSet from the rest records
  vecTrainingIds <- 0
  
  rowcount = 1
  
  for (i in 1:nrow(dData)) {
    if (!any(vecTestingIds == i)) {
      vecTrainingIds[rowcount] = i
      
      rowcount = rowcount + 1
      
    }
  }
  ##return two vectors by using list
  rtnList <- list(var1 = vecTestingIds, var2 = vecTrainingIds)
  
  return(rtnList)
  
}
#D.2 CBR algorithm in case selection
CBR <- function(target, ds, w, vecS) {
  ##print(w);
  tempData <- cbind(ds[, ncol(ds)], rep(NA, nrow(ds)))
  #distance from all rows
  for (i in 1:nrow(ds)) {
    total = 0.0
    #distance from the ith row
    for (j in 1:length(vecS)) {
      if (vecColType[vecS[j]] == 1) {
        if (target[vecS[j]] != ds[i, vecS[j]]) {
          total = total + w[j] * 1
          
          #total=total+1;
        }
      } else{
        total = total + w[j] * (target[vecS[j]] - ds[i, vecS[j]]) ^ 2
        
        #total = total + (target[vecS[j]]-ds[i,vecS[j]])^2;
      }
    }
    tempData[i, 2] <- sqrt(total)
    
  }
  #print(target);
  #print(tempData);
  #The number of rows with minimum distances
  minimum = which.min(tempData[, 2])
  nMin = length(minimum)
  estimate = 0.0
  #print(target);
  for (i in 1:k) {
    minimum = which.min(tempData[, 2])
    
    ##print(tempData[minimum[1],1]);
    estimate = estimate + tempData[minimum[1], 1]
    
    #Set the distance to a much greater value
    tempData[minimum[1], 2] = 100
    
  }
  estimate = estimate / k
  
  #print(estimate);
  return(estimate)
}
#D.3 Evaluation function
#ds is the whole data set
#weight is the weighting vector for each feature
#cbr is the CBR algorithm
#returns a n*2 matrix, where the first column is actual effort and the second the estimated
Eval <- function(ds, weight, vecS) {
  #Keep the result
  evaluation = matrix(data = NA, nrow = nrow(ds), 2)
  #Evaluate
  for (i in 1:nrow(ds)) {
    evaluation[i, 1] = ds[i, ncol(ds)]
    evaluation[i, 2] <- CBR(ds[i, ], ds[-i, ], weight, vecS)
  }
  return(evaluation)
}
##D.4 Evaluate the method: Use TrainingSet to evaluate the TestingSet
EvalTesting <- function(TestingDataSet,
                        TrainingDataSet,
                        weight,
                        vecS) {
  #Keep the result
  evaluation = matrix(data = NA, nrow = nrow(TestingDataSet), 2)
  #Evaluate
  for (i in 1:nrow(TestingDataSet)) {
    evaluation[i, 1] = TestingDataSet[i, ncol(TestingDataSet)]
    evaluation[i, 2] <-
      CBR(TestingDataSet[i, ], TrainingDataSet, weight, vecS)
  }
  return(evaluation)
}
###############################################################################
######################### Part E: Evaluate metric #######################
###############################################################################
##************EvaluationMetrics Begins***********##
##E.1 MMRE function:Mean Magnitude Relative Error
MMREFunc <- function(evaluation, n) {
  re = abs(evaluation[, 2] - evaluation[, 1]) / evaluation[, 1]
  reFinite = is.finite(re)
  mmre = sum(re[reFinite]) / fre(reFinite)
  return(mmre)
  
}
fre <- function(x) {
  count = 0
  
  for (i in x) {
    if (i == T) {
      count = count + 1
    }
  }
  return(count)
}
##E.2 MdMRE function:Median Magnitude Relative Error
MdMREFunc <- function(evaluation, n) {
  MREVector <- vector(length = n)
  
  for (i in 1:n) {
    MREVector[i] = abs(evaluation[i, 2] - evaluation[i, 1]) / evaluation[i, 1]
    
  }
  return(median(MREVector))
  
}
##E.3 PRED function: Pred ( l ) is used as a complementary criterion to count the
##percentage of estimates that fall within less than l of the actual values
PREDFunc <- function(evaluation, n, l) {
  counter <- 0
  for (i in 1:n) {
    temp <- abs(evaluation[i, 2] - evaluation[i, 1]) / evaluation[i, 1]
    
    if (temp < l) {
      counter = counter + 1
      
    }
  }
  pred = counter / n
  
  return(pred)
  
}
#Invoke the main function
mainFunc(wd, fileName)
