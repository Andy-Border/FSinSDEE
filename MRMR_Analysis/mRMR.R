mRMR <- function(dData,# data set
                 rFNum,# responce feature's column number
                 k = 5,# number of features selected from n features
                 MIMatrix = NULL) {
  # Example:
  # fSet<- mRMR(dData = ISBSG, rFNum = responseVNum,k=10);print(colnames(dData)[fSet])
  
  #browser()
  ## mRMR implementation
  # browser()
  isSelected <-
    c(rep(FALSE, ncol(dData))) # represent the variables in F
  # if this variable is selected, then the value is set to True
  # default is FALSE
  
  # Since the responce feature cannot be selected, we need to
  # remove the responce feature from F
  isSelected[rFNum] <- TRUE
  
  S <- NULL #the set of selected features
  mrmr <- NULL
  vmi <- NULL
  #Calculate the MImatrix, if it is not given
  if (is.null(MIMatrix)) {
    MIMatrix <- getMiMatrix(dData)
  }
  
  #Choise of the first feature
  firstFeature <- which.max(MIMatrix[rFNum,!isSelected])
  isSelected[firstFeature] <- TRUE;
  S <- c(S,firstFeature)
  mrmr <- c(mrmr,MIMatrix[rFNum,firstFeature])
  vmi <- c(vmi,MIMatrix[rFNum,firstFeature])
  #Selection of k-1 features using greedy search
  for (i in 1:(k-1)) { # select one feature at a time for k times
    #Calculate mRMR Matrix
    mRMR <- c(rep(-1, ncol(dData))) 
    # mRMR initialization -1 for default, the value will range 
    # from 0 to 1
    for (j in 1:ncol(dData)) {
      if (!isSelected[j]) { # calculate the mRMR of remain features
        Be <- 1/(length(S))
        # Similar to MIFS
        # mRMR = I(C;fi)- (Beta(sum(I(fi,fs)))) Beta=1/|S|
        ICFI <- MIMatrix[j, rFNum]
        sumIofOtherFeature <- sum(MIMatrix[j, isSelected])
        mRMR[j] <- ICFI - Be * sumIofOtherFeature
      }
    }
    # select the max mRMR feature
    maxmRMR <- which.max(mRMR)
    isSelected[maxmRMR] <- TRUE;
    S <- c(S,maxmRMR);
    vmi <- c(vmi,MIMatrix[rFNum,maxmRMR])
    mrmr <- c(mrmr,mRMR[maxmRMR])
  }
  featureNames <- factor(colnames(dData)[S],levels=unique(colnames(dData)[S]))
  result <- data.frame(featureNames =featureNames,S=S,mrmr=mrmr,vmi=vmi)
  return(result)
}
# Calculate mutual information matrix between two columns
getMiMatrix <- function(dData) {
  # Allocate a new matrix to store MI results
  miMat <- matrix(nrow = ncol(dData), ncol = ncol(dData))
  # Get MI of every two cols (Independent-Independent & Independent-Response)
  for (i in 1:ncol(dData)) {
    for (j in 1:ncol(dData)) {
      ## ##discretize continuous data
      miMat[i, j] = mutinformation(discretize(dData[, i]), discretize(dData[, j]))
    }
  }
  return(miMat)
}

