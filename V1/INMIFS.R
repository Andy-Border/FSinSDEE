library("infotheo")
INMIFS <- function(dData,# data set
                 rFNum,# responce feature's column number
                 k = 5,# number of features selected from n features
                 MIMatrix = NULL) {
  # Example:
  # fSet<- INMIFS(dData = ISBSG, rFNum = responseVNum,k=10);print(colnames(dData)[fSet])
  
  ## INMIFS implementation
   # browser()
  isSelected <-
    c(rep(FALSE, ncol(dData))) # represent the variables in F
  # if this variable is selected, then the value is set to True
  # default is FALSE
  
  # Since the responce feature cannot be selected, we need to
  # remove the responce feature from F
  isSelected[rFNum] <- TRUE
  
  S <- NULL #the set of selected features
  inmifs <- NULL
  nmi <- NULL
  #Calculate the MImatrix, if it is not given
  entp <- getEntropy(dData)
  if (is.null(MIMatrix)) {
    MIMatrix <- getNMIMatrix(dData,entp)
  }
  #Choise of the first feature
  firstFeature <- which.max(MIMatrix[rFNum,!isSelected])
  isSelected[firstFeature] <- TRUE;
  S <- c(S,firstFeature)
  inmifs <- c(inmifs,MIMatrix[rFNum,firstFeature])
  nmi <- c(nmi,MIMatrix[rFNum,firstFeature])
  #Selection of k-1 features using greedy search
  for (i in 1:(k-1)) { # select one feature at a time for k times
    #Calculate tempINMIFS Matrix
    tempINMIFS <- c(rep(-1, ncol(dData))) 
    # tempINMIFS initialization -1 for default, the value will range 
    # from 0 to 1
    for (j in 1:ncol(dData)) {
      if (!isSelected[j]) { # calculate the tempINMIFS of remain features
        Be <- 1/(length(S))
        # Similar to MIFS
        # tempINMIFS = I(C;fi)- (Beta(sum(I(fi,fs)))) Beta=1/|S|
        ICFI <- MIMatrix[j, rFNum]
        sumIofOtherFeature <- sum(MIMatrix[j, isSelected])
        tempINMIFS[j] <- ICFI - Be * sumIofOtherFeature
      }
    }
    # select the max tempINMIFS feature
    maxtempINMIFS <- which.max(tempINMIFS)
    isSelected[maxtempINMIFS] <- TRUE;
    S <- c(S,maxtempINMIFS);
    nmi <- c(nmi,MIMatrix[rFNum,maxtempINMIFS])
    inmifs <- c(inmifs,tempINMIFS[maxtempINMIFS])
  }
  featureNames <- factor(colnames(dData)[S],levels=unique(colnames(dData)[S]))
  result <- data.frame(featureNames =featureNames,S=S,inmifs=inmifs,nmi=nmi)
  return(result)
}
# Calculate mutual information matrix between two columns
getNMIMatrix <- function(dData,entp) {
  # Allocate a new matrix to store MI results
  miMat <- matrix(nrow = ncol(dData), ncol = ncol(dData))
  # Get MI of every two cols (Independent-Independent & Independent-Response)
  for (i in 1:ncol(dData)) {
    for (j in 1:ncol(dData)) {
      ## ##discretize continuous data
      tempMI = mutinformation(discretize(dData[, i]), discretize(dData[, j]))
      #normalized
      miMat[i,j]=tempMI/(min(entp[i], entp[j]))
    }
  }
  return(miMat)
}
getEntropy <- function(dData) {
  entp <- vector(length = ncol(dData))
  for (i in 1:ncol(dData)) {
    ##discretize continuous data and calculate entropy
    entp[i] = entropy(discretize(dData[, i]))
    
  }
  return(entp)
}
