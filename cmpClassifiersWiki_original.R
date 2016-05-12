source("base.R")
source("utils.R")
#source("featureExtraction.R")
#source("melt.R")
#source("analyze.R")
#source("plotting.R")

loadAndInstallPackage("rpart")
loadAndInstallPackage("e1071")
loadAndInstallPackage("klaR")
loadAndInstallPackage("ada")
loadAndInstallPackage("nlme")

#error ftk given in the DMC Task
errFktComplete <- function(clas,ref){
  mean1Used = mean(ref$coupon1Used)
  mean2Used = mean(ref$coupon2Used)
  mean3Used = mean(ref$coupon3Used)
  meanBasVal = mean(ref$basketValue)

  diff1Used = abs(clas$coupon1Used - ref$coupon1Used)
  diff2Used = abs(clas$coupon2Used - ref$coupon2Used)
  diff3Used = abs(clas$coupon3Used - ref$coupon3Used)
  diffBasVal =  abs(clas$basketValue - ref$basketValue)


  errSum <- sum((diff1Used / mean1Used)**2 + (diff2Used / mean2Used)**2 + (diff3Used / mean3Used)**2 + (diffBasVal / meanBasVal)**2)

}

#Given error fkt just for one value
errFkt <- function(clas,ref){

  clas <- as.numeric(clas)
  ref <- as.numeric(ref)
  meanRef = mean(ref)
  diff = abs(clas - ref)
  errSum <- sum((diff / meanRef)**2)

}

#Mean error for each element (classification error = 1-accuracy )
errFktRelative <- function(clas,ref){

  clas <- as.numeric(clas)
  ref <- as.numeric(ref)
  diffSum = sum(abs(clas - ref))
  errSum <- (diffSum / length(clas))

}

#' Cross validate a given classifer on given data
#'
#' @param input: data to train the classifier on, output: classification results of each sample in the trainingset, trainFkt: function which trains and returns a given classifier, predictFkt: function which uses the trained classifer to predict new values
#' @return an vector containing the error value of each run
xVal <- function(input,output,trainFkt,predFkt, n=10){

  if(nrow(input) != nrow(output)){
    stop()
  }

  #shuffle sets
  permut = sample(nrow(input))
  input <- input[permut,]
  output <- output[permut,]

  size <- nrow(input) / n
  inBatchesStart <- c()
  inBatchesEnd <- c()
  outBatchesStart <- c()
  outBatchesEnd <- c()

  for(i in 0:(n-1)){# divide testset in n batches

    inBatchesStart <- c(inBatchesStart,(i*size+1))
    inBatchesEnd <- c(inBatchesEnd,min(nrow(input),((i+1)*size)))
    outBatchesStart <- c(outBatchesStart,(i*size+1))
    outBatchesEnd <- c(outBatchesEnd,min(nrow(output),((i+1)*size)))

  }
  runErr <- c()

  for(i in 1:n){ # X Validate

    trainSet <- input[-(inBatchesStart[i]:inBatchesEnd[i]),]
    testSet <- input[(outBatchesStart[i]:outBatchesEnd[i]),]
    testVals <- output[(outBatchesStart[i]:outBatchesEnd[i])]

    classifier <- trainFkt(trainSet)
    predictedVals <- predFkt(classifier,testSet)

    #calculate error value for a run (Error function interchange-able with other error functions )
    err <- errFkt(testVals,predictedVals)
    runErr <- c(runErr, err)

  }

  print(paste("Mean: " , mean(runErr)))
  print(paste("SD:   " , sd(runErr)))
  return(runErr)

}


### Regression ###
testAVGRegression <- function(trainingSet,n=10){

  trainFkt <- function(da){}
  predFkt <- function(classifier,data) mean(trainingSet$basketValue)
  input <- trainingSet
  output <- subset(trainingSet, select=c(basketValue))

  xVal(input,output,trainFkt,predFkt,n)

}

testAVGWeekDayRegression <- function(trainingSet,n=10){

  trainFkt <- function(da){}
  predFkt <- function(classifier,data){
    x <- c("Mo","Di","Mi","Do","Fr","Sa","So")
    out <- c()
    for(i in 1:nrow(data)){
      for (j in x){
        if(data[i,]$couponsReceivedWeekday == j){
          out <- c(out, mean(subset(trainingSet, trainingSet$couponsReceivedWeekday == j)$basketValue))
        }
      }
    }
    return(out)
  }
  input <- trainingSet
  output <- subset(trainingSet, select=c(basketValue))

  xVal(input,output,trainFkt,predFkt,n)
}


### Classification ###
testAVGClassification <- function(trainingSet,n=10){

  trainFkt <- function(da){}
  predFkt <- function(classifier,data) mean(trainingSet$coupon1Used)
  input <- trainingSet
  output <- subset(trainingSet, select=c(coupon1Used))

  xVal(input,output,trainFkt,predFkt,n)

}


### Regression with Classification ###
testVerySimpleRegressionWithClassification <- function(trainingSet,n=10){

  trainingSet <- subset(trainingSet , select=-c(categoryIDs1,categoryIDs2,categoryIDs3))

  #Divide data in y evenly distributed chunks/classes according to their basketValue
  y <- 10
  x<- quantile(trainingSet$basketValue , (1:y)/y)
  basketClass <- rep(c(-1) , times=nrow(trainingSet) )
  for(i in rev(1:length(x))){
    for(j in 1:nrow(trainingSet)){
      if(trainingSet[j,]$basketValue < x[i]) {
        basketClass[j] <- i
      }
    }
  }
  trainingSet$basketClass <- as.factor(basketClass)


  trainFkt <- function(da) svm(basketClass~.,da)

  predFkt <- function(classifier,data) {
    bv <- predict(classifier, newdata=data)
    predVal <- c()
    #transform basketClasses back to basketValues
    for(j in 1:(length(bv))){
      n <- as.numeric(bv[j])
      predVal <- c(predVal,(x[n]+x[n-1])/2)
    }
    predVal <- replace(predVal,is.na(predVal),mean(trainingSet$basketValue))
    return(predVal)
  }

  input <- trainingSet
  output <- subset(trainingSet, select=c(basketValue))

  xVal(input,output,trainFkt,predFkt,n)

}

#Lukas 
testSVMClassification <- function(trainingSet,n=10){
  
  trainFkt <- function(da) svm(coupon1Used~., da, type="C")
  predFkt <- function(classifier,data) predict(classifier, data)
  input <- trainingSet
  output <- subset(trainingSet, select=c(coupon1Used))
  
  xVal(input,output,trainFkt,predFkt,n)
  
}

  #trainingSetNoFeatures <- dmc15ReadTrainingSet()
  #trainingSetNoFeatures <- (subset(trainingSetNoFeatures,basketValue<3000 ))
  #trainingSet <- dmc15ExtractFeatures(trainingSetNoFeatures)
  #trainingSet$orderTime <- as.numeric(trainingSet$orderTime)
  #trainingSet$couponsReceived <- as.numeric(trainingSet$couponsReceived)

  #testAVGRegression(trainingSet,10)
  #testDecisionTreeRegression(trainingSet,10)
  #testVerySimpleRegressionWithClassification(trainingSet,10)

  #testAVGClassification(trainingSet,10)
  #testSVMClassification(trainingSet,10)


#  testGeneralizedLeastSquaresRegression(trainingSet,10)
#  testAdaBoostClassification(trainingSet,2)
