#############Outliers with regard to the basketvalue
# R is a reference value and for our coupon data set we get 4 outliers for r=50
hboutlier <- function(x,r=50){
  x <- x[is.finite(x)]
  stopifnot(
    length(x) > 0
    , all(x>0)
  )
  xref <- median(x)
  if (xref <= sqrt(.Machine$double.eps))
    warning("Reference value close to zero: results may be inaccurate")
  pmax(x/xref, xref/x) > r
}

classifyAsOutlier <- function(data, r=50) {
  cbind(data, isOutlier=factor(hboutlier(data$basketValue, r)))
}

createOutlierFeature <- function(train){
  boxplot.stats(train$basketValue)$out
  plot(train$basketValue)
  outliers <- train$basketValue[hboutlier(train$basketValue)]
  classifyAsOutlier(train)
}

############ Generate Columns for two most frequent categoryIDs
getMostFrequentCatID <- function(catList, catFrequencies){
  mostfreqID <- ""
  freq <- 0
  if (length(catList) > 1){
    for( cat in catList){
      if (catFrequencies[cat] > freq){
        mostfreqID <- cat
        freq <- catFrequencies[cat]
      }
    }
  } else if (length(catList) == 1){
    mostfreqID <- catList
  }
  mostfreqID
}

getSecondMostFrequentCatID <- function(catList, catFrequencies){
  mostfreqID <- ""
  mostfreq <- 0
  secondMostfreqID <- ""
  secondMostfreq <- 0
  if (length(catList) >= 2){
    for( cat in catList){
      if (catFrequencies[cat] > mostfreq){
        secondMostfreqID <- mostfreqID
        secondMostfreq <- mostfreq
        mostfreqID <- cat
        mostfreq <- catFrequencies[cat]
        
      } else if(catFrequencies[cat] < mostfreq &&  catFrequencies[cat] >=  secondMostfreq){
        secondMostfreqID <- cat
        secondMostfreq <- catFrequencies[cat]
      }
    }
  }
  secondMostfreqID
}

getMostFrequentPairID <- function(catList, catFrequencies){
  first <- getMostFrequentCatID(catList, catFrequencies)
  second <- getSecondMostFrequentCatID(catList, catFrequencies)
  string <- ""
  if (!is.null(second) && second != ""){
    string <- paste (first, second, sep = ":")
  }
  string
}

###############Analyze category column
createCategoryIDFeatures <- function(melted){
  numOfCat <- do.call(rbind,lapply(melted$categoryIDs, length))
  plot(do.call(rbind,lapply(melted$categoryIDs, length)))
  summary(numOfCat)
  #chisq.test(table(numOfCat, melted$basketValue))
  #chisq.test(table(numOfCat, melted$used))
  
  # Create new category columns
  # Column for the count of category IDs
  preMelted <- cbind(melted, numOfCategoryIDs=do.call(rbind,lapply(melted$categoryIDs, length)))
  
  completeList <- unlist(melted$categoryIDs, recursive = TRUE)
  uniqueCategories <- unique(completeList)
  catFrequencies <- sort(decreasing = TRUE, table(factor(ordered=FALSE, unlist(completeList))))
  plot(catFrequencies)
  
  preMelted <- cbind(preMelted, mostFrequentCatID=do.call(rbind, lapply(melted$categoryIDs, getMostFrequentCatID, catFrequencies=catFrequencies)))
  preMelted <- cbind(preMelted, secondMostFrequentCatID=do.call(rbind, lapply(melted$categoryIDs, getSecondMostFrequentCatID, catFrequencies=catFrequencies)))
  preMelted <- cbind(preMelted, MostFrequentPairID=do.call(rbind, lapply(melted$categoryIDs, getMostFrequentPairID, catFrequencies=catFrequencies)))
  
  # Most frequent
  tableMostUsed <- table(preMelted$used, preMelted$mostFrequentCatID)
  chisq.test(tableMostUsed)
  barplot(tableMostUsed)
  
  tableMostBasket <- table(preMelted$basketValue, preMelted$mostFrequentCatID)
  chisq.test(tableMostBasket)
  
  #Second Most frequent
  tableSMostUsed <- table(preMelted$used, preMelted$secondMostFrequentCatID)
  chisq.test(tableSMostUsed)
  barplot(tableSMostUsed)
  
  tableSMostBasket <- table(preMelted$basketValue, preMelted$secondMostFrequentCatID)
  chisq.test(tableSMostBasket)
  
  # Pairs
  tableMostPairs <- sort(table(preMelted$MostFrequentPairID))
  barplot(tableMostPairs)
  
  tableMostPairUsed <- table(preMelted$used, preMelted$MostFrequentPairID)
  chisq.test(tableMostPairUsed)
  barplot(tableMostPairUsed)
  preMelted
}

createCategoryIDFeaturesForTrain <- function(train){
  # Number of categoryIDs
  train <- cbind(train, numOfCategoryIDs1=do.call(rbind,lapply(train$categoryIDs1, length)))
  train <- cbind(train, numOfCategoryIDs2=do.call(rbind,lapply(train$categoryIDs2, length)))
  train <- cbind(train, numOfCategoryIDs3=do.call(rbind,lapply(train$categoryIDs3, length)))
  
  # CategoryIDs Frequencies per Coupon
  completeCatList1 <- unlist(train$categoryIDs1, recursive = TRUE)
  completeCatList2 <- unlist(train$categoryIDs2, recursive = TRUE)
  completeCatList3 <- unlist(train$categoryIDs3, recursive = TRUE)
  uniqueCategories1 <- unique(completeCatList1)
  uniqueCategories2 <- unique(completeCatList2)
  uniqueCategories3 <- unique(completeCatList3)
  plot(c(length(uniqueCategories1),length(uniqueCategories2),length(uniqueCategories3)))
  
  frameCatListFreq1 <- data.frame(table(factor(ordered=TRUE, unlist(completeCatList1))))
  frameCatListFreq2 <- data.frame(table(factor(ordered=TRUE, unlist(completeCatList2))))
  frameCatListFreq3 <- data.frame(table(factor(ordered=TRUE, unlist(completeCatList3))))
  frameCatFrequencies <- merge(frameCatListFreq1, merge(frameCatListFreq2, frameCatListFreq3, by="Var1", all.x=TRUE, all.y=TRUE, suffixes=c("Freq_Cat2", "Freq_Cat3")), by="Var1", all.x=TRUE, all.y=TRUE, sort=1)
  colnames(frameCatFrequencies) <- c("CategoryID", "Cat1_Freq", "Cat2_Freq", "Cat3_Freq")
  frameCatFrequencies[c("CategoryID", "Cat1_Freq", "Cat2_Freq", "Cat3_Freq")][is.na(frameCatFrequencies[c("CategoryID", "Cat1_Freq", "Cat2_Freq", "Cat3_Freq")])] <- 0
  meltedFrameCatFrequencies <- melt(frameCatFrequencies, id.vars="CategoryID")
  ggplot(data=meltedFrameCatFrequencies, aes(x=CategoryID, y=value, group=variable, color=variable)) + geom_line() + xlab("CatIDs") + ylab("Frequencies") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  catFrequencies1 <- sort(decreasing = TRUE, table(factor(ordered=TRUE, unlist(completeCatList1))))
  catFrequencies2 <- sort(decreasing = TRUE, table(factor(ordered=TRUE, unlist(completeCatList2))))
  catFrequencies3 <- sort(decreasing = TRUE, table(factor(ordered=TRUE, unlist(completeCatList3))))
  plot(catFrequencies1)
  plot(catFrequencies2)
  plot(catFrequencies3)
  
  #Bind frequent catIDs to columnse
  #Cat1
  train <- cbind(train, mostFrequentCatID1=do.call(rbind, lapply(train$categoryIDs1, getMostFrequentCatID, catFrequencies=catFrequencies1)))
  train <- cbind(train, secondMostFrequentCatID1=do.call(rbind, lapply(train$categoryIDs1, getSecondMostFrequentCatID, catFrequencies=catFrequencies1)))
  train <- cbind(train, MostFrequentPairID1=do.call(rbind, lapply(train$categoryIDs1, getMostFrequentPairID, catFrequencies=catFrequencies1)))
  #Cat2
  train <- cbind(train, mostFrequentCatID2=do.call(rbind, lapply(train$categoryIDs2, getMostFrequentCatID, catFrequencies=catFrequencies2)))
  train <- cbind(train, secondMostFrequentCatID2=do.call(rbind, lapply(train$categoryIDs2, getSecondMostFrequentCatID, catFrequencies=catFrequencies2)))
  train <- cbind(train, MostFrequentPairID2=do.call(rbind, lapply(train$categoryIDs2, getMostFrequentPairID, catFrequencies=catFrequencies2)))
  #Cat3
  train <- cbind(train, mostFrequentCatID3=do.call(rbind, lapply(train$categoryIDs3, getMostFrequentCatID, catFrequencies=catFrequencies3)))
  train <- cbind(train, secondMostFrequentCatID3=do.call(rbind, lapply(train$categoryIDs3, getSecondMostFrequentCatID, catFrequencies=catFrequencies3)))
  train <- cbind(train, MostFrequentPairID3=do.call(rbind, lapply(train$categoryIDs3, getMostFrequentPairID, catFrequencies=catFrequencies3)))
  
  #Plots for Most Used
  tableMostUsed1 <- table(train$coupon1Used, train$mostFrequentCatID1)
  frameTableMostUsed1 <- data.frame(tableMostUsed1)
  ggplot(frameTableMostUsed1, aes(x=Var2, y=Freq, group=Var1, color=Var1)) + geom_bar(stat="identity") + xlab("CatIDs1") + ylab("Frequencies") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("MostUsedCatIDs_Coupon1")
  
  tableMostUsed2 <- table(train$coupon2Used, train$mostFrequentCatID2)
  frameTableMostUsed2 <- data.frame(tableMostUsed2)
  ggplot(frameTableMostUsed2, aes(x=Var2, y=Freq, group=Var1, color=Var1)) + geom_bar(stat="identity") + xlab("CatIDs2") + ylab("Frequencies") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("MostUsedCatIDs_Coupon2")
  
  tableMostUsed3 <- table(train$coupon3Used, train$mostFrequentCatID3)
  frameTableMostUsed3 <- data.frame(tableMostUsed3)
  ggplot(frameTableMostUsed3, aes(x=Var2, y=Freq, group=Var1, color=Var1)) + geom_bar(stat="identity") + xlab("CatIDs3") + ylab("Frequencies") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("MostUsedCatIDs_Coupon3")
  
  #Plots SecondMost Used
  tableSecondUsed1 <- table(train$coupon1Used, train$secondMostFrequentCatID1)
  frameTableSecondUsed1 <- data.frame(tableSecondUsed1)
  ggplot(frameTableSecondUsed1, aes(x=Var2, y=Freq, group=Var1, color=Var1)) + geom_bar(stat="identity") + xlab("CatIDs") + ylab("Frequencies") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("SecondMostUsedCatIDs_Coupon1")
  
  tableSecondUsed2 <- table(train$coupon2Used, train$secondMostFrequentCatID2)
  frameTableSecondUsed2 <- data.frame(tableSecondUsed2)
  ggplot(frameTableSecondUsed2, aes(x=Var2, y=Freq, group=Var1, color=Var1)) + geom_bar(stat="identity") + xlab("CatIDs") + ylab("Frequencies") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("SecondMostUsedCatIDs_Coupon1")
  
  tableSecondUsed3 <- table(train$coupon3Used, train$secondMostFrequentCatID3)
  frameTableSecondUsed3 <- data.frame(tableSecondUsed3)
  ggplot(frameTableSecondUsed3, aes(x=Var2, y=Freq, group=Var1, color=Var1)) + geom_bar(stat="identity") + xlab("CatIDs") + ylab("Frequencies") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("SecondMostUsedCatIDs_Coupon1")
  
  train
}

############ ProductGroup
getFrequency <- function(listItem, tableProductGroups){
  tableProductGroups[listItem] / length(melted$productGroup)
}

getProductGroupFeatures <- function(melted){
  uniqueProductGroups <- unique(melted$productGroup)
  tableProductGroups <- sort(decreasing = TRUE, table(melted$productGroup))
  plot(tableProductGroups)
  
  preMelted <- cbind(melted, freqProductGroup=do.call(rbind,lapply(melted$productGroup, getFrequency, tableProductGroups=tableProductGroups)))
  names(preMelted)[[ncol(preMelted)]] <- "freqOfProductGroup"
  #colnames(preMelted)
  chisq.test(table(preMelted$freq, preMelted$basketValue))
  ks.test(preMelted$freq, preMelted$basketValue)
  chisq.test(table(preMelted$freq, preMelted$used))
  ks.test(preMelted$freq, preMelted$used*1)
  preMelted
}

getTimeBetweenReceivedAndOrdered <- function(data){
  data$timeInSecondsBetweenReceivedAndOrdered <- data$orderTime - data$couponsReceived
  data
}

getDayOfWeek <- function(data){
  data$weekDayOfOrderTime <- as.factor(weekdays(as.Date(data$orderTime)))
  data$weekDayOfReceivedTime <- as.factor(weekdays(as.Date(data$couponsReceived)))
  data
}

getCatIDFromListByIndex <- function(catList, index){
  if (index <= length(catList)){
    catList[index]
  } else { 
    ""
  }
}

getAllCatIDsInSingleColumn <- function(data, coupon){
  if (coupon == 1){
    for(index in 1:5){
      columnName <- paste("Coupon",coupon,"_", "Cat", index, sep = "")
      data <- cbind(data, do.call(rbind, lapply(train$categoryIDs1, getCatIDFromListByIndex, index=index)))
      colnames(data)[ncol(data)] <- columnName
    }
  } else if (coupon == 2){
    for(index in 1:5){
      columnName <- paste("Coupon",coupon,"_", "Cat", index, sep = "")
      data <- cbind(data, do.call(rbind, lapply(train$categoryIDs2, getCatIDFromListByIndex, index=index)))
      colnames(data)[ncol(data)] <- columnName
    }
  } else if (coupon == 3){
    for(index in 1:5){
      columnName <- paste("Coupon",coupon,"_", "Cat", index, sep = "")
      data <- cbind(data, do.call(rbind, lapply(train$categoryIDs3, getCatIDFromListByIndex, index=index)))
      colnames(data)[ncol(data)] <- columnName
    }
  }
  data
}


#############################Return proprocessed data set
getPreprocessedMelted <- function(melted){
  melted <- createCategoryIDFeatures(melted)
  melted <- getProductGroupFeatures(melted)
  melted
}
getPreprocessedTrain <- function(train){
  train <- createOutlierFeature(train)
  train <- createCategoryIDFeaturesForTrain(train)
  train
}