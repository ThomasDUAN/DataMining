library(ggplot2)

getPriceDiffAndFrequency <- function(melted){
  # price difference
  priceDiff <- melted$basePrice - melted$price
  table1 <- table(melted$used, priceDiff)
  frame1 <- as.data.frame(table1)
  ggplot(frame1[c(1:1800),], aes(priceDiff, Freq, fill=Var1)) + geom_bar(position="dodge",stat="identity")
  
  # perchase frequency 
  userIDCounts <- count(melted$userID)
  # new column purchaseFreq
  purchaseFreq <- NULL
  for (i in 1:length(melted$userID) ) {
    purchaseFreq[i] <- userIDCounts$freq[which(userIDCounts$x == melted$userID[i])]
  }
  
  #table3 <- table(melted$used, purchaseFreq)
  #frame3 <- as.data.frame(table3)
  #ggplot(frame3, aes(purchaseFreq, Freq, fill=Var1)) + geom_bar(position="dodge",stat="identity")
  
  # Add column
  melted$priceDiff <- priceDiff
  melted$purchaseFreqOfUser <- purchaseFreq
  melted
}

getPriceDiffTrain <- function(data){
  # price difference
  priceDiff1 <- data$basePrice1 - data$price1
  priceDiff2 <- data$basePrice2 - data$price2
  priceDiff3 <- data$basePrice3 - data$price3
  
  # perchase frequency 
  userIDCounts <- count(data$userID)
  # new column purchaseFreq
  purchaseFreq <- NULL
  for (i in 1:length(train$userID) ) {
    purchaseFreq[i] <- userIDCounts$freq[which(userIDCounts$x == data$userID[i])]
  }
  
  # Add column
  data$purchaseFreqOfUser <- purchaseFreq
  data$priceDiff1 <- priceDiff1
  data$priceDiff2 <- priceDiff2
  data$priceDiff3 <- priceDiff3
  data
}