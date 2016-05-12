getFirstOrderFeatureTrain <- function(data){
  #create column: firstOrder
  userids <- list()
  number <- nrow(data)
  for (i in 1:number){
    if ( !(data$userID[i] %in%  userids)){
      userids <- data$userID[i]
      data$firstOrder[i] <- TRUE
    } else {
      data$firstOrder[i] <- FALSE
    }
  }
  data
}

dataMining <- function(train, melted){
  # Split in Subsets according firstOrder-column, Switch from train to melted dataset
  tableSecondOrder <- split(train, train$firstOrder) [1]
  tableFirstOrder <- split(train, train$firstOrder) [2]
  frameSecondOrder <- data.frame(subset(train, train$firstOrder==FALSE))
  frameFirstOrder <- data.frame(subset(train, train$firstOrder==TRUE))
  frameFirstOrderMelted <- data.frame(subset(melted, melted$orderID %in% frameFirstOrder$orderID))
  frameSecondOrderMelted <- data.frame(subset(melted, melted$orderID %in% frameSecondOrder$orderID))
  
  table(frameFirstOrderMelted$productGroup)
  
  #Datamining
  # AUSWERTUNG Productgroups with and without Coupon, First: All Orders, Then: Separate between first Orders and followed Orders
  melted <- within(melted, productGroup <- factor(productGroup, levels=names(sort(table(productGroup), decreasing=TRUE))))
  test <- ggplot(melted, aes(x=productGroup, fill = factor(used)))
  test + geom_bar(width=1)
  
  frameFirstOrderMelted <- within(frameFirstOrderMelted, productGroup <- factor(productGroup,levels=names(sort(table(productGroup), decreasing=TRUE))))
  test1 <- ggplot(frameFirstOrderMelted, aes(x=productGroup, fill = factor(used)))
  test1 + geom_bar(width=1)
  
  frameSecondOrderMelted <- within(frameSecondOrderMelted, productGroup <- factor(productGroup, levels=names(sort(table(productGroup), decreasing=TRUE))))
  test2 <- ggplot(frameSecondOrderMelted, aes(x=productGroup, fill = factor(used)))
  test2 + geom_bar(width=1)
  
  ks.test(frameSecondOrderMelted$productGroup, frameSecondOrderMelted$used*1)
  chisq.test(frameSecondOrderMelted$productGroup, frameSecondOrderMelted$used*1)
  ks.test(frameFirstOrderMelted$productGroup, frameFirstOrderMelted$used*1)
  chisq.test(frameFirstOrderMelted$productGroup, frameFirstOrderMelted$used*1)
  ks.test(frameSecondOrderMelted$productGroup, frameSecondOrderMelted$used*1)
  chisq.test(frameSecondOrderMelted$productGroup, frameSecondOrderMelted$used*1)
  
  #
  train <- within(train, userID <- factor(userID ))
  test3 <- ggplot(train, aes(x=userID))
  test3 + geom_bar(width=1)
  
  frameFirstOrderMelted <- within(frameFirstOrderMelted, userID <- factor(userID,levels=names(sort(table(userID), decreasing=TRUE))))
  test4 <- ggplot(frameFirstOrderMelted, aes(x=userID, fill = factor(used)))
  test4 + geom_bar(width=1)
  
  frameSecondOrderMelted <- within(frameSecondOrderMelted, userID <- factor(userID, levels=names(sort(table(userID), decreasing=TRUE))))
  test5 <- ggplot(frameSecondOrderMelted, aes(x=userID, fill = factor(used)))
  test5 + geom_bar(width=1)
}