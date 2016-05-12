#test priceDiff Spalten 
#preproData <- data
#priceDiff1 <- data$price1 - data$basePrice1 
#priceDiff2 <- data$price2 - data$basePrice2
#priceDiff3 <- data$price3 - data$basePrice3
#preproData$priceDiff1 <- priceDiff1
#preproData$priceDiff2 <- priceDiff2
#preproData$priceDiff3 <- priceDiff3
#priceDiff <- sort(union(union(priceDiff1, priceDiff2), priceDiff3))
#priceDiff <- matrix(c(priceDiff, rep(0,1274), rep(0,1274)), nrow = 3, ncol = 1274, byrow = TRUE)
#colnames(priceDiff) <- c(priceDiff[1,])
#rownames(priceDiff) <- c("priceDiff", "TRUE", "FALSE")

#Spalten: priceDiff with couponUsed, p,<2.2e-16
#priceDiff <- melted$basePrice - melted$price
#priceDiffTable <- t(table(priceDiff, melted$used))

#Spalten: priceRewardDiff with couponUsed, p < 2.2e-16
#priceRewardDiff <- melted$price - melted$reward
#priceRewardDiffTable <- t(table(priceRewardDiff, melted$used))

#Spalten: basepriceRewardDiff with couponUsed, p < 2.2e-16
#bpriceRewardDiff <- melted$basePrice - melted$reward
#bpriceRewardDiffTable <- t(table(bpriceRewardDiff, melted$used))

#Spalten: Tth in every monat, p = 4.286e-16
getOrderDay <- function(data){
  orderDay <- as.Date(data$orderTime)
  orderDay <- substr(as.character(orderDay), 9, 10)

  #orderDayTable <- t(table(orderDay, data$used))
  #orderDayDF <- as.data.frame(orderDayTable)
  #names(orderDayDF)[names(orderDayDF) == "Freq"] = "Count"
  #names(orderDayDF)[names(orderDayDF) == "Var1"] = "CouponUsed"
  #ggplot(orderDayDF, aes(orderDay, Count, fill = CouponUsed)) + geom_histogram(stat = "identity", position = "dodge", width = 1)
  #chisq.test(orderDayTable)

  data$orderDay <- orderDay
  data
}

#Spalten: day,received coupon  in every monat, p < 2.2e-16
getCouponReceivedPerMonth <- function(data){
  couponRecDay <- as.Date(melted$couponsReceived)
  couponRecDay <- substr(as.character(couponRecDay), 9, 10)
  couponRecDayTable <- t(table(couponRecDay, melted$used))
  couponRecDayDF <- as.data.frame(couponRecDayTable)
  names(couponRecDayDF)[names(couponRecDayDF) == "Freq"] = "Count"
  names(couponRecDayDF)[names(couponRecDayDF) == "Var1"] = "CouponUsed"
  ggplot(couponRecDayDF, aes(couponRecDay, Count, fill = CouponUsed)) + geom_histogram(stat = "identity", position = "dodge", width = 1)
  chisq.test(couponRecDayTable)
  data$couponRecDayOfMonth <- couponRecDay
  data
}

#Spalten:if reward >= median(reward), TRUE, else FALSE, p-value = 5.832e-08
getRewardGEMedianReward <- function(data){
  rewardTable <- t(table(as.numeric(melted$reward >= median(melted$reward)), melted$used))
  rewardDF <- as.data.frame(rewardTable)
  names(rewardDF)[names(rewardDF) == "Var1"] = "CouponUsed"
  names(rewardDF)[names(rewardDF) == "Var2"] = "reward"
  names(rewardDF)[names(rewardDF) == "Freq"] = "Count"
  ggplot(rewardDF, aes(reward, Count, fill = CouponUsed)) + geom_histogram(stat = "identity", position = "dodge", width = 1)
  chisq.test(rewardTable)
  data$rewardGEMedianReward <- as.numeric(melted$reward >= median(melted$reward))
  data
}
#Spalten:if reward >= mean(reward), TRUE, else FALSE, p-value = 3.652e-09
#rewardMeanTable <- t(table(as.numeric(melted$reward >= mean(melted$reward)), melted$used))
#rewardMeanDF <- as.data.frame(rewardMeanTable)
#names(rewardMeanDF)[names(rewardMeanDF) == "Var1"] = "CouponUsed"
#names(rewardMeanDF)[names(rewardMeanDF) == "Var2"] = "reward"
#names(rewardMeanDF)[names(rewardMeanDF) == "Freq"] = "Count"
#ggplot(rewardMeanDF, aes(reward, Count, fill = CouponUsed)) + geom_histogram(stat = "identity", position = "dodge", width = 1)

#Spalten: productGroupFreq
#productGroupFreqTable <- table(melted$productGroup, melted$used)
#productGroupFreqDF <- as.data.frame(productGroupFreqTable)
#tempTable <- table(melted$productGroup)
#tempDF <- as.data.frame(tempTable)
#productGroupFreqDF$Var1 <- tempDF$Freq
#names(productGroupFreqDF)[names(productGroupFreqDF) == "Var1"] = "prodGroupFreq"
#names(productGroupFreqDF)[names(productGroupFreqDF) == "Freq"] = "Count" 
#names(productGroupFreqDF)[names(productGroupFreqDF) == "Var2"] = "CouponUsed"
#productGroupFreqDF$prodGroupFreq <- as.numeric(productGroupFreqDF$prodGroupFreq)
#productGroupFreqDF <- productGroupFreqDF[order(productGroupFreqDF$prodGroupFreq),]
#ggplot(productGroupFreqDF, aes(prodGroupFreq, Count, fill = CouponUsed)) + geom_bar(stat = "identity", position = "dodge", width = 2)

#Spalten: OrderWeekday with couponUsed, p = 4.286e-16
#orderWeekDay <- weekdays(as.Date(melted$orderTime))
#orderWeekDayTable <- t(table(orderWeekday, melted$used))

#Spalten: couponRecWeekday with couponUsed, p < 2.2e-16
#couponRecWeekDay <- weekdays(as.Date(melted$couponsReceived))
#couponRecWeekDayTable <- t(table(couponRecWeekDay, melted$used))

#Spalten: categorieID with couponUsed
#allCategorieID <- union(dmc15SplitCategories(melted$categoryIDs)
#allcategorieID <- union(melted$categoryIDs, strsplit(as.character(melted$categoryIDs), ","))

#f <- 1

#while(f <- length(melted$categoryIDs)){
#  oneCategorie <- c(melted$categoryIDs[f])
#  allcategorieID <- union(union(oneCategorie[1], oneCategorie[2]), oneCategorie[3])
#  f <- f + 1
#} 

getDayAndReward <- function(data){
  data <- getOrderDay(data)
  #melted <- getCouponReceivedPerMonth(melted)
  data <- getRewardGEMedianReward(data)
}

getDayOfMonth <- function(data){
  data <- getOrderDay(data)
}