data <- read.csv(file="DMC_2015_orders_train.txt", sep="|");

# Coupon and CouponCombinations - distribution
rows <- nrow(data)
distributionCouponUsed <- NULL

noCoupons <- sum(!data$coupon1Used & !data$coupon2Used & !data$coupon3Used)
C1 <- sum(data$coupon1Used)
C2 <- sum(data$coupon2Used)
C3 <- sum(data$coupon3Used)
C1only <- sum(data$coupon1Used & !data$coupon2Used & !data$coupon3Used)
C2only <- sum(!data$coupon1Used & data$coupon2Used & !data$coupon3Used)
C3only <- sum(!data$coupon1Used & !data$coupon2Used & data$coupon3Used)
C1C2 <- sum(data$coupon1Used & data$coupon2Used & !data$coupon3Used)
C1C3 <- sum(data$coupon1Used & !data$coupon2Used & data$coupon3Used)
C2C3 <- sum(!data$coupon1Used & data$coupon2Used & data$coupon3Used)
C1C2C3 <- sum(data$coupon1Used & data$coupon2Used & data$coupon3Used)

name <- c("noCoupons","C1","C2","C3","C1only", "C2only", "C3only", "C1C2", "C1C3", "C2C3", "C1C2C3")
absolute <- c(noCoupons,C1,C2,C3,C1only,C2only,C3only,C1C2,C1C3,C2C3,C1C2C3)
percent <- absolute/rows*100

dis <- data.frame(name, absolute,percent)

# Couponusage for premium products
nC1 <- sum(data$coupon1Used)
nC2 <- sum(data$coupon2Used)
nC3 <- sum(data$coupon3Used)

pPC1 <- sum(data$coupon1Used & data$premiumProduct1)
pPC2 <- sum(data$coupon2Used & data$premiumProduct2)
pPC3 <- sum(data$coupon3Used & data$premiumProduct3)

name2 <- c("Total","UsedForPremiumProduct (absolute)","UsedForPremiumProduct (percent %)")
C1 <- c(nC1, pPC1, pPC1/nC1*100)
C2 <- c(nC2, pPC2, pPC2/nC2*100)
C3 <- c(nC3, pPC3, pPC3/nC3*100)

disPremiumProducts <- data.frame(name2, C1, C2, C3)