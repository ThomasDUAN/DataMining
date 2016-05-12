
dmc15ReadSet = function(filename, withTarget = T){
  data = dmc15TypeColumns(dmc15LoadData(filename), withTargetColumns = withTarget)
  return(data)
}
dmc15ReadTrainingSet = function(filename="../dmc-2015/DMC_2015_orders_train.txt", withTarget = T) {
  dmc15ReadSet(filename,withTarget)
}
dmc15ReadClassSet = function(filename="../dmc-2015/DMC_2015_orders_class.txt", withTarget = F) {
  dmc15ReadSet(filename,withTarget)
}

#' Parse the given file into a data frame
#'
#' @param filename path to a csv file in the dmc15 format.
#' @return a data frame containing these data
#' @examples
#' dmc15LoadData("~/dmc15/dmc-2015/DMC_2015_orders_class.txt")
dmc15LoadData = function(filename){
  read.csv(filename, header=T, sep = "|")
}


#' Parse dates out of a string vector
#'
#' @param vector of strings (or any other type that the strptime function can handle)
#' @return an vector containing date objects
#' @examples
#' dmc15LoadData(dmc_raw_train_data$orderTime)
dmc15ConvertDates = function(dates){
  strptime(dates,format = "%F %H:%M:%S")
}

dmc15SplitCategories = function(catvector){
  return(c(strsplit(as.character(catvector),",")))
}
#dmc15ReducedCategories = function(data){
#  cols = c("categoryIDs1","categoryIDs2","categoryIDs3")
#  list_of_unioned_categories = sapply(cols,function(col){Reduce(union,data[,col])})
#}

#' Converts the column types of the parsed dmc csv file to R types.
#'
#' @param data frame containing the dmc15 data
#' @return data frame with dmc15 data and correct column types
#' @examples clean_class_data = dmc15ConvertRawDMCData(dmc15LoadData("~/dmc15/dmc-2015/DMC_2015_orders_class.txt"), withTargetColumns = F)
dmc15TypeColumns = function(dmc_raw_data, withTargetColumns = T){

  couponIDLevels = union(dmc_raw_data$couponID1,union(dmc_raw_data$couponID2,dmc_raw_data$couponID3))
  brandLevels = union(dmc_raw_data$brand1,union(dmc_raw_data$brand2,dmc_raw_data$brand3))

  res = data.frame(
    orderID = factor(dmc_raw_data$orderID),
    orderTime = dmc15ConvertDates(dmc_raw_data$orderTime),
    userID = factor(dmc_raw_data$userID),
    couponsReceived = dmc15ConvertDates(dmc_raw_data$couponsReceived),
    couponID1 = factor(dmc_raw_data$couponID1,levels = couponIDLevels),
    couponID2 = factor(dmc_raw_data$couponID2,levels = couponIDLevels),
    couponID3 = factor(dmc_raw_data$couponID3,levels = couponIDLevels),
    price1 = as.numeric(dmc_raw_data$price1),
    basePrice1 = as.numeric(dmc_raw_data$basePrice1),
    reward1 = as.numeric(dmc_raw_data$reward1),
    price2 = as.numeric(dmc_raw_data$price2),
    basePrice2 = as.numeric(dmc_raw_data$basePrice2),
    reward2 = as.numeric(dmc_raw_data$reward2),
    price3 = as.numeric(dmc_raw_data$price3),
    basePrice3 = as.numeric(dmc_raw_data$basePrice3),
    reward3 = as.numeric(dmc_raw_data$reward3),
    premiumProduct1 = as.logical(dmc_raw_data$premiumProduct1),
    premiumProduct2 = as.logical(dmc_raw_data$premiumProduct2),
    premiumProduct3 = as.logical(dmc_raw_data$premiumProduct3),
    brand1 = factor(dmc_raw_data$brand1, levels = brandLevels),
    brand2 = factor(dmc_raw_data$brand2, levels = brandLevels),
    brand3 = factor(dmc_raw_data$brand3, levels = brandLevels),
    productGroup1 = factor(dmc_raw_data$productGroup1),
    productGroup2 = factor(dmc_raw_data$productGroup2),
    productGroup3 = factor(dmc_raw_data$productGroup3),
    categoryIDs1 = I(dmc15SplitCategories(dmc_raw_data$categoryIDs1)),
    categoryIDs2 = I(dmc15SplitCategories(dmc_raw_data$categoryIDs2)),
    categoryIDs3 = I(dmc15SplitCategories(dmc_raw_data$categoryIDs3))
  )

  if(withTargetColumns == T){
    res = data.frame(res,
    coupon1Used = as.logical(dmc_raw_data$coupon1Used),
    coupon2Used = as.logical(dmc_raw_data$coupon2Used),
    coupon3Used = as.logical(dmc_raw_data$coupon3Used),
    basketValue = as.numeric(dmc_raw_data$basketValue))
  }
  return(res)
}
