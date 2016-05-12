#require package plyr
#loadAndInstallPackage("plyr")
dmc15MeltSingleVoucher = function(voucherNumber,data){
  #coupon specific columnNames
  colNames = c("couponID","price","basePrice","reward","premiumProduct","brand","productGroup","categoryIDs")
  #get static columns (columns independent of coupon)
  df_for_single_voucher = subset(data,select = c(orderID,orderTime,userID,couponsReceived, basketValue))

  #map colname to data.frames with one column
  cols = llply(.data = colNames,.inform = T  ,.fun = function(name){
    colName = paste(name,voucherNumber,sep = "")#compute colName in data as string
    #data.frame(data[,colName])#create a data frame only containing this column
    data.frame(data[[colName]])#create a data frame only containing this column

  })
  #combine all one-column data frames to a multi-column data frame
  res = Reduce(cbind,cols)
  #fix column names
  colnames(res) = colNames
  #add column voucherNumber
  res = cbind(df_for_single_voucher,res,voucherNumber=voucherNumber)
  #compute and add used column
  usedColName = paste("coupon",voucherNumber,"Used",sep = "")
  res = cbind(res, used= data[[usedColName]])#add column used with computed values
  return(res)
}

dmc15MeltData = function(data){
  #map list(1,2,3) to a list containing the columns for voucher 1,voucher 2, voucher 3 in data frames.
  voucherDataFrames = llply(.data = list(1,2,3),.fun = function(voucherNumber){

    dmc15MeltSingleVoucher(voucherNumber,data)
  })
  #combine this three data frame rowwise
  return(Reduce(rbind,voucherDataFrames))

}
