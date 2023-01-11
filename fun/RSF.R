RSF <- function(est_dd,val_dd_list){
  fit <- rfsrc(Surv(OS.time,OS)~.,data = est_dd,
               ntree = 1000,nodesize = rf_nodesize,##该值建议多调整  
               splitrule = 'logrank',
               importance = T,
               proximity = T,
               forest = T,
               seed = seed)
  
  train_cindex=cindex(est_dd,fit$predicted)
  
  test_RS=predict(fit,newdata = val_dd_list)$predicted
  test_cindex=cindex(val_dd_list,test_RS)
  
  rid <- names(fit$importance[fit$importance>0])
  est_dd2 <- est_dd[,c('OS.time','OS',rid)]
  val_dd_list2 <- val_dd_list[,c('OS.time','OS',rid)]
  return(list(fit,est_dd2,val_dd_list2,train_cindex,test_cindex))
  
}