RSF <- function(est_dd,val_dd_list){
  fit <- rfsrc(Surv(OS.time,OS)~.,data = est_dd,
               ntree = 1000,nodesize = rf_nodesize,##该值建议多调整  
               splitrule = 'logrank',
               importance = T,
               proximity = T,
               forest = T,
               seed = seed)
  RS=predict(fit,newdata = val_dd_list)$predicted
  rid <- names(fit$importance[fit$importance>0])
  est_dd2 <- est_dd[,c('OS.time','OS',rid)]
  val_dd_list2 <- val_dd_list[,c('OS.time','OS',rid)]
  return(list(fit,RS,est_dd2,val_dd_list2))
}