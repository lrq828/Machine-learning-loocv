StepCox <- function(est_dd,val_dd_list,direction){
  fit <- step(coxph(Surv(OS.time,OS)~.,est_dd),direction = direction)
  
  train_RS = predict(fit, type = 'risk', newdata = est_dd)
  train_cindex=cindex(est_dd,train_RS)
  
  test_RS = predict(fit, type = 'risk', newdata = val_dd_list)
  test_cindex=cindex(val_dd_list,test_RS)
  
  rid <- names(coef(fit))
  est_dd2 <- est_dd[,c('OS.time','OS',rid)]
  val_dd_list2 <- val_dd_list[,c('OS.time','OS',rid)]
  return(list(fit,est_dd2,val_dd_list2,train_cindex,test_cindex))
}