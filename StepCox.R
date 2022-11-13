StepCox <- function(est_dd,val_dd_list,direction){
  fit <- step(coxph(Surv(OS.time,OS)~.,est_dd),direction = direction)
  
  RS = predict(fit, type = 'risk', newdata = val_dd_list)
  
  rid <- names(coef(fit))
  est_dd2 <- est_dd[,c('OS.time','OS',rid)]
  val_dd_list2 <- val_dd_list[,c('OS.time','OS',rid)]
  return(list(fit,RS,est_dd2,val_dd_list2))
}