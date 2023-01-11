# 只能放后面
mySurvivalsvm <- function(est_dd,val_dd_list){
  fit = survivalsvm(Surv(OS.time,OS)~., data= est_dd, gamma.mu = 1)
  
  train_RS = as.numeric(predict(fit, est_dd)$predicted)
  train_cindex=cindex(est_dd,train_RS)
  
  test_RS=as.numeric(predict(fit, val_dd_list)$predicted)
  test_cindex=cindex(val_dd_list,test_RS)
  
  return(list(fit,train_cindex,test_cindex))
  }